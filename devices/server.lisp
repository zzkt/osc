(cl:in-package #:osc)

(defun make-osc-server (&key (protocol :udp) debug-mode
                          (buffer-size *default-osc-buffer-size*)
                          cleanup-fun)
  (ecase protocol
    (:udp (make-instance 'osc-server-udp
                         :debug-mode debug-mode
                         :cleanup-fun cleanup-fun
                         :buffer-size buffer-size
                         :socket-buffer (make-socket-buffer buffer-size)))
    (:tcp (make-instance 'osc-server-tcp
                         :debug-mode debug-mode
                         :cleanup-fun cleanup-fun
                         :buffer-size buffer-size))))

(defgeneric boot (osc-server port))

(defmethod boot :around ((server osc-server) port)
  (if (device-active-p server)
      (warn "~%Server ~A already running" (machine-instance)))
  (set-socket (make-socket (protocol server)) server)
  (socket-bind (socket server) #(0 0 0 0) port)
  (call-next-method)
  (format t "~%Server ~A listening on port ~A~%"
          (machine-instance) port))

(defmethod boot ((server osc-server-udp) port)
  (declare (ignore port))
  "UDP server sockets are used for receiving and unconnected sending."
  (set-listening-thread (make-listening-thread server) server))

(defmethod boot ((server osc-server-tcp) port)
  (declare (ignore port))
  (set-listening-thread
   (sb-thread:make-thread
    (lambda ()
      (unwind-protect
           (progn (socket-listen (socket server) 10)
                  (loop for socket = (socket-accept (socket server))
                     for endpoint = (make-osc-client-endpoint-tcp
                                     socket
                                     (debug-mode server)
                                     (buffer-size server)
                                     (address-tree server)
                                     (clients server)
                                     (make-unregister-self-fun server))
                     do (register-tcp-client server endpoint)))
        (osc-device-cleanup server)))
    :name (format nil "osc-server-tcp: ~A" (name server)))
   server)
  server)

(defmethod osc-device-cleanup ((device osc-server-udp))
  (loop for client-name being the hash-key in (clients device)
     using (hash-value addr+port)
     do (notify-quit device client-name)
     do (unregister-udp-client device
                               (first addr+port)
                               (second addr+port)))
  (call-next-method))

(defmethod osc-device-cleanup ((device osc-server-tcp))
  (loop for client being the hash-value in (clients device)
     do (quit client))
  (call-next-method))

(defun make-clients-hash ()
  (make-hash-table :test 'equal))


;;;=====================================================================
;;; UDP server functions
;;;=====================================================================

(defmethod initialize-instance :after ((server osc-server-udp) &key)
  (make-server-responders server))

(defgeneric make-server-responders (server))

(defmethod make-server-responders ((server osc-server-udp))
  (add-osc-responder server "/cl-osc/register"
      (cmd args device address port timetag)
    (let ((listening-port (car args))) ; Optional port for sending
                                        ; return messages to.
      (register-udp-client device address
                           (if listening-port listening-port port))))
  (add-osc-responder server "/cl-osc/quit"
      (cmd args device address port timetag)
    (unregister-udp-client device address port)))

(defun register-udp-client (server addr port)
  (let ((client-name (make-addr+port-string addr port)))
    (format t "Client registered: ~A~%" client-name)
    (setf (gethash client-name (clients server))
          (list addr port))
    (post-register-hook server client-name)))

(defun unregister-udp-client (server addr port)
  (let ((client-name (make-addr+port-string addr port)))
    (format t "Client quit: ~A~%" client-name)
    (remhash client-name (clients server))))

(defgeneric post-register-hook (server client-name)
  (:method ((server osc-server-udp) client-name)
    (format t "Post-register hook for client: ~A~%" client-name)
    (notify-registered server client-name)))

(defun notify-registered (server client-name)
  (send-to-client server client-name "/cl-osc/server/registered"))

(defun notify-quit (server client-name)
  (send-to-client server client-name "/cl-osc/server/quit"))


;;;=====================================================================
;;; TCP server functions
;;;=====================================================================

(defun register-tcp-client (server transmitter)
  (setf (gethash (make-peername-string transmitter)
		 (clients server))
	transmitter))

(defun unregister-tcp-client (server transmitter)
  (remhash (make-peername-string transmitter)
	   (clients server)))

(defun make-unregister-self-fun (server)
  #'(lambda (client)
      (unregister-tcp-client server client)))

(defun get-tcp-client (server socket-peername)
  (gethash socket-peername (clients server)))

(defgeneric print-clients (server))

(defmethod print-clients ((server osc-server-udp))
  (loop for addr+port being the hash-value in (clients server)
     for i from 1
     do (format t "~A. Connected to: ~A~%" i (make-addr+port-string
                                              (first addr+port)
                                              (second addr+port)))))

(defmethod print-clients ((server osc-server-tcp))
  (loop for endpoint being the hash-value in (clients server)
     for i from 1
     do (format t "~A. Connected to: ~A~%" i (make-addr+port-string
                                              (peer-address endpoint)
                                              (peer-port endpoint)))))

;;;=====================================================================
;;; Server sending functions
;;;=====================================================================

(defgeneric send-to-client (server client-name &rest msg)
  (:method :around ((server osc-server) client-name &rest msg)
           (let ((client (gethash client-name (clients server))))
             (if client
                 (apply #'call-next-method server client msg)
                 (warn "No client called ~A~%" client-name)))))

(defmethod send-to-client ((server osc-server-udp) client-name &rest
                                                                 msg)
  (apply #'send-to server (first client-name) (second client-name)
         msg))

(defmethod send-to-client ((server osc-server-tcp) client &rest msg)
  (apply #'send client msg))

(defgeneric send-bundle-to-client (server client-name timetag &rest
                                                                msg)
  (:method :around ((server osc-server) client-name timetag &rest msg)
           (let ((client (gethash client-name (clients server))))
             (if client
                 (apply #'call-next-method server client timetag msg)
                 (warn "No client called ~A~%" client-name)))))

(defmethod send-bundle-to-client ((server osc-server-udp) client-name
                                  timetag &rest msg)
  (apply #'send-bundle-to server (first client-name)
         (second client-name) timetag msg))
