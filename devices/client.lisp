(cl:in-package #:osc)

(defun make-osc-client (&key(protocol :udp) debug-mode
                        (buffer-size *default-osc-buffer-size*)
                        address-tree cleanup-fun)
  (ecase protocol
    (:udp (make-instance 'osc-client-udp
                         :debug-mode debug-mode
                         :socket-buffer (make-socket-buffer
                                         buffer-size)
                         :address-tree (if address-tree
                                           address-tree
                                           (make-osc-tree))
                         :cleanup-fun cleanup-fun))
    (:tcp (make-instance 'osc-client-tcp
                         :debug-mode debug-mode
                         :socket-buffer (make-socket-buffer
                                         buffer-size)
                         :address-tree (if address-tree
                                           address-tree
                                           (make-osc-tree))
                         :cleanup-fun cleanup-fun))))

(defmethod initialize-instance :after ((client osc-client-udp) &key)
  (make-client-responders client))

(defgeneric make-client-responders (server))

(defmethod make-client-responders ((client osc-client-udp))
  (add-osc-responder client "/cl-osc/server/registered"
      (cmd args device address port timetag)
    (format t "Registered with server at ~A~%"
            (make-addr+port-string address port)))
  (add-osc-responder client "/cl-osc/server/quit"
      (cmd args device address port timetag)
    (format t "Server ~A has quit~%"
            (make-addr+port-string address port))))

(defgeneric register (client)
  (:method ((client osc-client-udp))
    (send client "/cl-osc/register" (port client))))

(defmethod osc-device-cleanup ((device osc-client-udp))
  (send device "/cl-osc/quit")
  (call-next-method))

(defun make-osc-client-endpoint-tcp (socket debug-mode buffer-size
                                     address-tree clients &optional
                                                            cleanup-fun)
  (socket-make-stream socket
                      :input nil :output t
                      :element-type '(unsigned-byte 8)
                      :buffering :full)
  (let ((client (make-instance 'osc-client-endpoint-tcp
                               :debug-mode debug-mode
                               :address-tree address-tree
                               :socket-buffer (make-socket-buffer
                                               buffer-size)
                               :clients clients
                               :cleanup-fun cleanup-fun)))
    (set-socket socket client)
    (set-listening-thread (make-listening-thread client) client)
    client))

(defmethod make-listening-thread ((receiver osc-client-tcp))
  "Creates a listening thread for tcp clients."
  (sb-thread:make-thread
   (lambda ()
     (unwind-protect
          (loop
             do (multiple-value-bind (buffer length address port)
                    (socket-receive (socket receiver)
                                    (socket-buffer receiver) nil)
                  (when (eq length 0)   ; Closed by remote
                    (sb-thread:terminate-thread
                     sb-thread:*current-thread*))
                  (multiple-value-bind (message timetag)
                      (decode-bundle buffer length)
                    (when (debug-mode receiver)
                      (print-osc-debug-msg receiver message length
                                           (peer-address receiver)
                                           (peer-port receiver) timetag))
                    (dispatch (address-tree receiver) message receiver
                              address port timetag))))
       (osc-device-cleanup receiver)))
   :name (format nil "osc-client-tcp-connection: ~A~%"
                 (name receiver))))
