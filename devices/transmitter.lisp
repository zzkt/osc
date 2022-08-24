(cl:in-package #:osc)

;; Only UDP devices can be transmitters.

(defun make-osc-transmitter (&key debug-mode cleanup-fun)
  (make-instance 'osc-transmitter-udp
                 :debug-mode debug-mode
                 :cleanup-fun cleanup-fun))

(defgeneric connect (osc-transmitter host-port &key host-address
                                                 host-name port)
  (:method-combination progn :most-specific-last))

(defmethod connect progn ((transmitter osc-transmitter) host-port
                          &key (host-address nil addr)
                            (host-name "localhost" name) port)
  (when (and addr name)
    (error "Supplied both :host-address and :host-name to connect"))
  (cond (addr
         (when (typep host-address 'string)
           (setf host-address
                 (sb-bsd-sockets:make-inet-address host-address))))
        (t
         (setf host-address
               (sb-bsd-sockets:host-ent-address
                (sb-bsd-sockets:get-host-by-name
                 host-name)))))
  (if (not (device-active-p transmitter))
      (progn
        (let ((socket (make-socket (protocol transmitter))))
          (if port
              (socket-bind socket #(127 0 0 1) port)
              (socket-bind socket))
          (socket-connect socket host-address host-port)
          (socket-make-stream socket
                              :input nil :output t
                              :element-type '(unsigned-byte 8)
                              :buffering :full)
          (set-socket socket transmitter))
        (when (debug-mode transmitter)
          (format t "~%Device connected: ~A~%~A -> ~A~%"
                  (name transmitter) #\Tab
                  (make-addr+port-string (peer-address transmitter)
                                         (peer-port transmitter)))))
      (warn "Already connected"))
  transmitter)

(defmethod quit ((transmitter osc-transmitter-udp))
  (if (device-active-p transmitter)
      (osc-device-cleanup transmitter)
      (warn "Not connected: ~A" (name transmitter))))


;;;=====================================================================
;;; Sending functions
;;;=====================================================================

(defmacro osc-write-to-stream (stream &body msg)
  `(progn (write-sequence ,@msg ,stream)
          (finish-output ,stream)))

(defgeneric send (transmitter data)
  (:method ((transmitter osc-transmitter) data)
    (let ((bytes (encode-osc-data data)))
      (osc-write-to-stream
          (slot-value (socket transmitter) 'stream) bytes))))

(defgeneric send-msg (transmitter command &rest args)
  (:method ((transmitter osc-transmitter) command &rest args)
    (let ((message (make-message command args)))
      (send transmitter message))))

(defgeneric send-bundle (transmitter timetag command &rest args)
  (:method ((transmitter osc-transmitter) timetag command &rest args)
    (let ((bundle (bundle timetag
                          (make-message command args))))
      (send transmitter bundle))))

;; Unconnected sending (UDP only)

(defgeneric send-to (transmitter address port data)
  (:method ((transmitter osc-transmitter-udp) address port data)
    (socket-send (socket transmitter)
                 (encode-osc-data data) nil
                 :address (list address port))))

(defgeneric send-msg-to (transmitter address port command &rest args)
  (:method ((transmitter osc-transmitter-udp) address port command
            &rest args)
    (let ((message (make-message command args)))
      (send-to transmitter address port message))))

(defgeneric send-bundle-to (transmitter address port timetag command
                            &rest args)
  (:method ((transmitter osc-transmitter-udp) address port timetag
            command &rest args)
    (let ((bundle (bundle timetag
                          (make-message command args))))
      (send-to transmitter address port bundle))))
