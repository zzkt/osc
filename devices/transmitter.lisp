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
	  (socket-bind socket #(127 0 0 1) port)
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

(defgeneric send (transmitter &rest msg-args)
  (:method ((transmitter osc-transmitter) &rest msg-args)
    (let ((msg (apply #'encode-message msg-args)))
      (osc-write-to-stream
	  (slot-value (socket transmitter) 'stream) msg))))

(defgeneric send-bundle (transmitter timetag &rest msg-args)
  (:method ((transmitter osc-transmitter) timetag &rest msg-args)
    (let ((msg (encode-bundle msg-args timetag)))
      (osc-write-to-stream
	  (slot-value (socket transmitter) 'stream) msg))))

;; Unconnected sending

(defgeneric send-to (transmitter address port &rest msg-args)
  (:method ((transmitter osc-transmitter-udp) address port &rest
	    msg-args)
    (socket-send (socket transmitter)
		 (apply #'encode-message msg-args) nil
		 :address (list address port))))

(defgeneric send-bundle-to (transmitter address port timestamp &rest
					msg-args)
  (:method ((transmitter osc-transmitter-udp) address port timestamp
	    &rest msg-args)
    (socket-send (socket transmitter)
		 (apply #'encode-bundle msg-args (list timestamp)) nil
		 :address (list address port))))

;; Server functions

(defgeneric send-all (server &rest msg-args))

(defmethod send-all ((server osc-server-udp) &rest msg-args)
  (loop for addr+port being the hash-value in (clients server)
     do (apply #'send-to server (first addr+port) (second addr+port)
	       msg-args)))

(defmethod send-all ((server osc-server-tcp) &rest msg-args)
  (loop for endpoint being the hash-value in (clients server)
     do (apply #'send endpoint msg-args)))

(defmethod send-all ((client-endpoint osc-client-endpoint) &rest
		     msg-args)
  (loop for endpoint being the hash-value in (clients client-endpoint)
     unless (eq endpoint client-endpoint) ; don't send to sender
     do (apply #'send endpoint msg-args)))

(defgeneric send-bundle-all (server timetag &rest msg-args))

(defmethod send-bundle-all ((server osc-server-udp) timetag &rest
			    msg-args)
  (loop for addr+port being the hash-value in (clients server)
     do (apply #'send-bundle-to server (first addr+port)
	       (second addr+port) timetag msg-args)))

(defmethod send-bundle-all ((server osc-server-tcp) timetag &rest
			    msg-args)
  (loop for endpoint being the hash-value in (clients server)
     do (apply #'send-bundle endpoint timetag msg-args)))

(defmethod send-bundle-all ((client-endpoint osc-client-endpoint)
			    timetag &rest msg-args)
  (loop for endpoint being the hash-value in (clients client-endpoint)
     unless (eq endpoint client-endpoint) ; don't send to sender
     do (apply #'send-bundle endpoint timetag msg-args)))
