(cl:in-package #:osc)

(defparameter *default-osc-buffer-size* 1024)

(defun make-socket-buffer (&optional (size *default-osc-buffer-size*))
  (make-sequence '(vector (unsigned-byte 8)) size))

(defun make-socket (protocol)
  (ecase protocol
    (:udp (make-udp-socket))
    (:tcp (make-tcp-socket))))

(defun make-tcp-socket ()
  (make-instance 'inet-socket :type :stream :protocol :tcp))

(defun make-udp-socket ()
  (make-instance 'inet-socket :type :datagram :protocol :udp))

(defun make-peername-string (socket)
  (multiple-value-bind (addr port)
      (socket-peername socket)
    (make-addr+port-string addr port)))

(defun make-name-string (osc-device)
  (when (socket osc-device)
    (multiple-value-bind (addr port)
	(socket-name (socket osc-device))
      (make-addr+port-string addr port))))

(defun make-addr+port-string (addr port)
  (format nil "~{~A~^.~}:~A" (coerce addr 'list) port))

(defun device-active-p (osc-device)
  (when (socket osc-device)
    (socket-open-p (socket osc-device))))

(defun device-socket-name (osc-device)
  (socket-name (socket osc-device)))

(defun port (osc-device)
  (if (device-active-p osc-device)
      (multiple-value-bind (addr port)
	  (device-socket-name osc-device)
	(declare (ignore addr))
	port)
      (warn "Device not connected.")))

(defun address (osc-device)
  (if (device-active-p osc-device)
      (multiple-value-bind (addr port)
	  (device-socket-name osc-device)
	(declare (ignore port))
	addr)
      (warn "Device not connected.")))

(defun device-socket-peername (osc-device)
  (socket-peername (socket osc-device)))

(defun peer-port (osc-device)
  (if (device-active-p osc-device)
      (multiple-value-bind (addr port)
	  (device-socket-peername osc-device)
	(declare (ignore addr))
	port)
      (warn "Device not connected.")))

(defun peer-address (osc-device)
  (if (device-active-p osc-device)
      (multiple-value-bind (addr port)
	  (device-socket-peername osc-device)
	(declare (ignore port))
	addr)
      (warn "Device not connected.")))
