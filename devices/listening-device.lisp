(cl:in-package #:osc)

(defgeneric make-listening-thread (listening-device))

(defmethod connect progn ((listening-device listening-device)
			  host-port &key host-address host-name port)
  (declare (ignore host-port host-address host-name port))
  (set-listening-thread (make-listening-thread listening-device)
			listening-device))

(defmethod quit ((device listening-device))
  (sb-thread:terminate-thread (listening-thread device)))

(defmethod osc-device-cleanup ((device listening-device))
  (set-listening-thread nil device)
  (call-next-method))

(defmethod osc-device-cleanup ((device receiving-device))
  (fill (socket-buffer device) 0)
  (call-next-method))

(defun print-osc-debug-msg (receiver message length address port
			    timetag)
  (format t "~%~A~%received:~A~A ~A bytes from ~A ~A ~%timetag:~A~A~%unix-time:~A~F~%"
	  (name receiver) #\Tab message length address port #\Tab
	  timetag #\Tab (when timetag (timetag->unix-time timetag))))
