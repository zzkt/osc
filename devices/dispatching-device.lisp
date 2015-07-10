(cl:in-package #:osc)

(defmethod make-listening-thread ((receiver dispatching-device-udp))
  "Creates a listening thread for udp devices (client and server)."
  (sb-thread:make-thread
   (lambda ()
     (unwind-protect
          (loop
             do (multiple-value-bind (buffer length address port)
                    (socket-receive (socket receiver)
                                    (socket-buffer receiver) nil)
                  (multiple-value-bind (message timetag)
                      (osc:decode-bundle buffer length)
                    (when (debug-mode receiver)
                      (print-osc-debug-msg receiver message length
                                           address port timetag))
                    (osc:dispatch (address-tree receiver) message
                                  receiver address port timetag))))
       (osc-device-cleanup receiver)))
   :name (format nil "osc-receiver-udp: ~A~%" (name receiver))))


;;;=====================================================================
;;; OSC Responders
;;;=====================================================================

(defmacro add-osc-responder (dispatcher cmd-name
                             (cmd args disp addr port timetag) &body
                                                                 body)
  `(dp-register (address-tree ,dispatcher) ,cmd-name
                (lambda (,cmd ,args ,disp ,addr ,port ,timetag)
                  (declare (ignorable ,cmd ,args ,disp ,addr
                                      ,port ,timetag))
                  ,@body)))

(defgeneric remove-osc-responder (dispatcher address)
  (:method ((dispatcher dispatching-device) address)
    (dp-remove (address-tree dispatcher) address)))
