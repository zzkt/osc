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
                  (multiple-value-bind (data timetag)
                      (osc:decode-bundle buffer :end length)
                    (when (debug-mode receiver)
                      (print-osc-debug-msg receiver data length
                                           address port timetag))
                    (dispatch (address-tree receiver) data receiver
                              address port))))
       (osc-device-cleanup receiver)))
   :name (format nil "osc-receiver-udp: ~A~%" (name receiver))))


;;;=====================================================================
;;; OSC Responders
;;;=====================================================================

(defmacro add-osc-responder (dispatcher cmd-name
                             (cmd args device address port timetag bundle)
                             &body body)
  `(dp-register (address-tree ,dispatcher) ,cmd-name
                (lambda (,cmd ,args ,device ,address ,port ,timetag
                         ,bundle)
                  (declare (ignorable ,cmd ,args ,device ,address
                                      ,port ,timetag ,bundle))
                  ,@body)))

(defgeneric remove-osc-responder (dispatcher address)
  (:method ((dispatcher dispatching-device) address)
    (dp-remove (address-tree dispatcher) address)))
