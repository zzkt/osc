(defpackage :osc
  (:use :cl :sb-bsd-sockets)
  (:documentation "OSC aka the 'open sound control' protocol")
  (:export #:encode-message
	   #:encode-bundle
	   #:decode-message
	   #:decode-bundle
	   #:make-osc-tree
	   #:dp-register
	   #:dp-remove
	   #:dp-match
	   #:dispatch

	   #:get-current-timetag	; osc-time
	   #:timetag+
	   #:get-unix-time
	   #:unix-time->timetag
	   #:timetag->unix-time
	   #:print-as-double))
