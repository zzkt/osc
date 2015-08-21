(defpackage :osc
  (:use :cl :sb-bsd-sockets)
  (:documentation "OSC aka the 'open sound control' protocol")
  (:export
   #:make-message
   #:message
   #:make-bundle
   #:bundle
   #:command
   #:args
   #:timetag
   #:elements
   #:encode-message
   #:encode-bundle
   #:decode-message
   #:decode-bundle
   #:make-osc-tree
   #:dp-register
   #:dp-remove
   #:dp-match
   #:dispatch

   #:get-current-timetag            ; osc-time
   #:timetag+
   #:get-unix-time
   #:unix-time->timetag
   #:timetag->unix-time
   #:print-as-double

   #:osc-transmitter                ; osc-devices
   #:osc-transmitter-udp
   #:osc-client
   #:osc-client-udp
   #:osc-client-tcp
   #:osc-server
   #:osc-server-udp
   #:osc-server-tcp
   #:protocol
   #:name
   #:buffer-size
   #:quit
   #:osc-device-cleanup
   #:make-listening-thread          ; listening
   #:add-osc-responder              ; dispatching
   #:remove-osc-responder
   #:make-osc-transmitter           ; transmitters
   #:connect
   #:send
   #:send-bundle
   #:send-to
   #:send-bundle-to
   #:send-all
   #:send-bundle-all
   #:make-osc-client                ; clients
   #:make-client-responders
   #:register
   #:make-osc-server                ; servers
   #:boot
   #:make-server-responders
   #:register-udp-client
   #:unregister-udp-client
   #:register-tcp-client
   #:unregister-tcp-client
   #:post-register-hook
   #:get-tcp-client
   #:print-clients
   #:send-to-client
   #:send-bundle-to-client
   #:*default-osc-buffer-size*      ; sockets
   #:make-name-string
   #:device-active-p
   #:device-socket-name
   #:address
   #:port
   #:peer-address
   #:peer-port))
