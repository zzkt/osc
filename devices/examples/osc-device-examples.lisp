(cl:in-package #:osc)

(ql:quickload "osc")

;;;=====================================================================
;;; OSC UDP transmitter -> server
;;;=====================================================================

(defparameter *osc-server* (make-osc-server :protocol :udp
                                            :debug-mode t))

(boot *osc-server* 57127)

(defparameter *osc-transmitter* (make-osc-transmitter
                                 :debug-mode t))

(connect *osc-transmitter* 57127 :host-name "localhost")
(device-active-p *osc-transmitter*)
(device-socket-name *osc-transmitter*)
(address *osc-transmitter*)
(port *osc-transmitter*)
(device-socket-peername *osc-transmitter*)
(peer-address *osc-transmitter*)
(peer-port *osc-transmitter*)

(send *osc-transmitter* "/bar" 1 2 9)

(send-bundle *osc-transmitter*
             :time ; current real time
             "/foo" 1 2 3)

(send-bundle *osc-transmitter*
             :now ; immediately
             "/foo" 1 2 3)

(send-bundle *osc-transmitter*
             (unix-time->timetag 1234567890.1234567d0)
             "/foo" 1 2 3)

(quit *osc-transmitter*)
(quit *osc-server*)


;;;=====================================================================
;;; OSC UDP client <-> server
;;;=====================================================================

(defparameter *osc-server* (make-osc-server :protocol :udp
                                            :debug-mode t))

(boot *osc-server* 57127)

(defparameter *osc-client* (make-osc-client
                            :protocol :udp
                            :debug-mode t))

(connect *osc-client* 57127 :host-name "localhost")

;; A UDP server can't know about a client unless it registers.
(print-clients *osc-server*)
(register *osc-client*)
(print-clients *osc-server*)
(quit *osc-client*)                     ; quit notifies the server
(print-clients *osc-server*)

(connect *osc-client* 57127 :host-name "localhost")

(send *osc-client* "/foo" 2 99)

(send-bundle *osc-client*
             (unix-time->timetag 1234567890.1234567d0)
             "/foo" 1 2 3)

(send-bundle *osc-client* :now "/foo" 1)

(send-bundle *osc-client* :time "/foo" 1)

;; Using the server as a transmitter.
(send-to *osc-server* (address *osc-client*) (port *osc-client*)
         "/bar" 1 2 3)

;; If a client is registered...
(send-to-client *osc-server* (make-name-string *osc-client*)
                "/bar" 2 99)

(register *osc-client*)

(send-to-client *osc-server* (make-name-string *osc-client*)
                "/bar" 2 99)

(send-bundle-to-client *osc-server*
                       (make-name-string *osc-client*)
                       :timeq "/bar" 2 99)

(add-osc-responder *osc-server* "/echo-sum"
    (cmd args device address port timetag)
  (send-to device address port "/echo-answer" (apply #'+ args)))

(add-osc-responder *osc-client* "/echo-answer"
    (cmd args device address port timetag)
  (format t "~%Sum is ~A" (car args)))
  
(send *osc-client* "/echo-sum" 1 2 3 4)

(add-osc-responder *osc-server* "/timetag+1"
    (cmd args device address port timetag)
  (send-bundle-to device address port (timetag+ timetag 1) "/future"))

(send-bundle *osc-client* (get-current-timetag)
             "/timetag+1")

;; Send a messages to all registered clients.
(send-all *osc-server* "/foo" 1 2 3)

(send-bundle-all *osc-server* :now "/foo" 1 2 3)

(quit *osc-client*)
(quit *osc-server*)


;;;=====================================================================
;;; OSC TCP client <-> server
;;;=====================================================================

(defparameter *osc-server* (make-osc-server :protocol :tcp
                                            :debug-mode t))

(boot *osc-server* 57127)

(defparameter *osc-client* (make-osc-client
                            :protocol :tcp
                            :debug-mode t))

(connect *osc-client* 57127 :host-name "localhost")

(device-active-p *osc-client*)
(device-socket-name *osc-client*)
(device-socket-peername *osc-client*)

(send *osc-client* "/foo" 1 2 3)

(send-to-client *osc-server* (make-name-string
                              *osc-client*)
                "/foo" 1 2 3)

(defparameter *osc-client2* (make-osc-client
                             :protocol :tcp
                             :debug-mode t))

(connect *osc-client2* 57127
         :host-address "127.0.0.1"
         :port 57666) ; choose local port

(device-socket-name *osc-client2*)

(send *osc-client2* "/bar" 4 5 6 9)

(print-clients *osc-server*)

(add-osc-responder *osc-server* "/print-sum"
    (cmd args device address port timetag)
  (format t "Sum = ~A~%" (apply #'+ args)))

(send *osc-client2* "/print-sum" 4 5 6 9)

(add-osc-responder *osc-server* "/echo-sum"
    (cmd args disp address port timetag)
  (send disp cmd (apply #'+ args)))

(send *osc-client2* "/echo-sum" 4 5 6 9)

(send-all *osc-server* "/bar" 1 2 3) ; send to all peers

(add-osc-responder *osc-server* "/echo-sum-all"
    (cmd args disp address port timetag)
  (send-all disp cmd (apply #'+ args)))

; Send to all peers (excluding self).
(send *osc-client2* "/echo-sum-all" 1 2 3)

(quit *osc-client*)
(quit *osc-client2*)
(quit *osc-server*)


;;;=====================================================================
;;; OSC UDP client <-> sclang
;;;=====================================================================

(defparameter *osc-client* (make-osc-client
                            :protocol :udp
                            :debug-mode t))

(connect *osc-client* 57120 :host-name "localhost" :port 57127)
(address *osc-client*)
(port *osc-client*)
(peer-address *osc-client*)
(peer-port *osc-client*)

;;---------------------------------------------------------------------
;; run in sc
c=OSCresponder(nil,
               '/foo',
               {|t,r,msg,addr| [t,r,msg,addr].postln}).add
;;---------------------------------------------------------------------

(send *osc-client* "/foo" 1 2 3)

(send-bundle *osc-client*
             (get-current-timetag)
             "/foo" 3)

(add-osc-responder *osc-client* "/echo-sum"
    (cmd args disp addr port timetag)
  (send disp cmd (apply #'+ args)))

;;---------------------------------------------------------------------
;; Send /echo-sum from sc, and lisp returns the sum.
n=NetAddr("localhost", 57127)

e=OSCresponder(nil,
               '/echo-sum',
               {|t,r,msg,addr| 
                   [t,r,msg,addr].postln;
               }).add

n.sendMsg('/echo-sum', 1, 2, 3) // send numbers, lisp returns sum.
;;---------------------------------------------------------------------

(quit *osc-client*)


;;;=====================================================================
;;; OSC UDP client <-> scsynth
;;;=====================================================================

(defparameter *osc-client* (make-osc-client
                            :protocol :udp
                            :debug-mode t))

(connect *osc-client* 57110 :host-name "localhost" :port 57127)

(send *osc-client* "/s_new" "default" 1001 0 0 "freq" 500)

(send *osc-client* "/n_free" 1001)

(send-bundle *osc-client*
             (timetag+ (get-current-timetag) 2) ; 2 secs later
             "/s_new" "default" 1001 0 0 "freq" 500)

(send *osc-client* "/n_free" 1001)

(quit *osc-client*) ; Sends default /quit notification which scsynth
                    ; ignores. Ideally osc-client should be subclassed
                    ; to allow scsynth specific behaviour to be
                    ; implemented.
