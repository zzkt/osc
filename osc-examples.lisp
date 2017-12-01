;; -*- mode: lisp -*-
;;
;; Examples of how to send OSC messages. ..
;;
;; Copyright (C) 2004 FoAM vzw
;;
;; Authors
;;  - nik gaffney <nik@fo.am>
;;
;;;;; ;; ;     ;    ;;    ;             ;

;;;;;::::: :   : : ;     ;; ;       ; ;    ;         ;     ;
;;
;; Commentry
;;
;;  These examples are currently sbcl specific, but should be easily ported to
;;  work with trivial-sockets, acl-compat or something similar.
;;  They should be enough to get you started.
;;
;;  eg. listen on port 6667 for incoming messages
;;
;;   (osc-receive-test 6667)
;;
;;  send a test message to localhost port 6668
;;
;;   (osc-send-test #(127 0 0 1) 6668)
;;
;;  listen on port 6667 and send to 10.0.89:6668
;;                     (note the ip# is formatted as a vector)
;;
;;   (osc-reflector-test 6667 #(10 0 0 89) 6668)
;;
;;;;;:::;;: ;     ; ;::: ;     ;; ;;     ;        ;;      ;

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :osc)
  (ql:quickload :usocket)
  (defpackage osc-examples (:use :cl :osc :usocket)))
(in-package :osc-examples)

(defun osc-receive-test (port)
  "a basic test function which attempts to decode an osc message on given port.
  note ip#s need to be in the format #(127 0 0 1) for now.. ."
  (let ((s (socket-connect nil nil
                           :local-port port
                           :local-host #(127 0 0 1)
                           :protocol :datagram
                           :element-type '(unsigned-byte 8)))
        (buffer (make-sequence '(vector (unsigned-byte 8)) 1024)))
    (format t "listening on localhost port ~A~%~%" port)
    (unwind-protect
         (loop do
               (socket-receive s buffer (length buffer))
               (format t "received -=> ~S~%" (osc:decode-bundle buffer)))
      (when s (socket-close s)))))


(defun osc-send-test (host port)
  "a basic test function which sends osc test message to a given port/hostname.
  note ip#s need to be in the format #(127 0 0 1) for now.. ."
  (let ((s (socket-connect host port
                           :protocol :datagram
                           :element-type '(unsigned-byte 8)))
        (b (osc:encode-message "/foo/bar" "baz" 1 2 3 (coerce PI 'single-float))))
    (format t "sending to ~a on port ~A~%~%" host port)
    (unwind-protect
         (socket-send s b (length b))
      (when s (socket-close s)))))


(defun osc-reflector-test (listen-port send-host send-port)
  "reflector. listens on a given port and sends out on another.
   note ip#s need to be in the format #(127 0 0 1) for now.. ."
  (let ((in (socket-connect nil nil
                            :local-port listen-port
                            :protocol :datagram
                            :element-type '(unsigned-byte 8)))
        (out (socket-connect send-host send-port
                             :protocol :datagram
                             :element-type '(unsigned-byte 8)))
        (buffer (make-sequence '(vector (unsigned-byte 8)) 1024)))
    (unwind-protect
         (loop do
               (socket-receive in buffer (length buffer))
               (format t "glonked -=> message: ~{~A, ~}~%"
                       (osc:decode-bundle buffer))
               (let ((mess (apply #'osc:encode-message
                                  (cons "/echo"
                                        (osc:decode-message buffer)))))
                 (socket-send out mess (length mess))))
      (when in (socket-close in))
      (when out (socket-close out)))))

;;end
