;; -*- mode: lisp -*-
;;
;; Quick and dirty tests for cl-osc
;;
;; You are granted the rights to distribute and use this software
;; as governed by the terms of GNU Public License (aka the GPL)
;; see the LICENCE file.

;; Authors
;;  - nik gaffney <nik@fo.am>

(require "usocket")

(defun osc-write ()
  "a basic test function which sends various osc stuff on port 5555"
  (let ((sock (sb-bsd-sockets::make-instance
               'inet-socket
               :type :datagram
               :protocol :udp)))
    (sb-bsd-sockets::socket-connect sock #(127 0 0 1) 5555)
    (let ((stream
            (sb-bsd-sockets::socket-make-stream
             sock
             :input t
             :output t
             :element-type '(unsigned-byte 8)
             :buffering :full)))
      (prin1 "int? ")
      (write-sequence (oti) stream)
      (force-output stream)
      (prin1 "float? ")
      (write-sequence (otf) stream)
      (force-output stream)
      (prin1 "string?")
      (write-sequence (ots) stream)
      (force-output stream)
      (prin1 "mutliple args?")
      (write-sequence (otm) stream)
      (force-output stream)
      (sb-bsd-sockets::socket-close sock)
      )))

(defun oti () (osc:encode-message "/test/int" 3))
(defun otf () (osc:encode-message "/test/float" 4.337))
(defun ots () (osc:encode-message "/test/string" "wonky_stringpuk"))
(defun otbl () (osc:encode-message "/test/blob" #(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(defun otm ()  (osc:encode-message "/test/multi 5.78 1" "five point seven eight" "and one"))
(defun otbn () (osc-encode-bundle (osc-make-test-bundle)))

;; test todo
;;  - negative floats
;;  - bignums
;;  - blobs, and long args
;;  - byte aligning 0,1,2,3,4 mod
;;  - error catching, junk data

(defun osc-test ()
  (format t "osc tests: ~a"
          (list
           (osc-t2) (osc-t3) (osc-t4)
           (osc-t5) (osc-t6) (osc-t7)
           (osc-t8) (osc-t9) (osc-t10)
           (osc-t11) (osc-t12) (osc-t13)))
  T)

(defun osc-t2 ()
  (equalp '("/dip/lop" 666)
          (osc:decode-message #(47 100 105 112 47 108 111 112 0 0 0 0 44 105 0 0 0 0 2 154))))

(defun osc-t3 ()
  (equalp '#(0 0 0 3 116 101 115 116 0 0 0 0 0 0 0 2 0 0 0 1 64 159 92 41)
          (osc::encode-data '(3 "test" 2 1 4.98))))

(defun osc-t4 ()
  (equalp #(44 105 115 102 0 0 0 0)
          (osc::encode-typetags '(1 "terrr" 3.4))))

(defun osc-t5 ()
  (equalp #(44 105 105 102 0 0 0 0)
          (osc::encode-typetags '(1 2 3.3))))

(defun osc-t6 ()
  (equal '("/test/one" 1 2 3.3)
         (osc:decode-message #(47 116 101 115 116 47 111 110 101 0 0 0 44 105 105 102 0 0 0 0 0 0 0 1 0 0 0 2 64 83 51 51))))

(defun osc-t7 ()
  (equalp '(#(0 0 0 0 0 0 0 1) ("/voices/0/tm/start" 0.0)
            ("/foo/stringmessage" "a" "few" "strings") ("/documentation/all-messages"))
          (osc:decode-bundle
           #(#x23 #x62 #x75 #x6e
             #x64 #x6c #x65 0
             0    0    0    0
             0    0    0    #x1
             0    0    0    #x20
             #x2f #x64 #x6f #x63
             #x75 #x6d #x65 #x6e
             #x74 #x61 #x74 #x69
             #x6f #x6e #x2f #x61
             #x6c #x6c #x2d #x6d
             #x65 #x73 #x73 #x61
             #x67 #x65 #x73 0
             #x2c 0    0    0
             0    0    0    #x2c
             #x2f #x66 #x6f #x6f
             #x2f #x73 #x74 #x72
             #x69 #x6e #x67 #x6d
             #x65 #x73 #x73 #x61
             #x67 #x65 0    0
             #x2c #x73 #x73 #x73
             0    0    0    0
             #x61 0    0    0
             #x66 #x65 #x77 0
             #x73 #x74 #x72 #x69
             #x6e #x67 #x73 0
             0    0    0    #x1c
             #x2f #x76 #x6f #x69
             #x63 #x65 #x73 #x2f
             #x30 #x2f #x74 #x6d
             #x2f #x73 #x74 #x61
             #x72 #x74 0    0
             #x2c #x66 0    0
             0    0    0    0))))

(defun osc-t8 ()
  (equalp (osc::encode-message "/blob/x" #(1 2 3 4 5 6 7 8 9))
          #(47 98 108 111 98 47 120 0 44 98 0 0 0 0 0 9 1 2 3 4 5 6 7 8 9 0 0 0)))

(defun osc-t9 ()
  (equalp '("/blob/x" #(1 2 3 4 5 6 7 8 9))
          (osc::decode-message
           #(47 98 108 111 98 47 120 0 44 98 0 0 0 0 0 9 1 2 3 4 5 6 7 8 9 0 0 0))))

(defun osc-t10 ()
  (equalp '("/blob" #(1 29 32 43 54 66 78 81) 2 "lop")
          (osc:decode-message
           #(47 98 108 111 98 0 0 0 44 98 105 115 0 0 0 0
             0 0 0 8 1 29 32 43 54 66 78 81 0 0 0 0 0 0 0 2 108 111 112 0))))

(defun osc-t11 ()
  (equalp '(#(0 0 0 0 0 0 0 1) ("/string/a/ling" "slink" "slonk" "slank")
            ("/we/wo/w" 1 2 3.4) ("/blob" #(1 29 32 43 54 66 78 81 90) "lop" -0.44))
          (osc:decode-bundle
           #(35 98 117 110 100 108 101 0 0 0 0 0 0 0 0 1 0 0 0 40 47 98 108 111 98 0 0 0
             44 98 115 102 0 0 0 0 0 0 0 9 1 29 32 43 54 66 78 81 90 0 0 0 108 111 112 0
             190 225 71 174 0 0 0 32 47 119 101 47 119 111 47 119 0 0 0 0 44 105 105 102 0
             0 0 0 0 0 0 1 0 0 0 2 64 89 153 154 0 0 0 48 47 115 116 114 105 110 103 47 97
             47 108 105 110 103 0 0 44 115 115 115 0 0 0 0 115 108 105 110 107 0 0 0 115
             108 111 110 107 0 0 0 115 108 97 110 107 0 0 0))))



#+sbcl (defun osc-read (port)
  "A basic test function which attempts to decode osc stuff on PORT."
  (let ((s (make-instance 'inet-socket
                          :type :datagram
                          :protocol (get-protocol-by-name "udp")))
        (buffer (make-sequence '(vector (unsigned-byte 8)) 512)))
    (format t "Socket type is ~A on port ~A~%" (sockopt-type s) port)
    (socket-bind s #(127 0 0 1) port)
    (socket-receive s buffer nil :waitall t)
    (socket-close s)
    (osc:decode-message buffer)
    ))

;;(osc-decode-message data)

(defun osc-ft ()
  (and (eql (osc::DECODE-FLOAT32 #(63 84 32 93))  0.8286188)
       (eql (osc::DECODE-FLOAT32 #(190 124 183 78)) -0.246793)))


;; equalp but not eql
(defun osc-t12 ()
  (equalp (osc:encode-message "/asdasd" 3.6 4.5)
          #(47 97 115 100 97 115 100 0 44 102 102 0 64 102 102 102 64 144 0 0)))

;; equal but not eql
(defun osc-t13 ()
  (equal (osc:decode-message #(47 97 115 100 97 115 100 0 44 102 102 0 64 102 102 102 64 144 0 0))
         (list "/asdasd" 3.6 4.5)))

;; not symmetrical?  how much of a problem is this?
(defun osc-asym-t1 ()
  "this test will fail"
  (osc:decode-message
   (osc:encode-message
    (osc:decode-message #(47 97 115 100 97 115 100 0 44 102 102 0 64 102 102 102 64 144 0 0)))))

(defun osc-asym-t2 ()
  "testing the assumptions about representations of messages"
  (setf packed-msg #(47 97 115 100 97 115 100 0 44 102 102 0 64 102 102 102 64 144 0 0))
  (setf cons-msg (osc:decode-message packed-msg))
  (osc:encode-message (values-list cons-msg)))

#|
sc3 server

32 byte message:
2f (/)  6e (n)  5f (_)  73 (s)
65 (e)  74 (t)  0 ()    0 ()
2c (,)  69 (i)  73 (s)  66 (f)
0 ()    0 ()    0 ()    0 ()
0 ()    0 ()    1 ()    fffffff6 (?)
66 (f)  72 (r)  65 (e)  71 (q)
0 ()    0 ()    0 ()    0 ()
3f (?)  ffffff80 (?)    0 ()    0 ()

/n_set 502 "freq" 1.000000

|#

(defun run-tests ()
  (osc-test))
