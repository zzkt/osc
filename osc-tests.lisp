;; -*- mode: lisp -*-
;;
;; Various tests for cl-osc using 5am
;;
;; Authors
;;  - nik gaffney <nik@fo.am>

(defpackage :osc/tests
  (:use :cl :osc :fiveam))

(in-package :osc/tests)

;; (in-package :osc)
;; (require "fiveam")

;; setup various test suites

(def-suite synchroscope
  :description "OSC test suite(s).")

(def-suite data-encoding
  :description "Test encoding and decoding of OSC data types."  :in synchroscope)

(def-suite message-encoding
  :description "Test encoding and decoding of OSC messages."  :in synchroscope)

(def-suite protocol-v1.0
  :description "OSC v1.0 compatibility."  :in synchroscope)

(def-suite protocol-v1.1
  :description "OSC v1.1 compatibility."  :in synchroscope)

(def-suite interoperability
  :description "Test interoperability (e.g. supercollider & pd)"  :in synchroscope)

;; test todo
;;  - negative floats, NaN +/- Inf, etc
;;  - bignums
;;  - blobs, and long args
;;  - byte aligning 0,1,2,3,4 mod
;;  - error catching, junk data
;;  - edge cases?

(in-suite data-encoding)

(test osc-int32
  "OSC int32 encoding tests."
  (is (equalp
       (osc::encode-int32 16843009) #(1 1 1 1)))
  (is (equalp
       (osc::decode-int32 #(1 1 11 111)) 16845679))
  (is (equalp
       (osc::encode-int32 -16843010) #(254 254 254 254)))
  (is (equalp
       (osc::decode-int32 #(255 255 255 255)) -1)))

(test osc-string
  "OSC string encoding tests."
  (is (equalp
       (osc::decode-string #(110 117 108 108 32 112 97 100 100 101 100 0))
       "null padded"))
  (is (equalp
       (osc::encode-string "OSC string encoding test")
       #(79  83 67  32  115 116 114 105 110 103 32  101
         110 99 111 100 105 110 103 32  116 101 115 116 0 0 0 0))))

;; blob
;;   (osc::encode-blob "THE BLOB")

(test osc-blob
  "OSC blob encoding tests."
  (is (equalp
       (osc::encode-blob #(1 1 1 1)) #(0 0 0 4 1 1 1 1))))

(test osc-timetag
  "OSC timetag encoding tests."
  (is (equalp
       (osc::encode-timetag :now) #(0 0 0 0 0 0 0 1))))

(test osc-int64
  "OSC int64 encoding tests."
  (is (equalp
       (osc::encode-int64 16843009) #(0 0 0 0 1 1 1 1)))
  (is (equalp
       (osc::decode-int64 #(1 1 1 1 1 1 1 1)) 72340172838076673))
  (is (equalp
       (osc::encode-int64 -8000000000000000008) #(144 250 74 98 196 223 255 248)))
  (is (equalp
       (osc::decode-int64 #(254 1 254 1 254 1 254 1)) -143554428589179391)))


;; floating point tests
;;  these tests cover only encoding and representation, not computation.

(test osc-float32
  "OSC float32 encoding tests."
  (is (equalp
       (osc::encode-float32 1.00001) #(63 128 0 84)))
  (is (equalp
       (osc::decode-float32  #(1 1 1 1)) 2.3694278s-38))
  (is (equalp
       (osc::encode-float32 -2.3694278s33) #(246 233 164 196)))
  (is (equalp
       (osc::decode-float32  #(254 255 255 255)) -1.7014117s38))
  (is (equalp
       (osc::decode-float32 #(127 255 255 255))
       :NOT-A-NUMBER)))

(test osc-float64
  "OSC float64 encoding tests."
  (is (equalp
       (osc::encode-float64 23.1d0) #(64 55 25 153 153 153 153 154)))
  (is (equalp
       (osc::decode-float64 #(64 55 25 153 153 153 153 154)) 23.1d0))
  (is (equalp
      (osc::decode-float64 #(1 1 1 1 1 1 1 1)) 7.748604185489348d-304))
  (is (equalp
        (osc::decode-float64 #(128 0 0 0 0 0 0 0)) -0.0d0))
  (is (equalp
       (osc::decode-float64 #(255 240 0 0 0 0 0 0))
       :NEGATIVE-INFINITY))
  (is (equalp
       (osc::decode-float64 #(255 255 255 255 0 0 0 0))
       :NOT-A-NUMBER)))

;; #+sbcl (osc::decode-float32 #(127 255 255 255)) -> #<SINGLE-FLOAT quiet NaN>
;; see also -> https://github.com/Shinmera/float-features/

;; single-float

(defun f32b (s) (write-to-string (osc::encode-float32 s ) :base 2))
(defun f64b (s) (write-to-string (osc::encode-float64 s ) :base 2))

(test single-float
  "Various single floats of interest."
  (is (equalp
       (f32b 0.000000059604645s0) "#(110011 10000000 0 0)"))
  (is (equalp
       (f32b 0.000060975552s0) "#(111000 1111111 11000000 0)")))

(test float-features
  #+sbcl (pass
          (format nil "SBCL floating point modes: ~A~%" (sb-int:get-floating-point-modes))))


;; empty messages tagged T, F, N, I

(in-suite message-encoding)

;; messages

(test osc-message-1
  "OSC message encoding tests. address and single int."
  :suite 'message-encoding
  (is (equalp
       '("/test/int" -1)
       (osc:decode-message #(47 116 101 115 116 47 105 110 116 0 0 0 44 105 0 0 255 255 255 255)))))


;; check padding boundaries. 1-3 or 1-4?
(test osc-t4
  "OSC typetag encoding test. string, ints and floats."
  (is (equalp
       #(44 105 115 102 0 0 0 0)
       (osc::encode-typetags '(1 "terrr" 3.4)))))

(test osc-t5
  "OSC typetag encoding test. ints and floats."
  (is (equalp
       #(44 105 105 102 0 0 0 0)
       (osc::encode-typetags '(1 2 3.3)))))

(test osc-t6
  "OSC message decoding test. ints and floats."
  (is (equalp
       '("/test/one" 1 2 3.3)
       (osc:decode-message
        #(47  116 101 115 116 47  111 110
          101 0   0   0   44  105 105 102
          0   0   0   0   0   0   0   1
          0   0   0   2   64  83  51  51)))))

(test osc-t7
  "OSC bundle decoding test. strings, ints and floats."
  (is (equalp
       '(#(0 0 0 0 0 0 0 1)
         ("/voices/0/tm/start" 0.0)
         ("/foo/stringmessage" "a" "few" "strings")
         ("/documentation/all-messages"))
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
          0    0    0    0)))))

(test osc-t8
  "OSC message encoding test. blob."
  (is (equalp
       (osc::encode-message "/blob/x" #(1 2 3 4 5 6 7 8 9))
       #(47 98 108 111 98 47 120 0 44 98 0 0 0 0 0 9 1 2 3 4 5 6 7 8 9 0 0 0))))

(test osc-t9
  "OSC message decoding test. blob."
  (is (equalp
       '("/blob/x" #(1 2 3 4 5 6 7 8 9))
       (osc::decode-message
        #(47 98 108 111 98 47 120 0 44 98 0 0 0 0 0 9 1 2 3 4 5 6 7 8 9 0 0 0)))))

(test osc-t10
  "OSC message decoding test. blob, int, string."
  (is (equalp '("/blob" #(1 29 32 43 54 66 78 81) "lop" 2)
              (osc:decode-message
               #(47 98 108 111 98 0 0 0 44 98 115 105 0 0 0
                 0 0 0 0 8 1 29 32 43 54 66 78 81
                 108 111 112 0 0 0 0 2)))))

;; (test osc-t11
;;   "OSC bundle decoding test."
;;   (is (equalp
;;        '(#(0 0 0 0 0 0 0 1)
;;          ("/string/a/ling" "slink" "slonk" "slank")
;;          ("/we/wo/w" 1 2 3.4)
;;          ("/blob" #(1 29 32 43 54 66 78 81 90) "lop" -0.44))
;;        (osc:decode-bundle
;;         #(35 98 117 110 100 108 101 0 0 0 0 0 0 0 0 1 0 0 0 40 47 98 108 111 98 0 0 0
;;           44 98 115 102 0 0 0 0 0 0 0 9 1 29 32 43 54 66 78 81 90 0 0 0 108 111 112 0
;;           190 225 71 174 0 0 0 32 47 119 101 47 119 111 47 119 0 0 0 0 44 105 105 102 0
;;           0 0 0 0 0 0 1 0 0 0 2 64 89 153 154 0 0 0 48 47 115 116 114 105 110 103 47 97
;;           47 108 105 110 103 0 0 44 115 115 115 0 0 0 0 115 108 105 110 107 0 0 0 115
;;           108 111 110 107 0 0 0 115 108 97 110 107 0 0 0)))))


;; equalp but not eql
(test osc-t13
  "OSC message encoding test."
  (is (equalp
       (osc:encode-message "/asdasd" 3.6 4.5)
       #(47 97 115 100 97 115 100 0 44 102 102 0 64 102 102 102 64 144 0 0))))

;; equal but not eql
(test osc-t14
  "OSC message decoding test."
  (is (equalp
       (osc:decode-message
        #(47 97 115 100 97 115 100 0 44 102 102 0 64 102 102 102 64 144 0 0))
       (list "/asdasd" 3.6 4.5))))

;; symmetrical?  how much of a issue is this?
(test osc-recode
  "OSC message encoding & decoding symmetry test."
  (let ((message (osc:decode-message
                  #(47 97 115 100 97 115 100 0 44 102 102 0 64 102 102 102 64 144 0 0))))
    (is (equalp
         message
         (osc:decode-message
          (apply #'osc:encode-message message))))))

;; partially pathological string tests...
(test osc-sp1
  (is (equalp
       (osc:encode-message "/s/t0" "four")
       #(47 115 47 116 48 0 0 0 44 115 0 0 102 111 117 114 0 0 0 0)))
  (is (equalp
       (osc:decode-message #(47 115 47 116 48 0 0 0 44 115 0 0 102 111 117 114 0 0 0 0))
       '("/s/t0" "four"))))

(test osc-sp2
  (is (equalp
       (osc:encode-message "/s/t0" 2 "xxxxx" 3)
       #(47 115 47 116 48 0 0 0 44 105 115 105 0 0 0 0
         0 0 0 2 120 120 120 120 120 0 0 0 0 0 0 3)))
  (is (equalp
       (osc:decode-message
        #(47 115 47 116 48 0 0 0 44 105 115 105 0 0 0 0
          0 0 0 2 120 120 120 120 120 0 0 0 0 0 0 3))
       '("/s/t0" 2 "xxxxx" 3))))

;; (test osc-t16
;;       "OSC message encoding & decoding symmetry test."
;;  (let* ((packed-msg #(47 97 115 100 97 115 100 0 44 102 102 0 64 102 102 102 64 144 0 0))
;;         (cons-msg (osc:decode-message packed-msg)))
;;    (is (equalp
;;         packed-msg
;;         (osc:encode-message (values-list cons-msg))))))

;; v1.0 tests
(in-suite protocol-v1.0)

(test v1.0-required-types
  "OSC data encoding test. All required types for v1.0"
  (is (equalp
       #(0 0 0 3 116 101 115 116 0 0 0 0 67 82 0 0 0 0 0 4 1 2 3 4)
       (osc::encode-data '(3 "test" 2.1e2 #(1 2 3 4))))))

;; v1.1. tests
(in-suite protocol-v1.1)

(test v1.1-required-data-types
  "OSC data encoding test. All required types for v1.1"
  (is (equalp
      #(44 105 104 115 102 100 98 0)
      (osc::encode-typetags '(3
                              4294967297
                              "test"
                              2.1e2
                              2.1d23
                              #(1 2 3 4)
                              ;; (osc::encode-timetag :now)
                              )))))

(test v1.1-keyword-typetags
  "OSC typetag encoding test."
  (is (equalp
       (osc::encode-typetags '(:true :false :null :impulse))
       #(44 84 70 78 73 0 0 0))))

;;  (osc::encode-typetags '("s" 1))


;; play nicely with others
(in-suite interoperability)

(test hex-strings
  "OSC data in hex."
  (is (equalp
       (osc::write-data-as-hex (osc::encode-string "hexadecimate"))
       "#(68 65 78 61 64 65 63 69 6D 61 74 65 0 0 0 0)"))
  (is (equalp
       (osc::decode-string #(#x68 #x65 #x78 #x61 #x64 #x65 #x63 #x69
                             #x6D #x61 #x74 #x65 #x0 #x0 #x0 #x0))
       "hexadecimate")))

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

(run! 'synchroscope)
