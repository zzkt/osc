;; -*- mode: lisp; coding: utf-8 -*-
;;
;; Various tests for cl-osc using 5am
;;
;; These tests are shared with oscule (https://codeberg.org/zzkt/oscule)
;;
;;   - osc-v1-compatability.janet      -> protocol-v1.0 & data-encoding
;;   - osc-v1.1-compatability.janet    -> protocol-v1.1 & data-encoding
;;   - fuzz-numerics.janet             -> fuzz-numerics
;;   - interop.janet, udp-p2p.janet    -> interoperability
;;
;; Authors
;;  - nik gaffney <nik@fo.am>

(defpackage :osc/tests
  (:use :cl :osc :fiveam))

(in-package :osc/tests)

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

(def-suite fuzz-numerics
  :description "Fuzzing tests for numeric encoding/decoding."  :in synchroscope)


;; Numeric comparisons & PRNG used for fuzzing

;; IEEE 754 float32 has 23 mantissa bits. round-trip error is bounded by 2^-23
;; relative to the value, plus an absolute floor for subnormals.
(defconstant +f32-eps+ 1.1920928955d-7)     ;; 2^-23
(defconstant +f32-abs-min+ 1.401298464d-45) ;; 2^-149 (smallest float32 denormal)

(defun f32= (a b)
  (let ((da (float a 1.0d0))
        (db (float b 1.0d0)))
    (<= (abs (- da db))
        (+ (* (max (abs da) (abs db)) +f32-eps+) +f32-abs-min+))))

;; IEEE 754 float64 has 52 mantissa bits. round-trip error is bounded by 2^-52
;; relative to the value, plus an absolute floor for subnormals.
(defconstant +f64-eps+ 2.220446049250313d-16)       ;; 2^-52
(defconstant +f64-abs-min+ 4.9406564584124654d-324) ;; 2^-1074 (smallest float64 denormal)

(defun f64= (a b)
  (<= (abs (- a b))
      (+ (* (max (abs a) (abs b)) +f64-eps+) +f64-abs-min+)))

(defconstant +float32-min+ (coerce 1.175494351d-38 'single-float))
(defconstant +float32-max+ (coerce 3.402823466d38 'single-float))

;; seeded PRNG for reproducible fuzzing
(defvar *seed* 42)

(defun rand-i32 ()
  "Return a pseudo-random integer in [0, #x7FFFFFFF]."
  (setf *seed* (mod (+ (* *seed* 1103515245) 12345) 2147483648))
  (logand (ash *seed* -16) #x7FFFFFFF))

(defun rand-range (lo hi)
  "Return a pseudo-random integer in [lo, hi)."
  (+ lo (mod (rand-i32) (- hi lo))))

(defun rand-f32 ()
  "Return a pseudo-random single-float in [0, 1)."
  (let ((d (max (rand-i32) 1)))
    (/ (mod (rand-i32) d) (float d))))

(in-suite data-encoding)

;; Integers. 32 and 64 bit. signed and unsigned.
;; (see: osc-v1-compatability.janet & osc-v1.1-compatability.janet)

(test osc-int32
  "OSC int32 encoding/decoding tests."
  (is (equalp
       (osc::encode-int32 16843009) #(1 1 1 1)))
  (is (equalp
       (osc::encode-int32 -16843010) #(254 254 254 254)))
  (is (equalp
       (osc::encode-int32 #xFFFFFFFF) #(255 255 255 255)))
  (is (equalp
       (osc::encode-int32 #x7FFFFFFF) #(127 255 255 255)))
  (is (equalp
       (osc::encode-int32 -2147483648) #(128 0 0 0)))
  (is (equalp
       (osc::decode-int32 #(127 255 255 255)) #x7FFFFFFF))
  (is (equalp
       (osc::decode-int32 #(127 255 255 255)) 2147483647))
  (is (equalp
       (osc::decode-int32 #(128 0 0 0)) -2147483648))
  (is (equalp
       (osc::decode-int32 #(255 255 255 255)) -1)))

(test osc-uint32
  "OSC uint32 decoding tests."
  (is (equalp
       (osc::decode-uint32 #(0 0 0 1)) 1))
  (is (equalp
       (osc::decode-uint32 #(127 255 255 255)) #x7FFFFFFF))
  (is (equalp
       (osc::decode-uint32 #(255 255 255 255)) #xFFFFFFFF)))

(test osc-string
  "OSC string encoding/decoding tests."
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

;; OSC 1.1 data types. character, rgba, midi.
;; (see: osc-v1.1-compatability.janet)

(test osc-char
  "OSC character encoding/decoding tests."
  (is (equalp
       (osc::encode-char 65) #(0 0 0 65)))
  (is (equalp
       (osc::encode-char #\A) #(0 0 0 65)))
  (is (equalp
       (osc::encode-char "a") #(0 0 0 97)))
  (is (equalp
       (osc::decode-char #(0 0 0 97)) 97)))

(test osc-rgba
  "OSC rgba color encoding/decoding tests."
  (is (equalp
       (osc::encode-rgba 255 0 128 255) #(255 0 128 255)))
  (is (equalp
       (osc::decode-rgba #(64 128 192 255)) '(64 128 192 255))))

(test osc-midi
  "OSC midi message encoding/decoding tests."
  (is (equalp
       (osc::encode-midi 1 144 60 127) #(1 144 60 127)))
  (is (equalp
       (osc::decode-midi #(1 144 60 127)) '(1 144 60 127))))

(test osc-alt-string
  "OSC alternate (utf-8) string encoding/decoding tests."
  (let ((s "größtmögliche Fußgängerüberweggeräuschendämpfung"))
    (is (equalp
         (osc::encode-typetags (list s)) #(44 83 0 0)))
    (is (equalp
         (osc::encode-typetags (list "strung")) #(44 115 0 0)))
    (is (equalp
         (osc::encode-string s)
         #(103 114 195 182 195 159 116 109 195 182 103 108 105 99 104 101 32 70 117 195 159 103 195 164 110 103 101 114 195 188 98 101 114 119 101 103 103 101 114 195 164 117 115 99 104 101 110 100 195 164 109 112 102 117 110 103 0 0 0 0)))
    (is (equalp
         (osc:decode-message (osc:encode-message "/alt" s))
         '("/alt" "größtmögliche Fußgängerüberweggeräuschendämpfung")))))

(test osc-int64
  "OSC int64/uint64 encoding/decoding tests."
  (is (equalp
       (osc::encode-int64 1) #(0 0 0 0 0 0 0 1)))
  (is (equalp
       (osc::encode-int64 16843009) #(0 0 0 0 1 1 1 1)))
  (is (equalp
       (osc::encode-int64 -1) #(255 255 255 255 255 255 255 255)))
  (is (equalp
       (osc::encode-int64 -16843009) #(255 255 255 255 254 254 254 255)))
  (is (equalp
       (osc::encode-int64 (- (expt 2 53) 1)) #(0 31 255 255 255 255 255 255)))
  (is (equalp
       (osc::encode-int64 -8000000000000000008) #(144 250 74 98 196 223 255 248)))
  (is (equalp
       (osc::decode-int64 #(1 1 1 1 1 1 1 1)) 72340172838076673))
  (is (equalp
       (osc::decode-int64 #(0 0 0 0 1 1 1 1)) 16843009))
  (is (equalp
       (osc::decode-uint64 #(0 0 0 0 1 1 1 1)) 16843009))
  (is (equalp
       (osc::decode-int64 #(255 255 255 255 255 255 255 255)) -1))
  (is (equalp
       (osc::decode-int64 #(0 31 255 255 255 255 255 255)) 9007199254740991))
  (is (equalp
       (osc::decode-int64 #(254 1 254 1 254 1 254 1)) -143554428589179391))
  ;; round-trip
  (dolist (n (list 1 -1 16843009 -16843009 0 2147483647 -2147483648
                   (- (expt 2 53) 1)))
    (is (= (osc::decode-int64 (osc::encode-int64 n)) n)
        (format nil "int64 round-trip ~a" n))))

;; Floating point tests
;;  these tests cover only encoding and representation, not computation.

(test osc-float32
  "OSC float32 encoding/decoding tests."
  (is (equalp
       (osc::encode-float32 1.00001) #(63 128 0 84)))
  (is (equalp
       (osc::decode-float32  #(1 1 1 1)) 2.3694278s-38))
  (is (equalp
       (osc::encode-float32 -2.3694278e33) #(246 233 164 196)))
  (is (equalp
       (osc::decode-float32  #(254 255 255 255)) -1.7014117s38))
  (is (equalp
       (osc::decode-float32 #(127 127 255 255)) most-positive-single-float))
  ;; NaN and +/-inf decode to keywords
  (is (eq :not-a-number (osc::decode-float32 #(127 255 255 255))))
  (is (eq :positive-infinity (osc::decode-float32 #(127 128 0 0))))
  (is (eq :negative-infinity (osc::decode-float32 #(255 128 0 0)))))

(test osc-float64
  "OSC float64 encoding/decoding tests."
  (is (equalp
       (osc::encode-float64 23.1d0) #(64 55 25 153 153 153 153 154)))
  (is (equalp
       (osc::decode-float64 #(64 55 25 153 153 153 153 154)) 23.1d0))
  (is (equalp
       (osc::decode-float64 #(1 1 1 1 1 1 1 1)) 7.748604185489348d-304))
  (is (equalp
       (osc::decode-float64 #(128 0 0 0 0 0 0 0)) -0.0d0))
  (is (equalp
       (osc::decode-float64 #(127 239 255 255 255 255 255 255))
       most-positive-double-float))
  (is (equalp
       (osc::encode-float64 most-positive-double-float)
       #(127 239 255 255 255 255 255 255)))
  (is (equalp
       (osc::encode-float64 (expt 2.0d0 1022)) #(127 208 0 0 0 0 0 0)))
  (is (equalp
       (osc::encode-float64 pi) #(64 9 33 251 84 68 45 24)))
  (is (equalp
       (osc::encode-float64 least-positive-double-float) #(0 0 0 0 0 0 0 1)))
  ;; NaN and +/-inf decode to keywords
  (is (eq :positive-infinity (osc::decode-float64 #(127 240 0 0 0 0 0 0))))
  (is (eq :negative-infinity (osc::decode-float64 #(255 240 0 0 0 0 0 0))))
  (is (eq :not-a-number (osc::decode-float64 #(255 255 255 255 0 0 0 0))))
  (is (eq :not-a-number (osc::decode-float64 #(255 248 0 0 0 0 0 0)))))

;; float32/64 NaN and infinity encode (SBCL specific, as ieee-floats only
;; decodes to the :POSITIVE-INFINITY/:NEGATIVE-INFINITY/:NOT-A-NUMBER keywords)
;;   see also -> https://github.com/Shinmera/float-features/
#+sbcl
(test float-special-values
  "Encode NaN and +/-infinity."
  (is (equalp
       (osc::encode-float32 (sb-kernel:make-single-float #x7FC00000))
       #(127 192 0 0)))
  (is (equalp
       (osc::encode-float32 (sb-kernel:make-single-float #x7FC00000))
       (osc::encode-float32 (sb-kernel:make-single-float #x7FC00000))))
  (is (equalp
       (osc::encode-float32 sb-ext:single-float-positive-infinity)
       #(127 128 0 0)))
  (is (equalp
       (osc::encode-float32 sb-ext:single-float-negative-infinity)
       #(255 128 0 0))))

;; single-float

(test float-features
  #+sbcl (pass
          (format nil "SBCL floating point modes: ~A~%" (sb-int:get-floating-point-modes))))

(defun f32b (s) (write-to-string (osc::encode-float32 s ) :base 2))
(defun f64b (s) (write-to-string (osc::encode-float64 s ) :base 2))

(test single-float
  "Various single floats of interest."
  (is (equalp
       (f32b 0.000000059604645s0) "#(110011 10000000 0 0)"))
  (is (equalp
       (f32b 0.000060975552s0) "#(111000 1111111 11000000 0)")))

;; Edge cases
;;  - float32: exact for 0, +/-1; approx for inexact float32 values
;;  - float64: byte-exact round-trips for normals and subnormals

(test f32-edge-cases
  "Various single floats of interest."
  (dolist (case `((0.0 "0.0") (-0.0 "-0.0") (1.0 "1.0") (-1.0 "-1.0")
                  (,(coerce pi 'single-float) "pi")
                  (,most-positive-single-float "FLT_MAX")
                  (,(coerce -3.4028234663852886d38 'single-float) "-FLT_MAX")
                  (,+float32-min+ "FLT_MIN")
                  (,(float (- +float32-min+) 1.0f0) "-FLT_MIN")
                  (1.00001 "1.00001")
                  (-2.3694278e33 "neg-large")
                  (,(coerce 3.4028234663852886d38 'single-float) "~FLT_MAX")))
    (destructuring-bind (n label) case
      (let ((dec (osc::decode-float32 (osc::encode-float32 n))))
        (is (f32= dec n)
            (format nil "float32 edge ~a: ~a got ~a" label n dec))))))

(test f64-edge-cases
  "Various double floats of interest."
  (let ((max-normal most-positive-double-float)
        (max-subnormal (expt 2.0d0 1022))
        (min-positive-denormal least-positive-double-float))
    (dolist (case `((0.0d0 "0.0") (-0.0d0 "-0.0") (1.0d0 "1.0") (-1.0d0 "-1.0")
                    (23.1d0 "23.1") (,pi "pi") (,(exp 1.0d0) "e")
                    (,max-normal "DBL_MAX") (,(float (- max-normal) 1.0d0) "-DBL_MAX")
                    (,max-subnormal "2^1022") (,(float (- max-subnormal) 1.0d0) "-2^1022")
                    (,min-positive-denormal "min-denormal")
                    (,(float (- min-positive-denormal) 1.0d0) "-min-denormal")
                    (1.0d10 "1e10") (1.0d-10 "1e-10")
                    (,(coerce 1.7976931348623157d308 'double-float) "~DBL_MAX")
                    (,(coerce 4.49423283715579d307 'double-float) "~2^1022")))
      (destructuring-bind (n label) case
        (let ((dec (osc::decode-float64 (osc::encode-float64 n))))
          (is (= dec n)
              (format nil "float64 edge ~a: ~a got ~a" label n dec)))))))


;; Messages tagged T, F, N, I, without data

(in-suite message-encoding)

;; messages

(test osc-message-1
  "OSC message encoding tests. address and single int."
  :suite 'message-encoding
  (is (equalp
       '("/test/int" -1)
       (osc:decode-message
        #(47 116 101 115 116 47 105 110 116 0 0 0 44 105 0 0 255 255 255 255)))))


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

;; symmetrical? how much of a issue is this?
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

;; OSC 1.1 keywords and optional typetags. (see: osc-v1.1-compatability.janet)
;; lisp nil doesn't distinguish false from nil, so the :true/:false/:null/:impulse
;; keywords are used. they decode back to t, nil, nil and :impulse respectively.

(test osc-tags
  "OSC message encoding test. T, F, N and I typetags."
  (is (equalp
       (osc:encode-message "/tags" :true :false :null :impulse)
       #(47 116 97 103 115 0 0 0 44 84 70 78 73 0 0 0)))
  (is (equalp
       (osc:decode-message
        #(47 116 97 103 115 0 0 0 44 84 70 78 73 0 0 0))
       '("/tags" t nil nil :impulse))))

(test osc-v1.1-message-decode
  "OSC message decoding test. all optional v1.1 typetags."
  (let ((message (osc::cat
                  #(47 120 0 0)                  ;; "/x"
                  #(44 99 104 100 116 83 99 114 109 84 70 78 73 0 0 0) ;; ",chdtScrmTFNI"
                  (osc::encode-char 65)
                  (osc::encode-int64 -1)
                  (osc::encode-float64 23.1d0)
                  (osc::encode-timetag :now)
                  (osc::encode-string "hi")
                  (osc::encode-char 97)
                  (osc::encode-rgba 64 128 192 255)
                  (osc::encode-midi 1 144 60 127))))
    (is (equalp
         (osc:decode-message message)
         '("/x" 65 -1 23.1d0 (0 1) "hi" 97 (64 128 192 255) (1 144 60 127)
           t nil nil :impulse)))))

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

;; Bundles (see: osc-v1.1-compatability.janet)
;; note: cl-osc decode-bundle returns the timetag followed by the elements
;; in reverse order of appearance.
(test v1.1-bundle-encoding
  "OSC bundle encoding & decoding round-trip test."
  (let* ((bundle (osc:encode-bundle
                  '(("/voices/0/tm/start" 0.0)
                    ("/foo/stringmessage" "a" "few" "strings")
                    ("/documentation/all-messages"))
                  :now))
         (decoded (osc:decode-bundle bundle)))
    (is (equalp #(0 0 0 0 0 0 0 1) (car decoded)) "bundle timetag")
    (is (= 3 (length (cdr decoded))) "bundle 3 elements")
    (is (equalp '("/documentation/all-messages")
                (cadr decoded)) "bundle element 0")
    (is (equalp '("/foo/stringmessage" "a" "few" "strings")
                (caddr decoded)) "bundle element 1")
    (is (equalp '("/voices/0/tm/start" 0.0)
                (cadddr decoded)) "bundle element 2")))

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

;; Round-trip numerics fuzzing
(in-suite fuzz-numerics)

(test int32-fuzz
  "int32 round-trip fuzzing."
  (loop repeat 10000
        for n = (- (rand-i32) #x3FFFFFFF)
        do (is (= (osc::decode-int32 (osc::encode-int32 n)) n)
                (format nil "int32 round-trip ~a" n)))
  (dolist (n (list (- (expt 2 31)) (1- (expt 2 31)) 0 1 -1
                   16843009 -16843010 #x01010101))
    (is (= (osc::decode-int32 (osc::encode-int32 n)) n)
        (format nil "int32 edge ~a" n))))

(test uint32-fuzz
  "uint32 round-trip fuzzing."
  (loop repeat 10000
        for n = (rand-i32)
        do (is (= (osc::decode-uint32 (osc::encode-int32 n)) n)
                (format nil "uint32 round-trip ~a" n))))

(test int64-fuzz
  "int64/uint64 round-trip fuzzing."
  (loop repeat 5000
        for n = (- (mod (* (rand-i32) (rand-i32)) 9007199254740991)
                   4503599627370495)
        do (is (= (osc::decode-int64 (osc::encode-int64 n)) n)
                (format nil "int64 round-trip ~a" n)))
  (loop repeat 5000
        for n = (rand-range 0 9007199254740991)
        do (is (= (osc::decode-uint64 (osc::encode-int64 n)) n)
                (format nil "uint64 round-trip ~a" n)))
  (dolist (n (list 0 1 -1 9007199254740991 -9007199254740991
                   858993459200 -858993459200 16843009 -16843009
                   (1- (expt 2 31)) (- (expt 2 31))))
    (is (= (osc::decode-int64 (osc::encode-int64 n)) n)
        (format nil "int64 edge ~a" n))))

(test float32-fuzz
  "float32 round-trip fuzzing."
  (loop repeat 10000
        for sign = (if (zerop (logand (rand-i32) 1)) 1 -1)
        for exp = (rand-range -126 128)
        for mant = (rand-f32)
        for f = (* sign mant (expt 2.0d0 exp))
        when (<= +float32-min+ (abs f) +float32-max+)
        do (let* ((f32 (coerce f 'single-float))
                  (dec (osc::decode-float32 (osc::encode-float32 f32))))
             (is (f32= dec f32)
                 (format nil "float32 round-trip ~a got ~a" f dec)))))

(test float64-fuzz
  "float64 round-trip fuzzing."
  (loop repeat 10000
        for sign = (if (zerop (logand (rand-i32) 1)) 1 -1)
        for exp = (rand-range -1022 1024)
        for mant = (rand-f32)
        for f = (* sign mant (expt 2.0d0 exp))
        do (let ((dec (osc::decode-float64 (osc::encode-float64 f))))
             (is (f64= dec f)
                 (format nil "float64 round-trip ~a got ~a" f dec)))))

(test message-fuzz
  "message round-trip fuzzing."
  (loop repeat 2000
        for addr = (format nil "/test/~d" (rand-i32))
        for n-args = (rand-range 1 8)
        for args = (loop repeat n-args
                         for tag = (rand-range 0 5)
                         collect (case tag
                                   (0 (logand (rand-i32) #x7FFFFFFF))
                                   (1 (coerce (rand-f32) 'single-float))
                                   (2 (format nil "s~d" (rand-i32)))
                                   (3 (coerce (* (rand-f32) (expt 2.0d0 (rand-range -10 10)))
                                              'single-float))
                                   (4 (- (logand (rand-i32) #x3FFFFFFF) #x3FFFFFFF))))
        do (let ((dec (osc:decode-message (apply #'osc:encode-message addr args))))
             (is (string= addr (car dec)) "message address")
             (is (= (length args) (length (cdr dec))) "message arg count")
             (is (equalp (cdr dec) args) "message args"))))

(test deterministic-encoding
  "Encoding the same value twice gives identical bytes."
  (loop repeat 1000
        for n = (rand-i32)
        for f = (coerce (* (rand-f32) (expt 2.0d0 (rand-range -10 10))) 'single-float)
        do (is (equalp (osc::encode-int32 n) (osc::encode-int32 n))
                (format nil "deterministic int32 ~a" n))
           (is (equalp (osc::encode-float32 f) (osc::encode-float32 f))
                (format nil "deterministic float32 ~a" f))))

(run! 'synchroscope)
