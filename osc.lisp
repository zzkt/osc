;; -*- mode: lisp -*-
;;
;; an implementation of the OSC (Open Sound Control) protocol
;;
;; copyright (C) 2004 FoAM vzw. 
;;
;; You are granted the rights to distribute and use this software
;; under the terms of the Lisp Lesser GNU Public License, known 
;; as the LLGPL. The LLGPL consists of a preamble and the LGPL. 
;; Where these conflict, the preamble takes precedence. The LLGPL
;; is available online at http://opensource.franz.com/preamble.html 
;; and is distributed with this code (see: LICENCE and LGPL files)
;; 
;; authors 
;;
;;  nik gaffney <nik@f0.am>
;;
;; requirements
;;
;;  dependent on sbcl for float encoding, other suggestions welcome.
;;
;; commentary
;;
;;  this is a partial implementation of the OSC protocol which is used
;;  for communication mostly amongst music programs and their attatched
;;  musicians. eg. sc3, max/pd, reaktor/traktorska etc+. more details 
;;  of the protocol can be found at the open sound control pages -=> 
;;                     http://www.cnmat.berkeley.edu/OpenSoundControl/
;; 
;;  - currently doesnt send timetags, but does send typetags
;;  - will most likely crash if the input is malformed
;;  - int32 en/de-coding based on code (c) Walter C. Pelissero
  
;; to do
;;
;;  - liblo like network wrapping
;;  - error handling
;;  - receiver -> osc-responder.lisp
;;  - osc-tree as name.value alist for responder/serve-event
;;  - portable en/decoding of floats -=> ieee754 tests
;;  - bundles, blobs, doubles and other typetags
;;  - asdf-installable

;; known BUGS
;;  - only unknown for now.. .

;; changes
;;
;;  2004-12-18 
;;    - initial version, single args only
;;  2005-01-24 
;;    - sends and receives multiple arguments
;;    - tests in osc-test.lisp
;;  2005-01-26 
;;    - fixed string handling bug
;;  2005-02-08  
;;    - in-package'd 

(defpackage :osc
  (:use :cl)
  (:documentation "OSC aka the 'open sound control' protocol")
  (:export :encode-message
	   :decode-message))

(in-package :osc)

;;;;;; ;    ;;    ;     ; ;     ; ; ;         ;
;; 
;;   eNcoding OSC messages
;;
;;; ;;  ;;   ; ; ;;           ;      ;  ;                  ;

(defun encode-message (address &rest data)
  "encodes an osc message with the given address and data."
  (concatenate '(vector '(unsigned-byte 8))
	       (encode-address address)
	       (encode-typetags data)
	       (encode-data data)))

(defun encode-address (address)
  (cat (map 'vector #'char-code address) 
       (pad-string address)))

(defun encode-typetags (data)
  "creates a typetag string suitable for the given data.
  valid typetags according to the osc spec are ,i ,f ,s and ,b
  non-std extensions include ,{h|t|d|S|c|r|m|T|F|N|I|[|]}
                             see the spec for more details. ..

  NOTE: currently handles the following tags only 
   i => #(105) => int32
   f => #(102) => float
   s => #(115) => string"

  (let ((lump (make-array 0 :adjustable t :fill-pointer t)))
    (vector-push-extend (char-code #\,) lump) ; typetag begins with ","
    (dolist (x data) 
      (typecase x
	(integer
	 (vector-push-extend (char-code #\i) lump))
	(float 
	 (vector-push-extend (char-code #\f) lump))
	(simple-string 
	 (vector-push-extend (char-code #\s) lump))
	(t 
	 (error "can only encode ints, floats or string"))))
    (cat lump
	 (pad (padding-length (length lump))))))      
		  
(defun encode-data (data)
  "encodes data in a format suitable for an OSC message"
  (let ((lump (make-array 0 :adjustable t :fill-pointer t)))
    (dolist (x data) 
      (typecase x
	(integer
	 (setf lump (cat lump (encode-int32 x)))) 
	(float 
	 (setf lump (cat lump (encode-float32 x)))) 
	(simple-string 
	 (setf lump (cat lump (encode-string x)))) 
	(t 
	 (error "wrong type. turn back"))))
    lump))

(defun encode-string (string)
  (cat (map 'vector #'char-code string) 
       (pad-string string)))


;;;;;; ;    ;;    ;     ; ;     ; ; ;         ;
;; 
;;    decoding OSC messages
;;
;;; ;;    ;;     ; ;     ;      ;      ; ;

(defun decode-message (message)
  "reduces an osc message to an (address . data) pair. .."
  (declare (omptimize debug 3)) 
  (let ((x (position (char-code #\,) message)))
    (cons (decode-address (subseq message 0 x))
	  (decode-taged-data (subseq message x)))))

(defun decode-address (address)
  (coerce (map 'vector #'code-char address) 'string))
 
(defun decode-taged-data (data)
  "decodes data encoded with typetags...

  NOTE: currently handles the following tags only 
   i => #(105) => int32
   f => #(102) => float
   s => #(115) => string"

  (setf div (position 0 data))
  (let ((tags (subseq data 1 div)) 
	(chunks (subseq data (osc-string-length (subseq data 0 div))))
        (acc '())
	(result '()))
    (setf acc chunks)
    (map 'vector
	 #'(lambda (x)
	     (cond
	      ((eq x (char-code #\i)) 
	       (push (decode-int32 (subseq acc 0 4)) 
		     result)
	       (setf acc (subseq acc 4)))
	      ((eq x (char-code #\f))
	       (push (decode-float32  (subseq acc 0 4)) 
		     result)
	       (setf acc (subseq acc 4)))
	      ((eq x (char-code #\s))
	       (let ((pointer (+ (padding-length (position 0 acc))
				 (position 0 acc))))
		 (push (decode-string 
			(subseq acc 0 pointer))
		       result)
		 (setf acc (subseq acc pointer))))
	      ((eq x (char-code #\b)) (decode-blob x))
	      (t  (error "unrecognised typetag"))))
	 tags)
    (nreverse result)))
	
(defun split-data (data)
  "splits incoming data into the relevant unpadded chunks, ready for conversion .. ."
  (loop for i = 0 then (1+ j)
	as j = (position #\0 string :start i)
	collect (subseq string i j)
	while j))

;; dataformat en- de- cetera.
 
(defun encode-float32 (f)
  "encode an ieee754 float as a 4 byte vector. currently sbcl specifc"
  #+sbcl (encode-int32 (sb-kernel:single-float-bits f))
  #-sbcl (error "cant encode floats using this implementation"))

(defun decode-float32 (s)
  "ieee754 float from a vector of 4 bytes in network byte order"
  #+sbcl (sb-kernel:make-single-float (decode-int32 s))
  #-sbcl (error "cant decode floats using this implementation"))

(defun decode-int32 (s)
  "4 byte > 32 bit int > two's compliment (in network byte order)"
  (let ((i (+ (ash (elt s 0) 24)
	      (ash (elt s 1) 16)
	      (ash (elt s 2) 8)
	      (elt s 3))))
    (if (>= i #x7fffffff)
        (- 0 (- #x100000000 i))
      i)))

(defun encode-int32 (i)
  "convert integer into a sequence of 4 bytes in network byte order."
  (declare (type integer s))
  (let ((buf (make-sequence '(vector (unsigned-byte 8)) 4)))
    (macrolet ((set-byte (n)
			 `(setf (elt buf ,n)
				(logand #xff (ash i ,(* 8 (- n 3)))))))
      (set-byte 0)
      (set-byte 1)
      (set-byte 2)
      (set-byte 3))
    buf))

(defun decode-string (data)
  "converts a binary vector to a string and removes trailing #\nul characters"
  (string-trim '(#\nul) (coerce (map 'vector #'code-char data) 'string)))

(defun encode-string (string)
  "encodes a string as a vector of character-codes, padded to 4 byte boundary"
  (cat (map 'vector #'char-code string) 
       (pad-string string)))

(defun decode-blob (b)
  (error "cant decode blobs for now. .."))

(defun encode-blob (b)
  (error "cant encode blobs for now. .."))


;; utility functions for OSC slonking

(defun cat (&rest catatac)
  (apply #'concatenate '(vector *) catatac))

(defun osc-string-length (string)
  "determines the length required for a padded osc string"
  (let ((n (length string)))
    (+ n (padding-length n))))

(defun padding-length (s)
  "returns the length of padding required for a given length of string"
  (- 4 (mod s 4)))

(defun pad-string (string)
  "returns the padding required for a given osc string"
  (pad (- 4 (mod (length string) 4))))

(defun pad (n)
  "make a sequence of the required number of #\Nul characters"
  (make-array n :initial-element 0 :fill-pointer n))

;; end