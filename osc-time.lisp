(in-package #:osc)

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))
(defconstant +2^32+ (expt 2 32))
(defconstant +2^32/million+ (/ +2^32+ (expt 10 6)))
(defconstant +usecs+ (expt 10 6))

(deftype timetag () '(unsigned-byte 64))

(defun timetagp (object)
  (typep object 'timetag))

(defun unix-secs+usecs->timetag (secs usecs)
  (let ((sec-offset (+ secs +unix-epoch+))) ; Seconds from 1900.
    (setf sec-offset (ash sec-offset 32))   ; Make seconds the top 32
                                            ; bits.
    (let ((usec-offset
           (round (* usecs +2^32/MILLION+)))) ; Fractional part.
      (the timetag (+ sec-offset usec-offset)))))

(defun get-current-timetag ()
  "Returns a fixed-point 64 bit NTP-style timetag, where the top 32
bits represent seconds since midnight 19000101, and the bottom 32 bits
represent the fractional parts of a second."
  #+sbcl (multiple-value-bind (secs usecs)
             (sb-ext:get-time-of-day)
           (the timetag (unix-secs+usecs->timetag secs usecs)))
  #-sbcl (error "Can't encode timetags using this implementation."))

(defun timetag+ (original seconds-offset)
  (declare (type timetag original))
  (let ((offset (round (* seconds-offset +2^32+))))
    (the timetag (+ original offset))))


;;;=====================================================================
;;; Functions for using double-float unix timestamps.
;;;=====================================================================

(defun get-unix-time ()
  "Returns a a double-float representing real-time now in seconds,
with microsecond precision, relative to 19700101."
  #+sbcl (multiple-value-bind (secs usecs)
             (sb-ext:get-time-of-day)
           (the double-float (+ secs (microseconds->subsecs usecs))))
  #-sbcl (error "Can't encode timetags using this implementation."))

(defun unix-time->timetag (unix-time)
  (multiple-value-bind (secs subsecs)
      (floor unix-time)
    (the timetag
         (unix-secs+usecs->timetag secs
                                   (subsecs->microseconds subsecs)))))

(defun timetag->unix-time (timetag)
  (if (= timetag 1)
      1                                 ; immediate timetag
      (let* ((secs (ash timetag -32))
             (subsec-int32 (- timetag (ash secs 32))))
        (the double-float (+ (- secs +unix-epoch+)
                             (int32->subsecs subsec-int32))))))

(defun microseconds->subsecs (usecs)
  (declare (type (integer 0 1000000) usecs))
  (coerce (/ usecs  +usecs+) 'double-float))

(defun subsecs->microseconds (subsecs)
  (declare (type (float 0 1) subsecs))
  (round (* subsecs +usecs+)))

(defun int32->subsecs (int32)
  "This maps a 32 bit integer, representing subsecond time, to a
double float in the range 0-1."
  (declare (type (unsigned-byte 32) int32))
  (coerce (/ int32 +2^32+) 'double-float))

(defun print-as-double (time)
  (format t "~%~F" (coerce time 'double-float))
  time)
