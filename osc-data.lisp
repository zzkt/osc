(cl:in-package #:osc)

;; Classes

(defclass osc-data () ())

(defclass message (osc-data)
  ((command
    :reader command
    :initarg :command)
   (args
    :reader args
    :initarg :args
    :initform nil)))

(defclass bundle (osc-data)
  ((timetag
    :reader timetag
    :initarg :timetag
    :initform :now)
   (elements
    :reader elements
    :initarg :elements
    :initform nil)))

;; Constructors

(defun make-message (command &rest args)
  (make-instance 'message
                 :command command
                 :args args))

(defun make-bundle (timetag &rest elements)
  (make-instance 'bundle
                 :timetag timetag
                 :elements elements))

(defgeneric format-osc-data (data &optional stream))

(defmethod format-osc-data ((message message) &optional (stream t))
  (format stream "~a~{ ~a~}~%"
          (command message)
          (args message)))

(defmethod format-osc-data ((bundle bundle) &optional (stream t))
  (format stream "~&[ ~a~%" (timetag bundle))
  (dolist (element (elements bundle))
    (format-osc-data element stream))
  (format stream "~&]~%"))
