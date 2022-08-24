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

(defun make-message (command args)
  (unless (listp args)
    (setf args (list args)))
  (make-instance 'message
                 :command command
                 :args args))

(defun message (command &rest args)
  (make-message command args))

(defun make-bundle (timetag elements)
  (unless (listp elements)
    (setf elements (list elements)))
  (make-instance 'bundle
                 :timetag timetag
                 :elements elements))

(defun bundle (timetag &rest elements)
  (make-bundle timetag elements))

(defgeneric format-osc-data (data &key stream width))

(defmethod format-osc-data ((message message) &key (stream t)
                                                (width 80))
  (let ((args-string (format nil "~{~a~^ ~}" (args message))))
    (when (> (length args-string) width)
      (setf args-string
            (concatenate 'string
                         (subseq args-string 0 width)
                         "...")))
    (format stream "~a ~a~%"
            (command message)
            args-string)))

(defmethod format-osc-data ((bundle bundle) &key (stream t) (width 80))
  (format stream "~&[ ~a~%" (timetag bundle))
  (dolist (element (elements bundle))
    (format-osc-data element :stream stream :width width))
  (format stream "~&]~%"))
