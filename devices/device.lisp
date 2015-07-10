(cl:in-package #:osc)

;;;=====================================================================
;;; OSC device base class
;;;=====================================================================

(defclass osc-device ()
  ((socket
    :reader socket
    :writer set-socket
    :initform nil)
   (debug-mode
    :reader debug-mode
    :writer set-debug-mode
    :initarg :debug-mode)
   (cleanup-fun
    :reader cleanup-fun
    :initarg :cleanup-fun
    :initform nil)))

;;;=====================================================================
;;; OSC device mixin classes
;;;=====================================================================

(defclass udp-device (osc-device) ())

(defclass tcp-device (osc-device) ())

(defclass listening-device (osc-device)
  ((listening-thread
    :reader listening-thread
    :writer set-listening-thread
    :initform nil)))

(defclass receiving-device (listening-device)
  ((socket-buffer
    :reader socket-buffer
    :initarg :socket-buffer
    :initform (make-socket-buffer))))

(defclass dispatching-device (listening-device)
  ((address-tree
    :reader address-tree
    :initarg :address-tree
    :initform (make-osc-tree))))

(defclass dispatching-device-udp (dispatching-device receiving-device
                                                     udp-device) ())


;;;=====================================================================
;;; OSC device abstract classes
;;;=====================================================================

(defclass osc-transmitter (osc-device) ())

(defclass osc-client (dispatching-device receiving-device
                                         osc-transmitter) ())

(defclass osc-server (dispatching-device osc-transmitter)
  ((buffer-size
    :reader buffer-size
    :initarg :buffer-size)
   (clients
    :reader clients
    :initarg :clients
    :initform (make-clients-hash))))

(defclass osc-client-endpoint (osc-client)
  ((clients
    :reader clients
    :initarg :clients)))


;;;=====================================================================
;;; OSC device concrete classes
;;;=====================================================================

(defclass osc-transmitter-udp (osc-transmitter udp-device) ())

(defclass osc-client-udp (osc-client dispatching-device-udp) ())

(defclass osc-client-tcp (osc-client tcp-device) ())

(defclass osc-server-udp (osc-server dispatching-device-udp
                                     osc-transmitter-udp) ())

(defclass osc-server-tcp (osc-server osc-transmitter tcp-device) ())

(defclass osc-client-endpoint-tcp (osc-client-endpoint
                                   osc-client-tcp) ())


;;;=====================================================================
;;; Device generic functions
;;;=====================================================================

(defgeneric protocol (osc-device)
  (:method ((osc-device udp-device))
    :udp)
  (:method ((osc-device tcp-device))
    :tcp))

(defgeneric name (osc-device)
  (:method ((osc-device osc-device))
    (concatenate 'string
                 (symbol-name (class-name (class-of osc-device)))
                 "-"
                 (make-name-string osc-device))))

(defmethod buffer-size ((osc-device dispatching-device))
  (length (socket-buffer osc-device)))

(defgeneric quit (osc-device))

(defgeneric osc-device-cleanup (device)
  (:method :before ((osc-device osc-device))
           (when (cleanup-fun osc-device)
             (funcall (cleanup-fun osc-device) osc-device)))
  (:method ((osc-device osc-device))
    (when (debug-mode osc-device)
      (format t "~%OSC device stopped: ~A~%"
              (name osc-device)))
    (when (socket osc-device)
      (handler-case
          (socket-close (socket osc-device) :abort t)
        (sb-int:simple-stream-error ()
          (when (debug-mode osc-device)
            (warn "Device ~A gone away." (name osc-device)))))
      (set-socket nil osc-device))))
