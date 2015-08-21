;; -*- mode: lisp -*-

(in-package #:common-lisp-user)

(asdf:defsystem osc
    :name "osc"
    :author "nik gaffney <nik@fo.am>"
    :licence "LLGPL"
    :description "The Open Sound Control protocol, aka OSC"
    :version "0.5"
    :components
    ((:file "osc" :depends-on ("osc-data" "osc-time"))
     (:file "osc-data" :depends-on ("package"))
     (:file "osc-dispatch" :depends-on ("osc"))
     (:file "osc-time" :depends-on ("package"))
     (:file "package")
     (:module "devices"
              :depends-on ("package" "osc-data")
              ::components
              ((:file "socket-functions")
               (:file "device")
               (:file "transmitter"
                      :depends-on ("device"
                                   "socket-functions"))
               (:file "listening-device"
                      :depends-on ("transmitter"))
               (:file "dispatching-device"
                      :depends-on ("listening-device"))
               (:file "client"
                      :depends-on ("dispatching-device"))
               (:file "server" :depends-on ("client"))))))
