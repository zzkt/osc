;; -*- mode: lisp -*-

(in-package #:common-lisp-user)

(asdf:defsystem osc
    :name "osc"
    :author "nik gaffney <nik@fo.am>"
    :licence "LLGPL"
    :description "The Open Sound Control protocol, aka OSC"
    :version "0.5"
    :components ((:file "osc" :depends-on ("osc-time"))
		 (:file "osc-dispatch" :depends-on ("osc"))
		 (:file "osc-time" :depends-on ("package"))
		 (:file "package")))
