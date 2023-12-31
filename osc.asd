;; -*- mode: lisp -*-
(in-package :asdf-user)

(defsystem "osc"
  :description "The Open Sound Control protocol, aka OSC"
  :author "nik gaffney <nik@fo.am>"
  :depends-on ("ieee-floats")
  :version "0.9.1"
  :licence "GPL v3"
  :components ((:file "osc"))
  :in-order-to ((test-op (test-op "osc/tests"))))

;; regression testing. can be ignored/disabled at run time if required
(defsystem "osc/tests"
  :description "Tests for OSC library."
  :depends-on ("osc" "fiveam")
  :components ((:file "osc-tests"))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam '#:run! :synchroscope)))
