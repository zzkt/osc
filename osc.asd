;; -*- mode: lisp -*-

(in-package #:cl-user)

(asdf:defsystem "osc"
  :author "nik gaffney <nik@fo.am>"
  :licence "GPL v3"
  :description "The Open Sound Control protocol, aka OSC"
  :version "1.0.0"
  :components ((:file "osc")))
