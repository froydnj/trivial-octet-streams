; -*- mode:lisp; indent-tabs-mode: nil -*-

(cl:defpackage :trivial-octet-streams-system
  (:use :cl))

(cl:in-package :trivial-octet-streams-system)

(asdf:defsystem :trivial-octet-streams
  :version "0.1"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library for octet input and output streams analogous to string streams."
  :components ((:static-file "README")
               (:static-file "LICENSE")
               (:file "package")
               (:file "octet-streams" :depends-on ("package"))))
