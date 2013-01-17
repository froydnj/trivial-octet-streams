; -*- mode:lisp; indent-tabs-mode: nil -*-

(cl:defpackage :trivial-octet-streams
  (:use :cl)
  (:export #:make-octet-input-stream #:make-octet-output-stream
           #:get-output-stream-octets))
