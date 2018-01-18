;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:fox5/fsh
  (:use
   #:cl
   #:fast-io
   #:phoe-toolbox
   #:alexandria)
  (:export
   #:file #:shape #:read-fsh))
