;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:fox5/fs2
  (:use
   #:cl
   #:fast-io
   #:phoe-toolbox
   #:alexandria
   #:cl-furcadia/remap
   #:fox5/base)
  (:export
   #:file #:shape #:read-fs2))
