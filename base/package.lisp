;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:fox5/base
  (:use
   #:cl
   #:phoe-toolbox
   #:alexandria
   #:cl-furcadia/constants)
  (:export
   #:image #:image-8bit #:image-32bit
   #:width #:height #:data #:remappablep))
