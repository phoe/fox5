;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(uiop:define-package #:fox5
    (:use
     #:cl
     #:fox5/base
     #:fox5/gif-fox5)
  (:shadowing-import-from
   #:cl
   #:ratio)
  (:reexport
   #:fox5/base
   #:fox5/gif-fox5))
