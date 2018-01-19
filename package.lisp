;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

;; TODO fix this, it's so damn outdated
(uiop:define-package #:fox5
  (:use
   #:cl
   #:fox5/fox5
   #:fox5/gif-fox5)
  (:shadowing-import-from
   #:cl
   #:ratio)
  (:reexport
   #:fox5/fox5
   #:fox5/gif-fox5))
