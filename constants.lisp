;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; constants.lisp

(in-package :fox5)

(defvar *fox5-header-magic*
  #.(coerce '(70 79 88 53 46 49 46 49) 'octet-vector))

(defvar *fox5-header-magic-mismatch*
  #.(format nil "FOX5 magic string mismatch.~%Expected: ~~S~%Got:      ~~S"))
