;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; base.lisp

(in-package :fox5/base)

(defclass image ()
  ((%width :accessor width
           :initarg :width)
   (%height :accessor height
            :initarg :height)
   (%data :accessor data
          :initarg :data)))
