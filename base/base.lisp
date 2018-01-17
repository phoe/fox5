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
          :initarg :data)
   (%remappablep :accessor remappablep
                 :initarg :remappablep
                 :initform nil)))

(defun 8bit-32bit-no-remap (vector)
  (let* ((length (array-total-size vector))
         (result (make-array (* 4 length))))
    (loop for i from 0 below length
          for color = (aref vector i)
          unless (= color 0)
            do (setf (aref result (+ 0 (* 4 i))) ;; B
                     (aref *classic-palette* (+ 2 (* 4 color)))
                     (aref result (+ 1 (* 4 i))) ;; G
                     (aref *classic-palette* (+ 1 (* 4 color)))
                     (aref result (+ 2 (* 4 i))) ;; R
                     (aref *classic-palette* (+ 0 (* 4 color)))
                     (aref result (+ 3 (* 4 i))) ;; A
                     (aref *classic-palette* (+ 3 (* 4 color))))
          finally (return result))))
