;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fsh.lisp

(in-package #:fox5/fsh)

(defclass file ()
  ((%shapes :accessor shapes
            :initform '())))

(defclass shape ()
  ((%width :accessor width)
   (%height :accessor height)
   (%position-x :accessor position-x)
   (%position-y :accessor position-y)
   (%image :accessor image)))

;; TODO add FSH2
(defun read-fsh (pathname)
  "Reads the provided FSH file from the given pathname and returns its parsed
representation."
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (with-input-from-binary (stream pathname)
    (with-fast-input (buffer nil stream)
      (let* ((instance (make-instance 'file))
             (nshapes (readu16-le buffer)))
        (loop repeat nshapes do (readu16-le buffer))
        (loop repeat nshapes
              for shape = (make-instance 'shape)
              do (setf (width shape) (readu8 buffer)
                       (height shape) (readu8 buffer)
                       (position-x shape) (read8 buffer)
                       (position-y shape) (read8 buffer)
                       (image shape)
                       (make-octet-vector (* (width shape) (height shape))))
                 (fast-read-sequence (image shape) buffer)
                 (push shape (shapes instance))
              finally (nreversef (shapes instance)))
        instance))))
