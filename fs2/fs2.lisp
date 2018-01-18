;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fsh2.lisp

(in-package #:fox5/fs2)

(defclass file ()
  ((%shapes :accessor shapes
            :initform '())))

(defclass shape (image)
  ((%position-x :accessor position-x)
   (%position-y :accessor position-y)
   (%replacement :accessor replacement)))

(defun read-fs2 (pathname)
  "Reads the provided FS2 file from the given pathname and returns its parsed
representation."
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (with-input-from-binary (stream pathname)
    (with-fast-input (buffer nil stream)
      (let ((magic (readu64-be buffer)))
        (assert (= magic #x465348322E303031) () "Not a FSH2 file: ~S" pathname))
      (let* ((instance (make-instance 'file))
             (nshapes (readu32-le buffer))
             (flags (readu32-be buffer)))
        (assert (= flags 0) () "Encrypted FSH2 file: ~S" pathname)
        (loop repeat nshapes
              for shape = (make-instance 'shape)
              do (setf (width shape) (readu8 buffer)
                       (height shape) (readu8 buffer)
                       (position-x shape) (read8 buffer)
                       (position-y shape) (read8 buffer)
                       (replacement shape) (readu16-le buffer) ;; TODO check
                       (data shape)
                       (make-octet-vector (* (width shape) (height shape))))
                 (fast-read-sequence (data shape) buffer)
                 (setf (data shape) (8bit-32bit (data shape)))
                 (push shape (shapes instance))
              finally (nreversef (shapes instance)))
        instance))))
