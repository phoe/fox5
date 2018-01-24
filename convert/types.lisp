;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; types.lisp

(in-package :fox5)

;; TODO add TRADEMARK function that puts raptor stamps on file

(defgeneric specialize (file type)
  (:documentation "Specialized all objects and shapes in the provided FOX5 file
into a concrete Edit Type.
\
This function is suitable for use on FOX5 files converted from legacy formats,
such as FSH, FS2, FOX1, GIF. This function may merge multiple objects into one
as a part of its work."))

(defmethod specialize ((file file) (type (eql :avatar)))
  )

(defmethod specialize ((file file) (type (eql :portrait)))
  )

(defmethod specialize ((file file) (type (eql :item)))
  )

(defmethod specialize ((file file) (type (eql :wall)))
  )

(defmethod specialize ((file file) (type (eql :floor)))
  )

(defmethod specialize ((file file) (type (eql :effect)))
  )
