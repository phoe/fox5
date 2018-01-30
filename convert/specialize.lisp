;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; types.lisp

(in-package :fox5)

(defgeneric specialize (file type &key)
  (:documentation "Specialized all objects and shapes in the provided FOX5 file
into a concrete Edit Type.
\
This function is suitable for use on FOX5 files converted from legacy formats,
such as FSH, FS2, FOX1, GIF. This function may merge multiple objects into one
as a part of its work."))

(defparameter %*specialize-avatar-subtypes*
  '((:sw :sit) (:sw :walk-right) (:sw :walk) (:sw :walk-left)
    (:se :sit) (:se :walk-right) (:se :walk) (:se :walk-left)
    (:nw :sit) (:nw :walk-right) (:nw :walk) (:nw :walk-left)
    (:ne :sit) (:ne :walk-right) (:ne :walk) (:ne :walk-left)
    (:sw :lie) (:se :lie) (:nw :lie) (:ne :lie)))

(defmethod specialize ((file file) (type (eql :player)) &key swap-lying-p)
  (flet ((g (direction state) `(:avatar :avatar nil ,direction :small ,state)))
    (let ((all-shapes (cdr (mappend #'children (children file))))
          (shape-types (mapcar (curry #'apply #'g)
                               %*specialize-avatar-subtypes*)))
      (loop for list-start = all-shapes then (nthcdr 20 list-start)
            for object in (cdr (children file))
            for shapes = (robust-subseq list-start 0 20)
            when (/= 20 (length shapes))
              do (loop-finish)
            do (mapc #'(setf shape-type) shape-types shapes)
               (setf (children object) '())
               (mapc (curry #'parent-push object) shapes)
            when swap-lying-p
              do (let ((list (last shapes 2)))
                   (rotatef (shape-type (first list))
                            (shape-type (second list))))
            collect object into objects
            finally (setf (children file) objects))
      file)))

(defmethod specialize ((file file) (type (eql :avatar)) &key swap-lying-p)
  (flet ((g (direction state) `(:avatar :avatar nil ,direction :small ,state)))
    (let ((shapes (mappend #'children (children file)))
          (shape-types (mapcar (curry #'apply #'g)
                               %*specialize-avatar-subtypes*)))
      (mapc #'(setf shape-type) shape-types shapes)
      (let ((object (first (children file))))
        (setf (children object) '())
        (mapc (curry #'parent-push object) shapes)
        (setf (children file) (list object)))
      (when swap-lying-p
        (let ((list (last shapes 2)))
          (rotatef (shape-type (first list))
                   (shape-type (second list)))))
      file)))

(defmethod specialize ((file file) (type (eql :portrait)) &key)
  (let ((shapes (mappend #'children (children file)))
        (shape-types (loop for i in '(:unspecified :female :male)
                           collect `(:portrait-set :portrait ,i))))
    (mapc #'(setf shape-type) shape-types shapes)
    (let ((object (first (children file))))
      (setf (children object) '())
      (mapc (curry #'parent-push object) shapes)
      (setf (children file) (list object)))
    file))

(defmethod specialize ((file file) (type (eql :wall)) &key)
  (loop for (object-1 object-2) on (children file) by #'cddr
        for shape-1 = (first (children object-1))
        for shape-2 = (first (children object-2))
        collect object-1 into final-objects
        do (setf (shape-type shape-1) '(:wall :wall :right :small))
           (setf (shape-type shape-2) '(:wall :wall :left :small))
           (parent-push object-1 shape-2)
        finally (setf (children file) final-objects))
  file)

(defmethod specialize ((file file) (type (eql :floor)) &key)
  (%specialize file type))

(defmethod specialize ((file file) (type (eql :effect)) &key)
  (%specialize file type))

(defmethod specialize ((file file) (type (eql :item)) &key)
  (%specialize file type))

(defun %specialize (file type)
  (loop for object in (children file)
        for shape = (first (children object))
        do (setf (shape-type shape) (list type type :small)))
  file)
