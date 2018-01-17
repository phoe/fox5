;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; qt.lisp

(in-package :fox5/base/qt)
(in-readtable :qtools)

(defun display (image)
  "Internal debugging function used for simple displaying of Furcadia images."
  (with-main-window (qlabel (q+:make-qlabel))
    (setf (q+:pixmap qlabel) (image-qpixmap image))))

(defun image-qpixmap (image)
  (let* ((data (data image))
         (length (array-total-size data)))
    (with-static-vector (vector length :initial-contents data)
      (with-finalizing ((qimage (qimage-from image vector)))
        (with-finalizing ((mirrored (q+:mirrored qimage)))
          (q+:qpixmap-from-image mirrored))))))

(defun qimage-from (image vector)
  (q+:make-qimage (static-vector-pointer vector)
                  (width image) (height image)
                  (q+:qimage.format_argb32)))
