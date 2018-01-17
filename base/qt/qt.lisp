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
