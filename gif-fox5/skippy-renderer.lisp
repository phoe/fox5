;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; skippy-renderer.lisp

(defpackage #:skippy/renderer
  (:use #:cl #:skippy)
  (:export #:render))

(in-package :skippy/renderer)

;;; TODO implement "Restore to Previous"

(defun render (data-stream)
  "Given a GIF data stream, returns a rendered image. Three values are returned.
The first value is a list of vectors containing resulting ARGB data in
row-first order and each second element
The second value is a list of integer values for the frame delays in
milliseconds.
The second value is a list of three values: image width, image height and a
generalized boolean signifying if the GIF should loop."
  (loop with color-table = (color-table data-stream)
        with loopingp = (loopingp data-stream)
        with width = (width data-stream)
        with height = (height data-stream)
        with frame = (make-array (* 4 width height)
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)
        for image across (images data-stream)
        for delay-time = (delay-time image)
        do (render-image-to-frame frame width image color-table)
        collect (copy-seq frame) into result
        collect delay-time into delays
        finally (return (values result
                                delays
                                (list width height loopingp)))))

(defun render-image-to-frame (frame frame-width image &optional color-table)
  (let ((disposal-method (disposal-method image)))
    (case disposal-method
      (:restore-background (loop for i below (array-dimension frame 0)
                                 do (setf (aref frame i) 0)))
      (:restore-previous (error "Not implemented yet." #| TODO |#))))
  (loop with color-table = (or (color-table image) color-table)
        with t-index = (transparency-index image)
        with width = (width image)
        with height = (height image)
        with top = (top-position image)
        with left = (left-position image)
        with data = (image-data image)
        for y from 0 below height
        do (loop for x from 0 below width
                 for index = (elt data (+ x (* y width)))
                 for argb = (index-argb color-table index t-index)
                 for offset = (* 4 (+ left x (* frame-width (+ top y))))
                 unless (= (first argb) 0)
                   do (setf (subseq frame offset) argb))))

(defun index-argb (color-table index transparency-index)
  (if (eql index transparency-index)
      (list 0 0 0 0)
      (cons 255 (multiple-value-list
                 (color-rgb (color-table-entry color-table index))))))
