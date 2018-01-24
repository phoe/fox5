;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; gif.lisp

(in-package :fox5)

(export 'read-gif :fox5)

(defun read-gif (pathname &key remapp)
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (let ((gif (skippy:load-data-stream pathname)))
    (multiple-value-bind (images ks) (gif-images gif)
      (gif-fox5 images ks :remapp remapp))))

(defun gif-images (gif)
  (flet ((ks (i delay)
           (let ((delay (if (= 0 delay) 100 (* 10 delay))))
             `((:frame-behind ,i 0) (:delay ,delay 0))))
         (reendian (vector)
           (declare (optimize speed)
                    (type (simple-array (unsigned-byte 8) (*)) vector))
           (loop for start below (array-total-size vector) by 4
                 for end = (+ start 4)
                 do (nreversef (subseq vector start end)))))
    (multiple-value-bind (vectors delays data) (skippy-renderer:render gif)
      (destructuring-bind (width height loopingp) data
        (declare (ignore loopingp))
        (values (mapcar (curry #'make-instance 'image
                               :width width :height height :data)
                        (mapc #'reendian vectors))
                (loop for i below (length delays)
                      append (ks i (nth i delays))))))))

(defun gif-fox5 (images ks &key remapp)
  (loop with purpose = (if remapp :remapping-data nil)
        with file = (make-instance 'file :images images)
        with object = (make-instance 'object)
        with shape = (make-instance 'shape :kitterspeak ks)
        for image in images
        for i from 1
        for frame = (make-instance 'frame)
        for sprite = (make-instance 'sprite :image-id i :purpose purpose)
        do (push frame (children shape))
           (push sprite (children frame))
        finally (nreversef (children shape))
                (push shape (children object))
                (push object (children file))
                (return file)))
