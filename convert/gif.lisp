;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; gif.lisp

(in-package :fox5)

(export 'read-gif :fox5)

(defun read-gif (pathname &key remapp)
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (let ((gif (skippy:load-data-stream pathname)))
    (destructuring-bind (images ks) (gif-images gif)
      (gif-fox5 images ks :remapp remapp))))

(defun gif-images (gif)
  (multiple-value-bind (vectors delays data)
      (skippy-renderer:render gif :byte-order :bgra)
    (destructuring-bind (width height loopingp) data
      (declare (ignore loopingp))
      (flet ((ks (i delay) (let ((delay (if (= 0 delay) 100 (* 10 delay))))
                             `((:frame-behind ,i 0) (:delay ,delay 0))))
             (make-image (data)
               (make-instance 'image :width width :height height :data data)))
        (list (mapcar #'make-image vectors)
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
        do (parent-push shape frame)
           (parent-push frame sprite)
        finally (nreversef (children shape))
                (parent-push object shape)
                (parent-push file object)
                (return file)))
