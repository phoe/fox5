;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; gif.lisp

(in-package :fox5)

(defun validate-gif (data-stream)
  (assert (= 95 (skippy:width data-stream)))
  (assert (= 95 (skippy:height data-stream)))
  (values))

(defun gif-frame-to-argb (color-table image)
  (loop with width = (skippy:width image)
        with height = (skippy:height image)
        with length = (* 4 width height)
        with array = (make-array length :element-type '(unsigned-byte 8))
        with i = -1
        for index across (skippy:image-data image)
        for rgb = (multiple-value-list
                   (skippy:color-rgb
                    (skippy:color-table-entry color-table index)))
        do (setf (aref array (incf i)) 255
                 (aref array (incf i)) (first rgb)
                 (aref array (incf i)) (second rgb)
                 (aref array (incf i)) (let ((b (third rgb)))
                                         (if (= b 0) 1 b)))
        finally (return (make-instance 'image
                                       :format :32-bit
                                       :width width
                                       :height height
                                       :data array))))

(defun gif-to-images (gif)
  (loop with table = (skippy:color-table gif)
        for image across (skippy:images gif)
        for i from 0
        collect (gif-frame-to-argb table image) into argbs
        append (make-gif-ks i (skippy:delay-time image)) into ks
        finally (return (values argbs (fix-ks ks)))))

(defun make-gif-ks (i delay)
  `((29 ,i 0) (2 ,(if (= 0 delay) 10 (* 10 delay)) 0)))

(defun fix-ks (ks)
  (setf (cdr (last ks)) (cons (first ks) nil))
  ks)

(defparameter *fox5-generator-number*
  200)

(defun gif-make-file (images kitterspeak &optional remappingp)
  (let ((file (make-instance 'file
                             :generator `(:third-party ,*fox5-generator-number*)
                             :image-list images)))
    (push (gif-make-object images kitterspeak remappingp) (children file))
    file))

(defun gif-make-object (images kitterspeak &optional remappingp)
  (let ((object (make-instance 'object
                               :edit-type 4
                               :flags nil
                               :authors '("Created by Raptor FOX5 library"))))
    (push (gif-make-shape images kitterspeak remappingp) (children object))
    object))

(defun gif-make-shape (images kitterspeak &optional remappingp)
  (let ((shape (make-instance 'shape
                              :direction nil
                              :kitterspeak kitterspeak
                              :purpose :portrait
                              :ratio '(1 1)
                              :state 2))
        (frames (loop for image in images
                      for i from 1
                      collect (gif-make-frame i remappingp))))
    (setf (children shape) frames)
    shape))

(defun gif-make-frame (i &optional remappingp)
  (let* ((frame (make-instance 'frame))
         (purpose (if remappingp :remapping-data nil))
         (sprite (make-instance 'sprite :image-id i
                                        :purpose purpose)))
    (push sprite (children frame))
    frame))
