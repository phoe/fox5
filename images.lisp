;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; header.lisp

(in-package :fox5)

(defun embed-images (file stream header)
  (let ((header-size (compressed-size header))
        (images (image-list file)))
    (file-position stream header-size)
    (let ((buffer (make-input-buffer :stream stream)))
      (loop for image in images
            for format = (image-format image)
            for compressed-size = (compressed-size image)
            for compressed = (make-octet-vector compressed-size)
            for props = (make-octet-vector 5)
            for length = nil
            for data = (make-octet-vector (- compressed-size 13))
            do (fast-read-sequence props buffer)
               (setf length (readu64-le buffer))
               (fast-read-sequence data buffer)
               (let ((decompressed (lzma-decompress data props length))
                     (multiplier (case format (:8-bit 1) (:32-bit 4))))
                 (assert (= (length decompressed)
                            (* multiplier (width image) (height image))))
                 (setf (data image) decompressed)))
      (let ((header (make-octet-vector 12))
            (magic (make-octet-vector 8)))
        (fast-read-sequence header buffer)
        (fast-read-sequence magic buffer)
        (assert (equal (coerce magic 'list)
                       (coerce *fox5-header-magic* 'list)))))))

(defun draw-rgb (image filespec)
  (let* ((width (width image))
         (height (height image))
         (buffer (make-input-buffer :vector (data image)))
         (png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width width
                             :height height)))
    (with-open-file (stream filespec :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type 'octet)
      (zpng:start-png png stream)
      (loop repeat (* width height)
            for a = (fast-read-byte buffer)
            for r = (fast-read-byte buffer)
            for g = (fast-read-byte buffer)
            for b = (fast-read-byte buffer)
            do (zpng:write-pixel (list r g b a) png))
      (zpng:finish-png png))))

(defun denormalize-gif (filespec)
  (flet ((fix-gif (transparency-index orig-canvas new-canvas)
           (let ((orig (skippy:image-data orig-canvas))
                 (new (skippy:image-data new-canvas)))
             (loop for i from 0
                   for x across orig
                   for y across new
                   if (= y transparency-index)
                     do (setf (aref new i) (aref orig i)))
             new-canvas)))
    (loop ;; VARS
          with data = (skippy:load-data-stream filespec)
          with images = (coerce (skippy:images data) 'list)
          with result = (list (first images))
          with width = (skippy:width data)
          with height = (skippy:height data)
          with color-table = (skippy:color-table data)
          with loopingp = (skippy:loopingp data)
          with base-image-data = (copy-seq (skippy:image-data (first images)))
          with base-canvas = (skippy:make-canvas :width width
                                                 :height height
                                                 :image-data base-image-data)
          ;; LOOP
          for image in (cdr images)
          for image-data = (skippy:image-data image)
          for new-width = (skippy:width image)
          for new-height = (skippy:height image)
          for disposal-method = (skippy:disposal-method image)
          for transparency-index = (skippy:transparency-index image)
          for canvas = (skippy:make-canvas :width new-width
                                           :height new-height
                                           :image-data image-data)
          for top-position = (skippy:top-position image)
          for left-position = (skippy:left-position image)
          for delay-time = (skippy:delay-time image)
          for clone = (skippy:clone base-canvas)
          for _ = (skippy:composite canvas clone
                                    :dx left-position
                                    :dy top-position)
          for fixed = (fix-gif transparency-index base-canvas clone)
          for new-image = (skippy:canvas-image fixed)
          do (setf (skippy:delay-time new-image) delay-time
                   (skippy:disposal-method new-image) disposal-method
                   (skippy:transparency-index new-image) transparency-index
                   base-canvas clone)
             (push new-image result)
          finally
             (return (skippy:make-data-stream
                      :width width
                      :height height
                      :color-table color-table
                      :loopingp loopingp
                      :initial-images (nreverse result))))))
