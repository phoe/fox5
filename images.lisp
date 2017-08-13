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
                                     :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop repeat (* width height)
            for a = (fast-read-byte buffer)
            for r = (fast-read-byte buffer)
            for g = (fast-read-byte buffer)
            for b = (fast-read-byte buffer)
            do (zpng:write-pixel (list r g b a) png))
      (zpng:finish-png png))))
