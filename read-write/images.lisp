;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; images.lisp

(in-package :fox5)

(defun embed-images (file stream footer)
  "Given a FOX5 file object, a stream pointing to a file and a parsed FOX5
footer, decompresses all images and stores them inside the file object."
  (let ((command-block-size (compressed-size footer))
        (images (image-list file)))
    (file-position stream command-block-size)
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
      (let ((footer (make-octet-vector 12))
            (magic (make-octet-vector 8)))
        (fast-read-sequence footer buffer)
        (fast-read-sequence magic buffer)
        (assert (equal (coerce magic 'list)
                       (coerce *fox5-footer-magic-string* 'list)))))))

(defun write-images (file buffer)
  (dolist (image (image-list file))
    (fast-write-sequence (compressed-data image) buffer)))

(defun ensure-compressed-images (file)
  (dolist (image (image-list file))
    (multiple-value-bind (compressed-block props-encoded decompressed-size)
        (cl-lzma:lzma-compress (data image))
      (let ((compressed-data
              (with-fast-output (buffer)
                (fast-write-sequence props-encoded buffer)
                (writeu64-le decompressed-size buffer)
                (fast-write-sequence compressed-block buffer))))
        (setf (compressed-size image) (length compressed-data)
              (compressed-data image) compressed-data)))))

(defun clean-compressed-images (file)
  (dolist (image (image-list file))
    (slot-makunbound image '%compressed-data)))
