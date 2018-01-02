;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; images.lisp

(in-package :fox5/base)

;;; TODO rewrite everything using FAST-IO instead of using raw Lisp stream
;;; reading functions
;;; TODO rewrite CL-LZMA using static-vectors
(defun embed-image (image)
  (let* ((file (file image))
         (footer (footer file))
         (command-block-size (compressed-size footer))
         (images (image-list file))
         (images-before (subseq images 0 (position image images)))
         (compressed-sizes (reduce #'+ images-before :key #'compressed-size))
         (total-size (+ command-block-size compressed-sizes))
         (compressed-size (compressed-size image)))
    (with-input-from-binary (stream (filepath file))
      (file-position stream total-size)
      (let* ((buffer (make-input-buffer :stream stream))
             (format (image-format image))
             (length 0))
        (with-static-vectors ((props 5)
                              (data (- compressed-size 13)))
          (fast-read-sequence props buffer)
          (setf length (readu64-le buffer))
          (fast-read-sequence data buffer)
          (let ((decompressed (decompress-from-static-vectors data props length))
                (multiplier (ecase format (:8-bit 1) (:32-bit 4))))
            (assert (= (length decompressed)
                       (* multiplier (width image) (height image))))
            (setf (data image) decompressed)
            (finalize image (curry #'free-static-vector decompressed))))))))

;;; TODO copy compressed data from original FOX5 instead of de- and
;;; recompressing them
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
        (setf (compressed-data image) compressed-data)))))

(defun clean-compressed-images (file)
  (dolist (image (image-list file))
    (slot-makunbound image '%compressed-size)
    (slot-makunbound image '%compressed-data)))

(defun draw-png (image pathname)
  "Renders a single FOX5 image into a PNG, saved under the provided filename."
  (let* ((width (width image))
         (height (height image))
         (buffer (make-input-buffer :vector (data image)))
         (png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width width :height height)))
    (with-output-to-binary (stream pathname)
      (zpng:start-png png stream)
      (loop repeat (* width height)
            for a = (fast-read-byte buffer)
            for r = (fast-read-byte buffer)
            for g = (fast-read-byte buffer)
            for b = (fast-read-byte buffer)
            do (zpng:write-pixel (list r g b a) png))
      (zpng:finish-png png))))
