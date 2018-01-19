;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; images.lisp

(in-package :fox5)

;;; TODO rewrite everything using FAST-IO instead of using raw Lisp stream
;;; reading functions
;;; TODO: aren't we already using FAST-IO everywhere? c'mon, man
(defun embed-image (image)
  (let* ((file (file image))
         (footer (footer file))
         (command-block-size (compressed-size footer))
         (images (images file))
         (images-before (subseq images 0 (position image images)))
         (compressed-sizes (reduce #'+ images-before :key #'compressed-size))
         (total-size (+ command-block-size compressed-sizes)))
    (with-input-from-binary (stream (filepath file))
      (file-position stream total-size)
      (let ((decompressed (decompress-from-stream stream))
            (multiplier (ecase (image-format image) (:8-bit 1) (:32-bit 4))))
        (assert (= (length decompressed)
                   (* multiplier (width image) (height image))))
        (setf (data image) decompressed)
        (finalize image (curry #'free-static-vector decompressed))))))

;;; TODO copy compressed data from original FOX5 instead of de- and
;;; recompressing them
(defun write-images (file buffer)
  (dolist (image (images file))
    (fast-write-sequence (compressed-data image) buffer)))

(defun ensure-compressed-images (file)
  (dolist (image (images file))
    (multiple-value-bind (compressed-block props-encoded decompressed-size)
        (cl-lzma:lzma-compress (data image))
      (let ((compressed-data
              (with-fast-output (buffer)
                (fast-write-sequence props-encoded buffer)
                (writeu64-le decompressed-size buffer)
                (fast-write-sequence compressed-block buffer))))
        (setf (compressed-data image) compressed-data)))))

(defun clean-compressed-images (file)
  (dolist (image (images file))
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
      (let ((vector (make-array 4 :initial-element 0
                                  :element-type '(unsigned-byte 8))))
        (declare (dynamic-extent vector))
        (loop repeat (* width height)
              do (setf (aref vector 3) (fast-read-byte buffer)
                       (aref vector 0) (fast-read-byte buffer)
                       (aref vector 1) (fast-read-byte buffer)
                       (aref vector 2) (fast-read-byte buffer))
                 (zpng:write-pixel vector png)))
      (zpng:finish-png png))))
