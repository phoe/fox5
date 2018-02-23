;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; images.lisp

(in-package :fox5)

(defun embed-image (image)
  "Reads the respective image's data from the FOX5 file and inserts it into the
image object"
  (let* ((file (file image))
         (footer (footer file))
         (command-block-size (compressed-size footer))
         (images (images file))
         (images-before (subseq images 0 (position image images)))
         (compressed-sizes (reduce #'+ images-before :key #'compressed-size))
         (total-size (+ command-block-size compressed-sizes)))
    (with-input-from-binary (stream (filepath file))
      (file-position stream total-size)
      (let ((decompressed (decompress-from-stream stream)))
        (assert (= (length decompressed)
                   (* 4 (width image) (height image))))
        (loop for i from 0 below (length decompressed) by 4
              do (nreversef (subseq decompressed i (+ 4 i))))
        (setf (slot-value image '%data) decompressed)
        (finalize image (curry #'free-static-vector decompressed))))))

(defun embed-images-into-sprites (file)
  (let ((images (images file)))
    (setf (images file) '())
    (dolist (object (children file))
      (dolist (shape (children object))
        (dolist (frame (children shape))
          (dolist (sprite (children frame))
            (let* ((id (1- (image-id sprite)))
                   (image (nth id images)))
              (when (null image)
                (warn "Null image for:~%~S~%~S~%~S~%"
                      object shape sprite))
              (setf (image sprite) image
                    (image-id sprite) nil))))))))

(defun regenerate-image-list (file)
  (let ((hash-table (make-hash-table)))
    (dolist (object (children file))
      (dolist (shape (children object))
        (dolist (frame (children shape))
          (dolist (sprite (children frame))
            (let ((image (image sprite)))
              (when image
                (setf (gethash image hash-table) t)))))))
    (let ((images (hash-table-keys hash-table)))
      (loop for i from 0
            for image in images
            do (setf (gethash image hash-table) i))
      (setf (images file) images))
    (dolist (object (children file))
      (dolist (shape (children object))
        (dolist (frame (children shape))
          (dolist (sprite (children frame))
            (let ((image (image sprite)))
              (setf (image-id sprite) (1+ (gethash image hash-table))))))))))

;;; TODO copy compressed data from original FOX5 instead of de- and
;;; recompressing them
(defun write-images (file buffer)
  (dolist (image (images file))
    (fast-write-sequence (compressed-data image) buffer)))

(defun ensure-compressed-images (file)
  (dolist (image (images file))
    (when (not (slot-boundp image '%compressed-data))
      (let ((data (copy-seq (data image))))
        (loop for i from 0 below (length data) by 4
              do (nreversef (subseq data i (+ 4 i))))
        (multiple-value-bind (compressed-block props-encoded decompressed-size)
            (cl-lzma:lzma-compress data)
          (let ((compressed-data
                  (with-fast-output (buffer)
                    (fast-write-sequence props-encoded buffer)
                    (writeu64-le decompressed-size buffer)
                    (fast-write-sequence compressed-block buffer))))
            (setf (compressed-data image) compressed-data)))))))

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
