;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; write-file.lisp

(in-package :fox5)

(defun write-fox5 (file filespec)
  (with-open-file (stream filespec :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede
                                   :element-type 'octet)
    (with-fast-output (buffer stream)
      (let ((command-block (with-fast-output (b) (fox5-write file b))))
        (multiple-value-bind (compressed-block props-encoded decompressed-size)
            (cl-lzma:lzma-compress command-block)
          (write-command-block compressed-block props-encoded
                               decompressed-size buffer)
          (write-images file buffer)
          (write-header (length compressed-block) decompressed-size buffer)))))
  filespec)

(defun write-command-block (compressed-block props-encoded
                            decompressed-size buffer)
  (fast-write-sequence props-encoded buffer)
  (writeu64-le decompressed-size buffer)
  (fast-write-sequence compressed-block buffer)
  (values))

(defun write-images (file buffer)
  (let ((images (image-list file)))
    (dolist (image images)
      (multiple-value-bind (compressed-block props-encoded decompressed-size)
          (cl-lzma:lzma-compress (data image))
        (fast-write-sequence props-encoded buffer)
        (writeu64-le decompressed-size buffer)
        (fast-write-sequence compressed-block buffer)))
    (values)))

;; TODO rename all headers to footers
(defun write-header (compressed-size decompressed-size buffer)
  (writeu8-be #x02 buffer) ;; LZMA compression
  (writeu8-be #x00 buffer) ;; no encryption
  (writeu16-be #x00 buffer) ;; reserved
  (writeu32-be compressed-size buffer)
  (writeu32-be decompressed-size buffer)
  (fast-write-sequence *fox5-header-magic* buffer)
  (values))

(defun compress-images (file)
  (let ((images (image-list file)))
    (mapcar #'compress-image images)))

(defun compress-image (image)
  (let ((compressed (cl-lzma:lzma-compress (data image))))
    (setf (compressed-size image) (length compressed))
    compressed))
