;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; command-block.lisp

(in-package :fox5/base)

(defun load-command-block (stream header)
  "Decompresses and reads the command block from the provided stream, using ~
the data from the provided header. Returns the unparsed command block."
  (file-position stream 0)
  (let* ((buffer (make-input-buffer :stream stream))
         (compressed-size (compressed-size header))
         (props (make-octet-vector 5))
         (length nil)
         (data (make-octet-vector compressed-size)))
    (fast-read-sequence props buffer)
    (setf length (readu64-le buffer))
    (fast-read-sequence data buffer)
    (lzma-decompress data props length)))

(defun parse-command-block (vector)
  "Parses the provided command block vector."
  (let ((buffer (make-input-buffer :vector vector)))
    (dotimes (i 4)
      (readu8-be buffer))
    (let ((command (read8-be buffer)))
      (assert (eql #x4C command))
      (prog1 (read-command (code-char command) buffer)
        (assert (= (length vector) (buffer-position buffer)))))))

(defun write-command-block (file buffer)
  "Writes the FOX5 file object into the provided buffer in form of a FOX5
command block."
  (let ((command-block (with-fast-output (b) (write-fox5-to-buffer file b))))
    (multiple-value-bind (compressed-block props-encoded decompressed-size)
        (cl-lzma:lzma-compress command-block)
      (setf (compressed-size *footer*) (+ 13 (length compressed-block))
            (decompressed-size *footer*) decompressed-size)
      (fast-write-sequence props-encoded buffer)
      (writeu64-le decompressed-size buffer)
      (fast-write-sequence compressed-block buffer)
      nil)))
