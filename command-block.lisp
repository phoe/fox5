;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; command-block.lisp

(in-package :fox5)

(defun load-command-block (stream header)
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
  (let ((buffer (make-input-buffer :vector vector)))
    (dotimes (i 4)
      (readu8-be buffer))
    (let ((command (read8-be buffer)))
      (assert (eql #x4C command))
      (prog1 (parse-command (code-char command) buffer)
        (assert (= (length vector) (buffer-position buffer)))))))
