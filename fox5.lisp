;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.lisp

(in-package #:fox5)

(defun read-fox5 (pathname)
  (with-input-from-binary (stream pathname)
    (unless (validate-header-magic-string stream)
      (format t "Not a FOX5 file, skipping.~%")
      (return-from read-fox5 nil))
    (let* ((header (parse-header (read-header stream)))
           (command-block (load-command-block stream header))
           (file (parse-command-block command-block)))
      (embed-images file stream header)
      file)))
