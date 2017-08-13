;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; utils.lisp

(in-package :fox5)

(defun read-string (buffer &optional (external-format :utf-8))
  (declare (type fast-io::input-buffer buffer))
  (let* ((n (readu16-be buffer))
         (vector (make-octet-vector n)))
    (fast-io:fast-read-sequence vector buffer)
    (flexi-streams:octets-to-string vector :external-format external-format)))
