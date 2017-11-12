;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; utils.lisp

(in-package :fox5/base)

(defun octetize (string &optional (external-format :utf-8))
  "Shortcut for FLEXI-STREAMS:STRING-TO-OCTETS."
  (flexi-streams:string-to-octets string :external-format external-format))

(defun read-string (buffer &optional (external-format :utf-8))
  "Reads a FOX5 string from the provided octet buffer. First, it reads a
U16-BE signifying the number of bytes in the string, and then, it reads that~
many characters and puts them in the string."
  (declare (type fast-io::input-buffer buffer))
  (let* ((n (readu16-be buffer))
         (vector (make-octet-vector n)))
    (fast-io:fast-read-sequence vector buffer)
    (octetize vector external-format)))
