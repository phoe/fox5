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

(defmacro with-input-from-binary ((stream filespec) &body body)
  `(with-open-file (,stream ,filespec :direction :input
                                      :if-does-not-exist :error
                                      :element-type 'octet)
     ,@body))

(defgeneric parse-command (command buffer))

(defvar *current-object* nil)

(defmacro define-parser ((byte buffer &optional (object-class 't)) &body body)
  `(defmethod parse-command ((command (eql ,(code-char byte))) ,buffer)
     (when (typep *current-object* ,object-class)
       ,@body)))
