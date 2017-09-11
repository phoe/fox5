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

(defgeneric fox5-write-slot (class-name slot-name buffer))

(defvar *current-object* nil)

(defvar *parent-object* nil)

(defmacro define-parser ((byte buffer
                          &optional (object-class 't)) &body body)
  `(defmethod parse-command ((command (eql ,(code-char byte)))
                             ,buffer)
     (when (typep *current-object* ,object-class)
       ,@body)))

(defmacro define-writer (class-name accessor-name (buffer-var) &body body)
  (let ((slot-name (symbolicate "%" accessor-name)))
    `(defmethod fox5-write-slot ((,class-name ,class-name)
                                 (slot-name (eql ',slot-name))
                                 ,buffer-var)
       (let ((,accessor-name (slot-value ,class-name slot-name)))
         ,@body))))

(defun octetize (string &optional (external-format :utf-8))
  (flexi-streams:string-to-octets string :external-format external-format))

(defun bound-slots-values (o)
  (loop for s in (c2mop:class-direct-slots (class-of o))
        for n = (c2mop:slot-definition-name s)
        when (slot-boundp o n)
          collect n))
