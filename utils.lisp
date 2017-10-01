;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; utils.lisp

(in-package :fox5)

;;; TODO Migrate to PHOE-TOOLBOX

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

(defmacro with-input-from-binary ((stream filespec) &body body)
  "Like WITH-OPEN-FILE, except with defaults suitable for reading from binary."
  `(with-open-file (,stream ,filespec :direction :input
                                      :if-does-not-exist :error
                                      :element-type 'octet)
     ,@body))

(defmacro with-output-to-binary ((stream filespec) &body body)
  "Like WITH-OPEN-FILE, except with defaults suitable for wriiting to binary."
  `(with-open-file (,stream ,filespec :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create
                                      :element-type 'octet)
     ,@body))

(defun bound-slots-values (instance)
  "Given an instance of STANDARD-OBJECT, returns a list of all slot names which
are bound in that instance."
  (check-type instance standard-object)
  (loop for slot in (c2mop:class-direct-slots (class-of instance))
        for name = (c2mop:slot-definition-name slot)
        when (slot-boundp instance name)
          collect name))

(defun vector-times (vector n)
  "Returns a fresh vector which is VECTOR concatenated to itself N times."
  (let* ((length (length vector))
         (result (make-array (* length n)
                             :element-type (array-element-type vector))))
    (loop for i from 0 upto (* length n) by length
          do (replace result vector :start1 i)
          finally (return result))))

(defun rassoc-value-or-die (alist key &key (test 'eql))
  "Like ALEXANDRIA:RASSOC-VALUE, except it signals an error if the value is
not found."
  (multiple-value-bind (value foundp)
      (alexandria:rassoc-value alist key :test test)
    (if foundp value
        (error "RASSOC of ~A was not found in ~A." key alist))))

(defun assoc-value-or-die (alist key &key (test 'eql))
  "Like ALEXANDRIA:ASSOC-VALUE, except it signals an error if the value is
not found."
  (multiple-value-bind (value foundp)
      (alexandria:assoc-value alist key :test test)
    (if foundp value
        (error "ASSOC of ~A was not found in ~A." key alist))))

(defun print-hash-table-readably (hash-table
                                  &optional (stream *standard-output*))
  "Prints a hash table readably using ALEXANDRIA:ALIST-HASH-TABLE."
  (let ((test (hash-table-test hash-table))
        (*print-circle* t))
    (format stream "#.(ALEXANDRIA:ALIST-HASH-TABLE~%")
    (format stream "'~S~%" (hash-table-alist hash-table))
    (format stream "  :TEST '~A)" test)
    nil))

(defun read-data-file (pathname)
  "Reads the data file from the provided pathname. The pathname should be
a system relative pathname."
  (let ((full-pathname (asdf:system-relative-pathname :fox5 pathname)))
    (with-input-from-file (stream full-pathname) (read stream))))

;;; The following function, ROBUST-SUBSEQ, was taken from
;;; https://github.com/death/gnusdumps and is MIT-licensed.

(defun robust-subseq (sequence start &optional end)
  "Like SUBSEQ, but handles out-of-range bounding index designators
gracefully."
  (let* ((length (length sequence))
         (start (max 0 (min start length)))
         (end (max 0 start (min length (or end length)))))
    (subseq sequence start end)))
