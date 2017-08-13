;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; header.lisp

(in-package :fox5)

(defclass fox5-header ()
  ((%compression-type :accessor compression-type
                      :initarg :compression-type)
   (%encryption :accessor encryption
                :initarg :encryption)
   (%compressed-size :accessor compressed-size
                     :initarg :compressed-size)
   (%decompressed-size :accessor decompressed-size
                       :initarg :decompressed-size)))

(defun read-header (stream)
  (file-position stream (- (file-length stream) 20))
  (let ((buffer (make-input-buffer :stream stream))
        (vector (make-octet-vector 20)))
    (fast-read-sequence vector buffer)
    vector))

(defun parse-header (header)
  (let ((buffer (make-input-buffer :vector header))
        (instance (make-instance 'fox5-header)))
    (setf (compression-type instance)
          (ecase (read8-be buffer) (1 :zlib) (2 :lzma))
          (encryption instance)
          (let ((byte (read8-be buffer)))
            (ecase byte (0 :no) (t (list :yes byte)))))
    (read16-be buffer) ;; reserved bytes
    (setf (compressed-size instance) (read32-be buffer)
          (decompressed-size instance) (read32-be buffer))
    instance))

(defun validate-header-magic-string (stream)
  (file-position stream (- (file-length stream) 8))
  (let* ((buffer (make-input-buffer :stream stream))
         (magic-string (make-octet-vector 8))
         (magic-buffer (make-output-buffer :vector magic-string)))
    (dotimes (i 8)
      (fast-write-byte (fast-read-byte buffer) magic-buffer)
      (finish-output-buffer magic-buffer))
    (when (equal (coerce magic-string 'list)
                 (coerce *fox5-header-magic* 'list))
      t)))
