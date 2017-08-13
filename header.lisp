;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defun read-header (buffer)
  (declare (fast-io::input-buffer buffer))
  (let ((vector (make-octet-vector 20)))
    (fast-read-sequence vector buffer)
    vector))

(defun analyze-header (header)
  (let ((buffer (make-input-buffer :vector header))
        (data '()))
    (flet ((add (key value) (push (cons key value) data)))
      (add :compression-type (ecase (read8-be buffer) (1 :zlib) (2 :lzma)))
      (add :encryption (let ((byte (read8-be buffer)))
                         (ecase byte (0 :no) (t (list :yes byte)))))
      (read16-be buffer) ;; reserved bytes
      (add :compressed-size (read32-be buffer))
      (add :decompressed-size (read32-be buffer))
      (validate-magic-string buffer)
      (nreverse data))))

(defun validate-header-magic-string (buffer)
  (let* ((magic-string (make-octet-vector 8))
         (magic-buffer (make-output-buffer :vector magic-string)))
    (dotimes (i 8)
      (fast-write-byte (fast-read-byte buffer) magic-buffer)
      (finish-output-buffer magic-buffer))
    (when (not (equal (coerce magic-string 'list)
                      (coerce *fox5-header-magic* 'list)))
      (error *fox5-header-magic-mismatch* *fox5-header-magic* magic-string))))
