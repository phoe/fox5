;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; footer.lisp

(in-package :fox5)

(defclass footer ()
  ((%compression-type :accessor compression-type
                      :initarg :compression-type)
   (%encryption :accessor encryption
                :initarg :encryption)
   (%compressed-size :accessor compressed-size
                     :initarg :compressed-size)
   (%decompressed-size :accessor decompressed-size
                       :initarg :decompressed-size)))

(defun load-footer (stream)
  "Loads the FOX5 footer from the file and returns its unparsed form."
  (file-position stream (- (file-length stream) 20))
  (let ((buffer (make-input-buffer :stream stream))
        (vector (make-octet-vector 20)))
    (fast-read-sequence vector buffer)
    vector))

(defun parse-footer (footer)
  "Parses the provided FOX5 footer and returns its parsed form."
  (let ((buffer (make-input-buffer :vector footer))
        (instance (make-instance 'footer)))
    (setf (compression-type instance)
          (ecase (read8-be buffer) (1 :zlib) (2 :lzma))
          (encryption instance)
          (let ((byte (read8-be buffer)))
            (case byte (0 :no) (t (list :yes byte)))))
    (unless (eq (encryption instance) :no)
      (error "File is encrypted with encryption format ~D."
             (second (encryption instance))))
    (read16-be buffer) ;; reserved bytes
    (setf (compressed-size instance) (readu32-be buffer)
          (decompressed-size instance) (readu32-be buffer))
    instance))

(defun validate-footer-magic-string (stream)
  "Returns true if the file behind the provided stream is a FOX5 file, and false
otherwise."
  (file-position stream (- (file-length stream) 8))
  (let* ((buffer (make-input-buffer :stream stream))
         (magic-string (make-octet-vector 8))
         (magic-buffer (make-output-buffer :vector magic-string)))
    (dotimes (i 8)
      (fast-write-byte (fast-read-byte buffer) magic-buffer)
      (finish-output-buffer magic-buffer))
    (when (equal (coerce magic-string 'list)
                 (coerce *footer-magic-string* 'list))
      t)))

(defun write-footer (buffer)
  "Writes a FOX5 footer to the provided buffer."
  (writeu8-be #x02 buffer) ;; LZMA compression
  (writeu8-be #x00 buffer) ;; no encryption
  (writeu16-be #x00 buffer) ;; reserved bytes
  (writeu32-be (compressed-size *footer*) buffer)
  (writeu32-be (decompressed-size *footer*) buffer)
  (fast-write-sequence *footer-magic-string* buffer))
