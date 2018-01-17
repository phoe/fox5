;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; classes.lisp

(in-package :fox5/fox1)

(defclass file ()
  ((%version :accessor version)
   (%nshapes :accessor nshapes)
   (%generator :accessor generator)
   (%encryption :accessor encryption)
   (%shapes :accessor shapes :initform '())))

(defclass shape ()
  ((%walkablep :accessor walkablep)
   (%gettablep :accessor gettablep)
   (%sittablep :accessor sittablep)
   (%index :accessor index :initform -1)
   (%frames :accessor frames :initform '())
   (%steps :accessor steps :initform '())))

(defclass frame (fox5/base:image)
  ((%position-x :accessor position-x)
   (%position-y :accessor position-y)
   (%furre-position-x :accessor furre-position-x)
   (%furre-position-y :accessor furre-position-y)))

(defun read-fox1 (pathname)
  "Reads the provided FOX1 file from the given pathname and returns its parsed
representation."
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (with-input-from-binary (stream pathname)
    (unless (validate-magic-string stream)
      (error "Not a FOX1 file: ~A" pathname))
    (let* ((vector (load-header stream))
           (header (parse-header vector)))
      (parse-file stream header))))

(defun validate-magic-string (stream)
  "Returns true if the file behind the provided stream is a FOX1 file, and false
otherwise."
  (file-position stream 0)
  (with-fast-input (buffer nil stream)
    (= (readu32-be buffer) #x46534858)))

(defun load-header (stream)
  "Loads the FOX1 header from the file and returns its unparsed form."
  (file-position stream 4)
  (let ((buffer (make-input-buffer :stream stream))
        (vector (make-octet-vector 24)))
    (fast-read-sequence vector buffer)
    vector))

(defun parse-header (footer)
  "Parses the provided FOX1 header and returns its parsed form."
  (let ((buffer (make-input-buffer :vector footer))
        (instance (make-instance 'file)))
    (let ((version (readu32-le buffer)))
      (case version
        ((1 2) (setf (version instance) version))
        (t (error "Unimplemented FOX version ~D detected." version))))
    (setf (nshapes instance) (read32-le buffer)
          (generator instance)
          (let ((generator (read32-le buffer)))
            (list (if (<= generator #x20000) :furcadia :other) generator))
          (encryption instance)
          (let ((encryption (read32-le buffer)))
            (case encryption (0 :no) (t (list :yes encryption)))))
    (unless (eq (encryption instance) :no)
      (error "File is encrypted with encryption format ~D."
             (second (encryption instance))))
    (read32-le buffer) ;; reserved bytes
    (read32-le buffer) ;; reserved bytes
    instance))

(defun parse-file (stream file)
  (file-position stream 28)
  (with-fast-input (buffer nil stream)
    (let* ((nshapes (nshapes file))
           (shapes (loop repeat nshapes collect (parse-shape buffer))))
      (setf (shapes file) shapes))
    (assert (= (file-position stream) (file-length stream)))
    file))

(defun parse-shape (buffer)
  (let* ((shape (make-instance 'shape))
         (flags (readu16-le buffer)))
    (setf (walkablep shape) (ldb (byte 1 0) flags)
          (gettablep shape) (ldb (byte 1 1) flags)
          (sittablep shape) (ldb (byte 1 2) flags)
          (index shape) (read16-le buffer))
    (let ((nframes (readu16-le buffer))
          (nsteps (readu16-le buffer)))
      (setf (frames shape) (loop repeat nframes collect (parse-frame buffer))
            (steps shape) (loop repeat nsteps collect (parse-step buffer))))
    shape))

(defun parse-frame (buffer)
  (let ((frame (make-instance 'frame)))
    (let ((image-format (ecase (readu16-le buffer)
                          (1 :8-bit) (2 :bgr) (3 :bgra) (7 :bgra-recol))))
      (unless (eq image-format :8-bit)
        (error "Unsupported image format: ~A" image-format)))
    (setf (width frame) (readu16-le buffer)
          (height frame) (readu16-le buffer)
          (position-x frame) (read16-le buffer)
          (position-y frame) (read16-le buffer)
          (furre-position-x frame) (read16-le buffer)
          (furre-position-y frame) (read16-le buffer))
    (let* ((data (make-octet-vector (readu32-le buffer))))
      (fast-read-sequence data buffer)
      (setf (data frame) (8bit-32bit-no-remap data)))
    frame))

(defun parse-step (buffer)
  (list (assoc-value-or-die *kitterspeak* (readu16-le buffer))
        (readu16-le buffer) (readu16-le buffer)))
