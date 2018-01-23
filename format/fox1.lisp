;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda
;;;; classes.lisp

(in-package :fox5)

(defun read-fox1 (pathname &optional remapp)
  "Reads the provided FOX1 file from the given pathname and returns its parsed
representation converted to a FOX5 file. The key argument REMAPP controls if
the file is meant to be remappable."
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (with-input-from-binary (stream pathname)
    (with-fast-input (buffer nil stream)
      (unless (= (readu32-be buffer) #x46534858)
        (error "Not a FOX1 file: ~A" pathname))
      (let* ((nshapes (%fox1-parse-header buffer))
             (file (%fox1-parse-file buffer nshapes remapp)))
        (assert (= (file-position stream) (file-length stream)))
        file))))

(defun %fox1-parse-header (buffer)
  (let ((version (read32-le buffer)))
    (when (> version 2)
      (error "Unimplemented FOX version ~D detected." version)))
  "Parses the provided FOX1 header and returns its parsed form."
  (let ((nshapes (read32-le buffer))
        (generator (read32-le buffer))
        (encryption (read32-le buffer)))
    (declare (ignore generator))
    (read32-le buffer) (read32-le buffer) ;; reserved
    (unless (= encryption 0)
      (error "File is encrypted with encryption format ~D." encryption))
    nshapes))

(defvar %*fox1-image-id* nil)

(defun %fox1-parse-file (buffer nshapes remapp)
  (loop with %*fox1-image-id* = 0
        with file = (make-instance 'file)
        repeat nshapes
        for object = (make-instance 'object)
        for shape = (make-instance 'shape)
        for flags = (readu16-le buffer)
        for id = (read16-le buffer)
        for nframes = (readu16-le buffer)
        for nsteps = (readu16-le buffer)
        for (frames images)
          = (loop repeat nframes
                  for (frame image) = (%fox1-parse-frame buffer remapp)
                  collect frame into frames
                  collect image into images
                  finally (return (list frames images)))
        for steps = (loop repeat nsteps
                          collect (list (assoc-value-or-die *kitterspeak*
                                                            (readu16-le buffer))
                                        (read16-le buffer)
                                        (read16-le buffer)))
        for walkablep = (= 1 (ldb (byte 1 0) flags))
        for gettablep = (= 1 (ldb (byte 1 1) flags))
        for sittablep = (= 1 (ldb (byte 1 2) flags))
        do (when walkablep (push :walkable (flags object)))
           (when gettablep (push :gettable (flags object)))
           (when sittablep (push :sittable (flags object)))
           (push shape (children object))
           (push object (children file))
           (nconcf (images file) images)
           (setf (id object) id
                 (children shape) frames
                 (kitterspeak shape) steps)
        finally (nreversef (children file))
                (return file)))

(defun %fox1-parse-frame (buffer remapp)
  (let* ((frame (make-instance 'frame))
         (image (make-instance 'image))
         (sprite (make-instance 'sprite))
         (image-format (readu16-le buffer))
         (width (readu16-le buffer))
         (height (readu16-le buffer)))
    (unless (eq image-format 1)
      (error "Unsupported image format: ~A" image-format))
    (setf (width image) width
          (height image) height
          (frame-offset frame)
          (list :x (read16-le buffer) :y (read16-le buffer))
          (furre-offset frame)
          (list :x (read16-le buffer) :y (read16-le buffer))
          (image-id sprite) (incf *image-id*)
          (purpose sprite) (if remapp :remapping-data nil))
    (readu32-le buffer)
    (setf (data image) (%read-fsh-image buffer width height remapp)
          (image-format image) :32-bit)
    (push sprite (children frame))
    (list frame image)))
