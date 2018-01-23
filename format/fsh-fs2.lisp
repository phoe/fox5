;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fsh.lisp

(in-package #:fox5)
;; TODO new package for these(?)

(export 'read-fsh :fox5)
(export 'read-fs2 :fox5)

(defun read-fsh (pathname &key remapp)
  "Reads the provided FSH file from the given pathname and returns its parsed
representation converted to a FOX5 file. The key argument REMAPP controls if
the file is meant to be remappable."
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (with-input-from-binary (stream pathname)
    (with-fast-input (buffer nil stream)
      (let ((file (make-instance 'file))
            (nshapes (readu16-le buffer)))
        (loop repeat nshapes do (readu16-le buffer))
        (loop for i from 1 to nshapes
              for object = (make-instance 'object)
              for shape = (make-instance 'shape)
              for frame = (make-instance 'frame)
              for sprite = (make-instance 'sprite)
              for image = (make-instance 'image)
              for width = (readu8 buffer)
              for height = (readu8 buffer)
              do (setf
                  (image-format image) :32-bit
                  (generator file) (list :third-party *fox5-generator-number*)
                  (width image) width
                  (height image) height
                  (frame-offset frame)
                  (list :x (read8 buffer) :y (read8 buffer))
                  (image-id sprite) i
                  (data image) (%read-fsh-image buffer width height remapp)
                  (purpose sprite) (if remapp :remapping-data nil))
                 (push image (images file))
                 (push object (children file))
                 (push shape (children object))
                 (push frame (children shape))
                 (push sprite (children frame))
              finally (nreversef (images file))
                      (nreversef (children file))
                      (return file))))))

(defun read-fs2 (pathname &key remapp)
  "Reads the provided FS2 file from the given pathname and returns its parsed
representation converted to a FOX5 file. The key argument REMAPP controls if
the file is meant to be remappable."
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (with-input-from-binary (stream pathname)
    (with-fast-input (buffer nil stream)
      (let ((magic (readu64-be buffer)))
        (assert (= magic #x465348322E303031) () "Not a FSH2 file: ~S" pathname))
      (let ((file (make-instance 'file))
            (nshapes (readu32-le buffer))
            (flags (readu32-be buffer)))
        (assert (= flags 0) () "Encrypted FSH2 file: ~S" pathname)
        (loop for i from 1 to nshapes
              for object = (make-instance 'object)
              for shape = (make-instance 'shape)
              for frame = (make-instance 'frame)
              for sprite = (make-instance 'sprite)
              for image = (make-instance 'image)
              for width = (readu8 buffer)
              for height = (readu8 buffer)
              do (setf
                  (image-format image) :32-bit
                  (width image) width
                  (height image) height
                  (frame-offset frame)
                  (list :x (read8 buffer) :y (read8 buffer))
                  (id object) (readu16-le buffer)
                  (image-id sprite) i
                  (data image) (%read-fsh-image buffer width height remapp))
                 (push image (images file))
                 (push object (children file))
                 (push shape (children object))
                 (push frame (children shape))
                 (push sprite (children frame))
              finally (nreversef (images file))
                      (nreversef (children file))
                      (return file))))))

(defun %read-fsh-image (buffer width height remapp)
  ;; TODO optimize to avoid reading and rotatefing
  (let ((total (* width height)))
    (with-static-vector (v total)
      (fast-read-sequence v buffer)
      ;; FSH images are Y-flipped, so we unflip them here.
      (loop for i below (truncate (/ height 2))
            for start1 = (* i width)
            for end1 = (+ width start1)
            for start2 = (- total (* (1+ i) width))
            for end2 = (+ width start2)
            do (rotatef (subseq v start1 end1)
                        (subseq v start2 end2)))
      (8bit-32bit v remapp))))

#|
### Legacy:
Seated SW
Traveling right foot forward SW
Standing SW
Traveling left foot forward SW
Seated SE
Traveling right foot forward SE
Standing SE
Traveling left foot forward SE
Seated NW
Traveling right foot forward NW
Standing NW
Traveling left foot forward NW
Seated NE
Traveling right foot forward NE
Standing NE
Traveling left foot forward NE
Lying SW
Lying SE
Lying NE
Lying NW
|#
