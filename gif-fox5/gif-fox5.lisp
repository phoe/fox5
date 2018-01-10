;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; gif-fox5.lisp

(defpackage #:fox5/gif-fox5
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:fox5/fox5
   #:skippy-renderer)
  (:shadowing-import-from
   #:fox5/fox5
   #:ratio)
  (:export
   #:gif-fox5
   #:validate-gif))

(in-package :fox5/gif-fox5)

;;; TODO add ability to convert non-portrait GIFs
;;; TODO add modifiable conversion types
;;; TODO implement all of the freaking edit types from the FOX5 spec

(defun gif-fox5 (input-filename output-filename)
  "Loads the GIF file from the provided filename, converts it to a FOX5 file and
saves the result under the provided filename."
  (let ((gif (skippy:load-data-stream input-filename)))
    ;; TODO warning when over 50 frames
    ;;(validate-gif gif)
    (multiple-value-bind (images ks) (gif-to-images gif)
      (write-fox5 (gif-make-file images ks) output-filename))))

(defun validate-portrait (data-stream truncatep)
  "Validate that the provided GIF data stream is a proper portrait."
  (let ((width (skippy:width data-stream))
        (height (skippy:height data-stream))
        (length (length (skippy:images data-stream))))
    (unless (= 95 width)
      (warn "Target file has width of ~D pixels; portraits of width different ~
than 95px will not display correctly." width))
    (unless (= 95 height)
      (warn "Target file has height of ~D pixels; portraits of height ~
different than 95px will not display correctly." height))
    (unless (>= 50 length)
      (warn "Target file has ~D animation frames; the maximum for portraits is ~
50 frames. File will~A be truncated." length (if truncatep " " " NOT")))
    (values)))

(defun gif-to-images (gif)
  (multiple-value-bind (arrays delays data) (skippy-renderer:render gif)
    (destructuring-bind (width height loopingp) data
      (declare (ignore loopingp))
      (values (mapcar (curry #'make-instance 'image
                             :format :32-bit :width width
                             :height height :data)
                      (robust-subseq arrays 0 50))
              (loop for i below (min 50 (length delays))
                    append (make-gif-ks i (nth i delays)))))))

(defun make-gif-ks (i delay)
  (let ((delay (if (= 0 delay) 100 (* 10 delay))))
    `((29 ,i 0) (2 ,delay 0))))

;;; Generating CLOS objects

(defun gif-make-file (images kitterspeak &optional remappingp)
  (let ((file (make-instance 'file
                             :generator `(:third-party ,*fox5-generator-number*)
                             :image-list images)))
    (push (gif-make-object images kitterspeak remappingp) (children file))
    file))

(defun gif-make-object (images kitterspeak &optional remappingp)
  (let ((object (make-instance 'object
                               :edit-type 4 :flags nil
                               :authors '("Created by Raptor FOX5 library"))))
    (push (gif-make-shape images kitterspeak remappingp) (children object))
    object))

(defun gif-make-shape (images kitterspeak &optional remappingp)
  (let ((shape (make-instance 'shape
                              :direction nil :kitterspeak kitterspeak
                              :purpose :portrait :ratio '(1 1) :state 2))
        (frames (loop for image in images
                      for i from 1
                      collect (gif-make-frame i remappingp))))
    (setf (children shape) frames)
    shape))

(defun gif-make-frame (i &optional remappingp)
  (let* ((frame (make-instance 'frame))
         (purpose (if remappingp :remapping-data nil))
         (sprite (make-instance 'sprite :image-id i :purpose purpose)))
    (push sprite (children frame))
    frame))
