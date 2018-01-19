;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; read-write-file.lisp

(in-package :fox5/fox5)

(defun read-fox5 (pathname &optional (embed-images-p t))
  #.(format nil "Reads the FOX5 file from the provided file and returns the ~
parsed FOX5 file object.")
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (with-input-from-binary (stream pathname)
    (unless (validate-footer-magic-string stream)
      (error "Not a FOX5 file: ~A" pathname))
    (let* ((vector (load-footer stream))
           (footer (parse-footer vector))
           (command-block (load-command-block stream footer))
           (file (parse-command-block command-block)))
      (setf (filepath file) pathname
            (footer file) footer)
      (when embed-images-p
        (mapc #'embed-image (image-list file)))
      file)))

(defun write-fox5 (file pathname &optional preserve-compressed-p)
  #.(format nil "Writes the provided FOX5 file object into a file with the ~
provided pathname. The resulting file is a valid FOX5 file.
\
If PRESERVE-COMPRESSED-P is true, then the compressed image data is not ~
removed from the file after writing the FOX5 file.")
  (with-output-to-binary (stream pathname)
    (with-fast-output (buffer stream)
      (unwind-protect
           (let ((*footer* (make-instance 'footer)))
             (ensure-compressed-images file)
             (write-command-block file buffer)
             (write-images file buffer)
             (write-footer buffer))
        (unless preserve-compressed-p
          (clean-compressed-images file))))
    pathname))
