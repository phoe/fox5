;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; read-write-file.lisp

(in-package :fox5)

(defun read-fox5 (pathname)
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
      (mapc #'embed-image (images file))
      (embed-images-into-sprites file)
      file)))

(defun write-fox5-stream (file stream &optional preserve-compressed-p)
  ;; TODO description here
  (regenerate-image-list file)
  (with-fast-output (buffer stream)
    (unwind-protect
         (let ((*footer* (make-instance 'footer)))
           (ensure-compressed-images file)
           (write-command-block file buffer)
           (write-images file buffer)
           (write-footer buffer))
      (unless preserve-compressed-p
        (clean-compressed-images file)))))

(defun write-fox5 (file pathname &optional preserve-compressed-p)
  #.(format nil "Writes the provided FOX5 file object into a file with the ~
provided pathname. The resulting file is a valid FOX5 file.
\
If PRESERVE-COMPRESSED-P is true, then the compressed image data is not ~
removed from the file after writing the FOX5 file.")
  (with-output-to-binary (stream pathname)
    (write-fox5-stream file stream preserve-compressed-p)
    pathname))
