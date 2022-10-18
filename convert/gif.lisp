;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; gif.lisp

(in-package :fox5)

(export 'read-gif :fox5)

(defun read-gif (pathname &key remapp)
  (unless (pathnamep pathname) (setf pathname (pathname pathname)))
  (let ((gif (skippy:load-data-stream pathname)))
    (destructuring-bind (images ks) (gif-images gif)
      (gif-fox5 images ks :remapp remapp))))

(defun gif-images (gif)
  (multiple-value-bind (vectors delays data)
      (skippy-renderer:render gif :byte-order :bgra)
    (destructuring-bind (width height loopingp) data
      (declare (ignore loopingp))
      (flet ((ks (i delay) (let ((delay (if (= 0 delay) 100 (* 10 delay))))
                             `((:move-behind ,i 0) (:delay ,delay 0))))
             (make-image (data)
               (make-instance 'image :width width :height height :data data)))
        (list (mapcar #'make-image vectors)
              (loop for i below (length delays)
                    append (ks i (nth i delays))))))))

(defun gif-fox5 (images ks &key remapp)
  (loop with purpose = (if remapp :remapping-data nil)
        with file = (make-instance 'file :images images)
        with object = (make-instance 'object)
        with shape = (make-instance 'shape :kitterspeak ks)
        for image in images
        for i from 1
        for frame = (make-instance 'frame)
        for sprite = (make-instance 'sprite :image-id i :purpose purpose)
        do (parent-push shape frame)
           (parent-push frame sprite)
        finally (nreversef (children shape))
                (parent-push object shape)
                (parent-push file object)
                (embed-images-into-sprites file)
                (return file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML interface

(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))

(defparameter *html* "
<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <title>fox5-web</title>
  </head>
  <body>
    <h1>Furcadia GIF => FOX5 Portrait Converter</h1>
    <form action=\"submit\" method=\"post\"
          enctype=\"multipart/form-data\"
          accept=\"image/gif\">
      <p>Select image to upload:</p>
      <input type=\"file\" name=\"file\" id=\"file\"><br />
      <input type=\"checkbox\" id=\"remap\" name=\"remap\" value=\"Remappable\">
      <label for=\"vehicle1\">Remappable</label>
      <br />
      <input type=\"submit\" value=\"Convert!\" name=\"submit\">
    </form>
  </body>
</html>
")

(hunchentoot:define-easy-handler (serve-index :uri "/") ()
  *html*)

(hunchentoot:define-easy-handler (submit :uri "/submit") (remapp)
  (let ((file (hunchentoot:post-parameter "file")))
    (if (null file)
        "Please specify a file!"
        (handler-case
            (destructuring-bind (pathname filename content-type) file
              (declare (ignore content-type))
              (prog1 (flex:with-output-to-sequence (stream)
                       (let ((gif (read-gif pathname :remapp remapp)))
                         (write-fox5-stream gif stream)))
                (setf (hunchentoot:content-type*) "application/force-download")
                (setf (hunchentoot:header-out :content-disposition)
                      (format nil "attachment; filename=~A.fox" filename))))
          (error (e)
            (setf (hunchentoot:return-code*) 500)
            (format nil "Error: ~A" e))))))
