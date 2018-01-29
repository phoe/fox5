;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; qt.lisp

(defpackage #:fox5/qt
  (:use
   #:cl+qt
   #:fox5
   #:qtools
   #:static-vectors
   #:phoe-toolbox
   #:alexandria
   #:cl-furcadia/remap
   #:cl-furcadia/constants)
  (:shadowing-import-from #:fox5 #:ratio #:parent)
  (:export
   #:display #:display2 #:display3 #:display4 #:display5
   #:*sample-color-codes*
   #:image-qpixmap))

(in-package :fox5/qt)
(in-readtable :qtools)

(defun display (image)
  (with-main-window (qlabel (q+:make-qlabel))
    (setf (q+:pixmap qlabel) (image-qpixmap image))))

(defun display2 (images)
  (with-main-window (qlabel (q+:make-qlabel "Test"))
    (loop for image in images
          for label = (q+:make-qlabel)
          do (setf (q+:pixmap label) (image-qpixmap image))
             (q+:show label))))

(defun display3 (images &rest color-codes)
  (with-main-window (main-window (make-instance 'qui:flow-layout))
    (loop for image in images
          do (loop for color-code in (cons nil color-codes)
                   for label = (q+:make-qlabel)
                   do (setf (q+:pixmap label) (image-qpixmap image color-code))
                      (qui:add-widget label main-window)))))

(defun display4 (images)
  (apply #'display3 images *sample-color-codes*))

(defun display5 (images &rest color-codes)
  (with-main-window (main-window (make-instance 'qui:flow-layout))
    (loop for image in images
          do (loop for color-code in color-codes
                   for label = (q+:make-qlabel)
                   ;; TODO optimize, right now the gradients are recalculated every time
                   do (setf (q+:pixmap label) (image-qpixmap image color-code))
                      (qui:add-widget label main-window)))))

;;; TODO write something that utilizes REMAP-ALL

(defvar *sample-color-codes*
  '("w#'''<1)1%9###'#" "w1/?9?#,,?55?$*#" "w0#2$8(4*')'7#-#" "w223&0++*++###(#"
    "w%&L=J;;;;;###'#" "w&#L&@(=(='###$#" "w88C3I58::<###'#" "w88C<I88::<##$-#"
    "w&%K=?=;;;;###'#" "w'-(?1'9)=9###)#" "w(;/?H';';'##%&#" "w/5='#?@;(+7<#'#"
    "w:#L7:;=:;;/;#'#" "w--+>97894%###&#" "w%&M$*<;;;;###%#" "w88M9/@@@@@##%$#"
    "w7//1&*&#####$(#" "w88M9/<:<<<###)#" "w%%K<.;(;;;###&#" "w8(,6D':>84+.$%#"
    "w6$121*3:7<1%$&#" "w,(=-;;.49$45#-#" "w%%K##;;;;;##%&#")
  "Some valid color codes for debugging, scavenged off my Furcadia characters.")

(defun image-qpixmap (image &optional color-code)
  (let* ((data (if color-code
                   (remap color-code (data image))
                   (data image)))
         (length (array-total-size data)))
    (with-static-vector (vector length :initial-contents data)
      (with-finalizing ((qimage (qimage-from image vector)))
        (q+:qpixmap-from-image qimage)))))

(defun qimage-from (image vector)
  (q+:make-qimage (static-vector-pointer vector)
                  (width image) (height image)
                  (q+:qimage.format_argb32)))
