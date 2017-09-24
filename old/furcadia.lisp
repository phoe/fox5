;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; furcadia.lisp

(in-package :fox5)

(defun from-220 (char)
  (let ((code (- (char-code char) 35)))
    (if (< 0 code 220)
        code
        0)))

(defun to-220 (number)
  (if (< 0 number 220)
      (code-char (+ number 35))
      #\#))

(defparameter *color-code-indices*
  '(version fur markings hair eyes badge vest bracers cape
    boots trousers wings accent gender species reserved))

(defparameter *genders*
  '(female male unspecified))

(defun color-code-gradient (type code)
  (check-type type color)
  (let* ((char-position (position type *color-code-indices*))
         (char (aref code char-position))
         (color-position (from-220 char))
         (names (gethash type *color-names*))
         (name (nth color-position names))
         (gradient (gethash (list type name) *gradients*)))
    (values gradient name)))

(defun all-gradients (&optional color-code)
  (let ((color-code (or color-code "w###############"))
        (result (make-hash-table)))
    (flet ((generate (type)
             (setf (gethash type result)
                   (multiple-value-list
                    (color-code-gradient type color-code)))))
      (mapc #'generate *color-types*)
      result)))

(defun remap (image-data &optional color-code)
  (check-type image-data vector)
  (check-type color-code (or null string))
  (let* ((gradients (all-gradients color-code))
         (length (length image-data))
         (result (make-array length :element-type '(unsigned-byte 8)
                                    :initial-contents image-data)))
    (loop for i from 0 below length by 4
          for b = (aref result (+ i 3))
          if (= b 0)
            do (setf (subseq result i (+ i 4))
                     (remap-argb (subseq result i (+ i 4)) gradients)))
    result))

(defun remap-argb (sequence gradients)
  (destructuring-bind (a r g b) (coerce sequence 'list)
    (assert (= b 0))
    (if (= g 255)
        (list 255 0 0 0)
        (let* ((type (gethash g *remap-color-values*))
               (gradient (first (gethash type gradients)))
               (offset (* r 4))
               (color (subseq gradient offset (+ offset 4))))
          (list a (aref color 0) (aref color 1) (aref color 2))))))
