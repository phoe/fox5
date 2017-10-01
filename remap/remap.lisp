;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; remap.lisp

(in-package :fox5)

(defun from-220 (char)
  "Converts a character into a Furcadia base-220 integer."
  (let ((code (- (char-code char) 35)))
    (if (< 0 code 220)
        code
        0)))

(defun to-220 (number)
  "Converts a Furcadia base-220 integer into a character."
  (if (< 0 number 220)
      (code-char (+ number 35))
      #\#))

(defun color-code-gradient (type code)
  "Given a color type and a color code, returns the respective gradient."
  (check-type type color)
  (let* ((char-position (position type *color-code-indices*))
         (char (aref code char-position))
         (color-position (from-220 char))
         (names (gethash type *color-names*))
         (name (nth color-position names))
         (gradient (gethash (list type name) *gradients*)))
    (values gradient name)))

(defun all-gradients (color-code)
  "Provided a color code, returns a fresh hashtable containing a map between
all valid color types and their respective gradients."
  (let ((result (make-hash-table)))
    (flet ((generate (type)
             (setf (gethash type result)
                   (multiple-value-list
                    (color-code-gradient type color-code)))))
      (mapc #'generate *color-types*)
      result)))

(defun remap (image-data color-code)
  "Provided an ARGB image data and a color code, returns a fresh copy of the
image data with all eligible pixels remapped."
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
