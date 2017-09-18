;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; palettes.lisp

(in-package :fox5)

(defun palette-colors (&rest colors)
  (mapcar (lambda (x) (aref *classic-palette* x)) colors))

(defun color (type name)
  (check-type type color)
  (check-type name string)
  (gethash name (gethash type *colors*)))

(defun remap (r g b)
  (if (= b 0)
      (cons r (remap-type g))
      nil))

(defun remap-type (g)
  (case g
    (1 '(:badge)) (2 '(:cape)) (3 '(:eyes)) (4 '(:markings))
    (6 '(:vest)) (7 '(:accent)) (9 '(:bracers)) (10 '(:wings))
    (12 '(:trousers)) (13 '(:hair)) (14 '(:boots)) (15 '(:fur))
    (255 '(:outline))
    (t (cond ((<= 16 g 23) `(:fur-markings ,(- g 16)))
             ((<= 24 g 31) `(:fur-hair ,(- g 24)))
             ((<= 32 g 47) `(:markings-hair ,(- g 32)))
             (t (error "Remap value ~D is invalid." g))))))
