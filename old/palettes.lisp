;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; palettes.lisp

(in-package :fox5)

(defun classic-palette-colors (&rest colors)
  (mapcar (lambda (x) (aref *classic-palette* x)) colors))

(defun color (type name)
  (check-type type color)
  (check-type name string)
  (gethash name (gethash type *colors*)))

(defun remap (r g b)
  (declare (ignore r))
  (if (= b 0)
      (remap-type g)
      nil))

(defun remap-type (g)
  (case g
    (1 '(badge)) (2 '(cape)) (3 '(eyes)) (4 '(markings))
    (6 '(vest)) (7 '(accent)) (9 '(bracers)) (10 '(wings))
    (12 '(trousers)) (13 '(hair)) (14 '(boots)) (15 '(fur))
    (255 '(outline))
    (t (error "Remap value ~D is invalid or unsupported." g)
     ;; (cond ((<= 16 g 23) `(fur-markings ,(- g 16)))
     ;;       ((<= 24 g 31) `(fur-hair ,(- g 24)))
     ;;       ((<= 32 g 47) `(markings-hair ,(- g 32)))
     ;;       (t (error "Remap value ~D is invalid." g)))
     )))

(defun make-gradient-data (color type)
  (check-type type color)
  (typecase color
    ((member eyes badge) (eyes-gradient-data color))
    (t (color-gradient-data color))))

(defun eyes-gradient-data (color)
  `(0 (0 0 0)
      1/2 ,(nth 3 color)
      1 (255 255 255)))

(defun gradient-data (color)
  (flet ((stops (&rest rest)
           (loop for (stop color) on rest by #'cddr
                 collect (list stop (mapcar #'round color)) into stops
                 finally (return (sort stops #'< :key #'car)))))
    (destructuring-bind (c0 c1 c2 c3 c4 c5 c6 c7) color
      (destructuring-bind (s0 s1 s2 s3 s4 s5 s6 s7) *gradient-color-stops*
        (stops
         0 '(0 0 0)
         (/ s0 5) (mapcar (lambda (x) (* x 1/3)) c0)
         (/ s0 2) (mapcar (lambda (x) (* x 4/7)) c0)
         s0 c0 s1 c1 s2 c2 s3 c3 s4 c4 s5 c5 s6 c6 s7 c7
         (/ (1+ (* s7 2)) 3) (mapcar (lambda (x) (/ (+ 255 (* x 5)) 6)) c7)
         (/ (+ s7 2) 3) (mapcar (lambda (x) (/ (+ x 255) 2)) c7)
         1 '(255 255 255))))))

(defun gradient (color &optional (length 256))
  (let ((data (gradient-data color))
        (height 1))
    (vecto:with-canvas (:width length :height height)
      (loop for ((fs1 c1) (fs2 c2)) on data
            for s1 = (round (* fs1 (1- length)))
            for s2 = (round (* (or fs2 1) (1- length)))
            for (r1 g1 b1) = (mapcar (curry #'* 1/255) c1)
            for (r2 g2 b2) = (mapcar (curry #'* 1/255) c2)
            while fs2
            do (vecto:set-gradient-fill s1 0 r1 g1 b1 1
                                        s2 0 r2 g2 b2 1)
               (vecto:rectangle s1 0 (1+ (- s2 s1)) height)
               (vecto:fill-path))
      (zpng:image-data (vecto::image vecto::*graphics-state*)))))
