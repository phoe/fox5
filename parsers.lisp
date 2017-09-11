;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; parsers.lisp

(in-package #:fox5)

;;; Generic commands

(define-parser (#x00 buffer))

(define-parser (#x4C buffer)
  (let* ((level (readu8-be buffer))
         (count (readu32-be buffer))
         (class (nth level *list-levels*))
         (*parent-object* *current-object*)
         (*current-object* (make-instance class)))
    (loop with i = 0
          for command = (code-char (readu8-be buffer))
          if (eql command #\<)
            do (incf i)
               (unless *parent-object*
                 (return *current-object*))
               (push *current-object* (children *parent-object*))
               (setf *current-object* (make-instance class))
               (when (= i count) (return))
          else do (parse-command command buffer))))

;;; File commands

(define-parser (#x67 buffer 'file)
  (let ((byte (readu8-be buffer)))
    (setf (generator *current-object*)
          (if (< byte 128)
              (list :furcadia byte)
              (list :third-party byte)))))

(define-parser (#x53 buffer 'file)
  (flet ((parse-image (buffer)
           (make-instance 'image :compressed-size (readu32-be buffer)
                                 :width (readu16-be buffer)
                                 :height (readu16-be buffer)
                                 :format (ecase (readu8-be buffer)
                                           (0 :8-bit) (1 :32-bit)))))
    (let ((n (readu32-be buffer)))
      (setf (image-list *current-object*)
            (loop repeat n collect (parse-image buffer))))))

;;; Object commands

(define-parser (#x72 buffer 'object)
  (setf (revisions *current-object*)
        (readu8-be buffer)))

(define-parser (#x61 buffer 'object)
  (setf (authors *current-object*)
        (loop repeat (readu16-be buffer)
              collect (read-string buffer))))

(define-parser (#x6C buffer 'object)
  (setf (license *current-object*)
        (let ((byte (readu8-be buffer)))
          (case byte
            (0 :fc-by-sa) (1 :fc0)
            (2 :fc-by-nc-sa) (3 :fc-nd-nc-sa)
            (4 :fc-private-sa) (5 :fc-by-x-sa)
            (t (list :reserved byte))))))

(define-parser (#x6B buffer 'object)
  (setf (keywords *current-object*)
        (loop repeat (readu16-be buffer)
              collect (read-string buffer))))

(define-parser (#x6E buffer 'object)
  (setf (name *current-object*)
        (read-string buffer)))

(define-parser (#x64 buffer 'object)
  (setf (description *current-object*)
        (read-string buffer)))

(define-parser (#x21 buffer 'object)
  (let ((flags '(:walkable :gettable :sittable :flyable
                 :swimmable :clickable :highlightable :kickable))
        (bitfield (readu8-be buffer)))
    (setf (flags *current-object*)
          (loop for i from 0
                for keyword in flags
                if (logbitp i bitfield)
                  collect keyword))))

(define-parser (#x50 buffer 'object)
  (setf (portal *current-object*)
        (read-string buffer :iso-8859-1)))

;; TODO interpret this in a later pass
(define-parser (#x3F buffer 'object)
  (setf (more-flags *current-object*)
        (readu32-be buffer)))

(define-parser (#x69 buffer 'object)
  (let ((byte (read32-be buffer)))
    (setf (object-id *current-object*)
          (if (= byte -1) :default byte))))

(define-parser (#x74 buffer 'object)
  (setf (edit-type *current-object*)
        (readu8-be buffer)))

(define-parser (#x46 buffer 'object)
  (flet ((blend-mode (byte)
           (ecase byte
             (0 nil) (1 :addition) (4 :darken) (5 :difference)
             (7 :hard-light) (8 :lighten) (9 :multiply) (10 :normal)
             (11 :overlay) (12 :screen) (14 :subtract) (15 :alpha)
             (16 :erase))))
    (setf (fx-filter *current-object*)
          (list :target-layer
                (ecase (readu8-be buffer) (0 :vb) (1 :sfx))
                :blend-mode (blend-mode (readu8-be buffer))))))

;;; Shape commands

(define-parser (#x70 buffer 'shape)
  (flet ((purpose (byte)
           (ecase byte
             (0 nil) (1 :menu-icon) (2 :ui-button) (3 :butler) (4 :portrait)
             (5 :ds-button) (11 :avatar) (21 :floor) (22 :item) (23 :wall)
             (24 :region) (25 :effect) (28 :pad-item) (29 :portal-item)
             (35 :specitag) (41 :lighting) (42 :ambience))))
    (setf (purpose *current-object*)
          (purpose (readu8-be buffer)))))

;; TODO interpret this in a later pass
(define-parser (#x73 buffer 'shape)
  (setf (state *current-object*)
        (readu8-be buffer)))

(define-parser (#x44 buffer 'shape)
  (flet ((dir (byte)
           (ecase byte
             (0 nil) (1 :sw) (2 :s) (3 :se) (4 :w) (5 :none) (6 :e)
             (7 :nw) (8 :n) (9 :ne) (10 :up) (11 :down))))
    (setf (direction *current-object*)
          (dir (readu8-be buffer)))))

(define-parser (#x52 buffer 'shape)
  (let ((numerator (readu8-be buffer))
        (denominator (readu8-be buffer)))
    (setf (ratio *current-object*)
          (list numerator denominator))))

(define-parser (#x4B buffer 'shape)
  (setf (kitterspeak *current-object*)
        (loop repeat (readu16-be buffer)
              collect (list (readu16-be buffer)
                            (read16-be buffer)
                            (read16-be buffer)))))

;;; Frame commands

(define-parser (#x6F buffer 'frame)
  (setf (frame-offset *current-object*)
        (list :x (read16-be buffer)
              :y (read16-be buffer))))

(define-parser (#x66 buffer 'frame)
  (setf (furre-offset *current-object*)
        (list :x (read16-be buffer)
              :y (read16-be buffer))))

;;; Sprite commands

(define-parser (#x43 buffer 'sprite)
  (flet ((pur (byte)
           (ecase (logand byte #xEF)
             (#x00 nil) (#x20 :remapping-data)
             (#x40 :shadow-layer) (#x80 :markup-layer))))
    (setf (purpose *current-object*)
          (pur (read16-be buffer)))))

(define-parser (#x63 buffer 'sprite)
  (setf (image-id *current-object*)
        (readu16-be buffer)))

(define-parser (#x4F buffer 'sprite)
  (setf (offset *current-object*)
        (list :x (read16-be buffer)
              :y (read16-be buffer))))
