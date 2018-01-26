;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; parsers.lisp

(in-package :fox5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic commands

;;; Base writing mechanism
(defmethod write-fox5-to-buffer ((object fox5-class) buffer)
  "When a FOX5 object is being written, all of its bound slots must be written
to the file in form of FOX5 commands."
  (mapc (lambda (x) (write-command object x buffer))
        (bound-slots-values object)))

(defmethod write-fox5-to-buffer :after ((object fox5-class) buffer)
  "We do not create a write method for children of a FOX5 object. Rather, we
create an :AFTER method that:
1) writes a list of all children, if present, with a proper list level,
2) closes the list element with a #x3C."
  (when-let ((children (children object)))
    (writeu8-be #x4C buffer)
    (let* ((class-name (class-name (class-of object)))
           (position (1+ (position class-name *fox5-list-levels*))))
      (writeu8-be position buffer))
    (writeu32-be (length children) buffer)
    (mapc (rcurry #'write-fox5-to-buffer buffer) children))
  (writeu8-be #x3C buffer))

(defmethod write-fox5-to-buffer ((object file) buffer)
  "This method initializes the file writing, since writing the file object
requires a slightly different technique."
  (dotimes (i 4)
    (writeu8-be #x00 buffer))
  (writeu8-be #x4C buffer)
  (let* ((class-name (class-name (class-of object)))
         (position (position class-name *fox5-list-levels*)))
    (writeu8-be position buffer)
    (writeu32-be 1 buffer)
    (mapc (lambda (x) (write-command object x buffer))
          '(%images %generator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic commands

;;; #x00 Generic > Null Command
(define-fox5-reader (#x00 buffer))

;;; #x4C Generic > List Start
(define-fox5-reader (#x4C buffer)
  (let* ((level (readu8-be buffer))
         (count (readu32-be buffer))
         (class (nth level *fox5-list-levels*))
         (*parent-object* *current-object*)
         (*current-object* (make-instance class)))
    (prog1 (loop with i = 0
                 for command = (code-char (readu8-be buffer))
                 if (eql command (code-char #x3C))
                   do (incf i)
                      (unless *parent-object* (return *current-object*))
                      (push *current-object* (children *parent-object*))
                      (setf *current-object* (make-instance class))
                      (setf (parent *current-object*) *parent-object*)
                      (when (= i count) (return))
                 else do (read-command command buffer))
      (setf (children *current-object*)
            (nreverse (children *current-object*))))))

(define-fox5-reader (#x3C buffer)
  (error "Unexpected #x3C (#\<) encountered when reading."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File commands

;;; File > Generator
(define-fox5-reader (#x67 buffer 'file)
  (let ((byte (readu8-be buffer)))
    (setf (generator *current-object*)
          (if (< byte 128)
              (list :furcadia byte)
              (list :third-party byte)))))

(define-fox5-writer (file generator buffer)
  (writeu8-be #x67 buffer)
  (writeu8-be (second generator) buffer))

;;; File > Images
(define-fox5-reader (#x53 buffer 'file)
  (flet ((parse-image (buffer)
           (prog1 (make-instance 'image :file *current-object*
                                        :compressed-size (readu32-be buffer)
                                        :width (readu16-be buffer)
                                        :height (readu16-be buffer))
             (readu8-be buffer))))
    (let ((n (readu32-be buffer)))
      (setf (images *current-object*)
            (loop repeat n collect (parse-image buffer))))))

(define-fox5-writer (file images buffer)
  (writeu8-be #x53 buffer)
  (writeu32-be (length images) buffer)
  (loop for image in images
        do (with-accessors ((compressed-size compressed-size) (width width)
                            (height height))
               image
             (writeu32-be compressed-size buffer)
             (writeu16-be width buffer)
             (writeu16-be height buffer)
             (writeu8-be 1 buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object commands

;;; Object > Revisions
(define-fox5-reader (#x72 buffer 'object)
  (setf (revisions *current-object*)
        (readu8-be buffer)))

(define-fox5-writer (object revisions buffer)
  (writeu8-be #x72 buffer)
  (writeu8-be revisions buffer))

;;; Object > Authors
(define-fox5-reader (#x61 buffer 'object)
  (setf (authors *current-object*)
        (loop repeat (readu16-be buffer)
              collect (read-string buffer))))

(define-fox5-writer (object authors buffer)
  (writeu8-be #x61 buffer)
  (writeu16-be (length authors) buffer)
  (flet ((octetize (string) (flexi-streams:string-to-octets
                             string :external-format :utf-8)))
    (loop for string in authors
          for octets = (octetize string)
          do (writeu16-be (length octets) buffer)
             (fast-write-sequence octets buffer))))

;;; Object > License
(defparameter *fox5-object-license*
  '((0 . :fc-by-sa) (1 . :fc0)
    (2 . :fc-by-nc-sa) (3 . :fc-nd-nc-sa)
    (4 . :fc-private-sa) (5 . :fc-by-x-sa)))

(define-fox5-reader (#x6C buffer 'object)
  (setf (license *current-object*)
        (let ((byte (readu8-be buffer)))
          (case byte
            (0 :fc-by-sa) (1 :fc0)
            (2 :fc-by-nc-sa) (3 :fc-nd-nc-sa)
            (4 :fc-private-sa) (5 :fc-by-x-sa)
            (t (list :reserved byte))))))

(define-fox5-writer (object license buffer)
  (writeu8-be #x6C buffer)
  (let ((byte (case license
                (:fc-by-sa 0) (:fc0 1)
                (:fc-by-nc-sa 2) (:fc-nd-nc-sa 3)
                (:fc-private-sa 4) (:fc-by-x-sa 5)
                (t (second license)))))
    (writeu8-be byte buffer)))

;;; Object > Keywords
(define-fox5-reader (#x6B buffer 'object)
  (setf (keywords *current-object*)
        (loop repeat (readu16-be buffer)
              collect (read-string buffer))))

(define-fox5-writer (object keywords buffer)
  (writeu8-be #x6B buffer)
  (writeu16-be (length keywords) buffer)
  (loop for string in keywords
        for octets = (octetize string)
        do (writeu16-be (length octets) buffer)
           (fast-write-sequence octets buffer)))

;;; Object > Name
(define-fox5-reader (#x6E buffer 'object)
  (setf (name *current-object*)
        (read-string buffer)))

(define-fox5-writer (object name buffer)
  (writeu8-be #x6E buffer)
  (let ((octets (octetize name)))
    (writeu16-be (length octets) buffer)
    (fast-write-sequence octets buffer)))

;;; Object > Description
(define-fox5-reader (#x64 buffer 'object)
  (setf (description *current-object*)
        (read-string buffer)))

(define-fox5-writer (object description buffer)
  (writeu8-be #x64 buffer)
  (let ((octets (octetize description)))
    (writeu16-be (length octets) buffer)
    (fast-write-sequence octets buffer)))

;;; Object > Flags
(defparameter *fox5-object-flags*
  '(:walkable :gettable :sittable :flyable
    :swimmable :clickable :highlightable :kickable))

(define-fox5-reader (#x21 buffer 'object)
  (let ((bitfield (readu8-be buffer)))
    (setf (flags *current-object*)
          (loop for i from 0
                for keyword in *fox5-object-flags*
                if (logbitp i bitfield)
                  collect keyword))))

(define-fox5-writer (object flags buffer)
  (let ((byte 0))
    (loop for flag in flags
          for position = (position flag *fox5-object-flags*)
          do (setf byte (dpb 1 (byte 1 position) byte)))
    (writeu8-be #x21 buffer)
    (writeu8-be byte buffer)))

;;; Object > Portal
(define-fox5-reader (#x50 buffer 'object)
  (setf (portal *current-object*)
        (read-string buffer :iso-8859-1)))

(define-fox5-writer (object portal buffer)
  (writeu8-be #x50 buffer)
  ;; TODO replace this with a write-string-to-buffer function everywhere
  (let ((octets (octetize portal :iso-8859-1)))
    (writeu16-be (length octets) buffer)
    (fast-write-sequence octets buffer)))

;;; Object > More Flags
;; TODO interpret this in a later pass
(define-fox5-reader (#x3F buffer 'object)
  (setf (more-flags *current-object*)
        (readu32-be buffer)))

(define-fox5-writer (object more-flags buffer)
  (writeu8-be #x3F buffer)
  (writeu32-be more-flags buffer))

;;; Object > ID
(define-fox5-reader (#x69 buffer 'object)
  (let ((byte (read32-be buffer)))
    (setf (id *current-object*)
          (if (= byte -1) :default byte))))

(define-fox5-writer (object id buffer)
  (writeu8-be #x69 buffer)
  (let ((byte (if (eq id :default) -1 id)))
    (write32-be byte buffer)))

;;; Object > Edit Type
(defparameter *fox5-object-edit-type*
  '((0 . nil) (1 . :floor) (2 . :item) (3 . :effect) (4 . :portrait-set)
    (5 . :avatar) (6 . :gendered-avatar) (7 . :region) (8 . :wall)
    (10 . :lighting) (11 . :ambience) (12 . :button) (13 . :ds-button)
    (14 . :system) (15 . :portal)))

(define-fox5-reader (#x74 buffer 'object)
  (setf (edit-type *current-object*)
        (assoc-value-or-die *fox5-object-edit-type* (readu8-be buffer))))

(define-fox5-writer (object edit-type buffer)
  (writeu8-be #x74 buffer)
  (let ((edit-type (rassoc-value-or-die *fox5-object-edit-type* edit-type)))
    (writeu8-be edit-type buffer)))

;;; Object > FX Filter
(defparameter *fox5-object-fx-filter*
  '((0 . nil) (1 . :addition) (4 . :darken) (5 . :difference) (7 . :hard-light)
    (8 . :lighten) (9 . :multiply) (10 . :normal) (11 . :overlay)
    (12 . :screen) (14 . :subtract) (15 . :alpha) (16 . :erase)))

(define-fox5-reader (#x46 buffer 'object)
  (flet ((blend-mode (byte) (assoc-value-or-die *fox5-object-fx-filter* byte)))
    (setf (fx-filter *current-object*)
          (list :target-layer (ecase (readu8-be buffer) (0 :vb) (1 :sfx))
                :blend-mode (blend-mode (readu8-be buffer))))))

(define-fox5-writer (object fx-filter buffer)
  (writeu8-be #x46 buffer)
  (let* ((target-layer (getf fx-filter :target-layer))
         (blend-mode (getf fx-filter :blend-mode))
         (target-layer (ecase target-layer (:vb 0) (:sfx 1)))
         (blend-mode (rassoc-value-or-die *fox5-object-fx-filter* blend-mode)))
    (writeu8-be target-layer buffer)
    (writeu8-be blend-mode buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shape commands

;;; Shape > Purpose
(defparameter *fox5-shape-purpose*
  '((0 . nil)
    (1 . :menu-icon) (2 . :ui-button) (3 . :butler) (4 . :portrait)
    (5 . :ds-button)
    (11 . :avatar) (12 . :attachment)
    (21 . :floor) (22 . :item) (23 . :wall) (24 . :region) (25 . :effect)
    (26 . :lighting) (27 . :magic) (28 . :pad-item) (29 . :portal-item)
    (31 . :desctag) (34 . :grouptag) (35 . :specitag) (36 . :for-life-tag)
    (37 . :smiley) (38 . :font)
    (41 . :lighting) (42 . :ambience)
    (255 . :do-not-show)))

(define-fox5-reader (#x70 buffer 'shape)
  (flet ((purpose (byte) (assoc-value-or-die *fox5-shape-purpose* byte)))
    (setf (purpose *current-object*)
          (purpose (readu8-be buffer)))))

(define-fox5-writer (shape purpose buffer)
  (writeu8-be #x70 buffer)
  (let ((byte (rassoc-value-or-die *fox5-shape-purpose* purpose)))
    (writeu8-be byte buffer)))

;;; Shape > State
;; TODO interpret this in a later pass
(define-fox5-reader (#x73 buffer 'shape)
  (setf (state *current-object*)
        (readu8-be buffer)))

(define-fox5-writer (shape state buffer)
  (writeu8-be #x73 buffer)
  (writeu8-be state buffer))

;;; Shape > Direction
(defparameter *fox5-shape-direction*
  '((0 . nil) (1 . :sw) (2 . :s) (3 . :se) (4 . :w) (5 . :none)
    (6 . :e) (7 . :nw) (8 . :n) (9 . :ne) (10 . :up) (11 . :down)))

(define-fox5-reader (#x44 buffer 'shape)
  (flet ((dir (byte) (assoc-value-or-die *fox5-shape-direction* byte)))
    (setf (direction *current-object*)
          (dir (readu8-be buffer)))))

(define-fox5-writer (shape direction buffer)
  (writeu8-be #x44 buffer)
  (let ((byte (rassoc-value-or-die *fox5-shape-direction* direction)))
    (writeu8-be byte buffer)))

;;; Shape > Ratio
(define-fox5-reader (#x52 buffer 'shape)
  (let ((numerator (readu8-be buffer))
        (denominator (readu8-be buffer)))
    (setf (ratio *current-object*)
          (list numerator denominator))))

(define-fox5-writer (shape ratio buffer)
  (writeu8-be #x52 buffer)
  (writeu8-be (first ratio) buffer)
  (writeu8-be (second ratio) buffer))

;;; Shape > Kitterspeak
(define-fox5-reader (#x4B buffer 'shape)
  (setf (kitterspeak *current-object*)
        (loop repeat (readu16-be buffer)
              collect (list (assoc-value-or-die *kitterspeak*
                                                (readu16-be buffer))
                            (read16-be buffer)
                            (read16-be buffer)))))

(define-fox5-writer (shape kitterspeak buffer)
  (writeu8-be #x4B buffer)
  (writeu16-be (length kitterspeak) buffer)
  (loop for (i j k) in kitterspeak
        do (writeu16-be (rassoc-value-or-die *kitterspeak* i) buffer)
           (write16-be j buffer)
           (write16-be k buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame commands

;;; Frame > Frame Offset
(define-fox5-reader (#x6F buffer 'frame)
  (setf (frame-offset *current-object*)
        (list :x (read16-be buffer)
              :y (read16-be buffer))))

(define-fox5-writer (frame frame-offset buffer)
  (writeu8-be #x6F buffer)
  (write16-be (getf frame-offset :x) buffer)
  (write16-be (getf frame-offset :y) buffer))

;;; Frame > Furre Offset
(define-fox5-reader (#x66 buffer 'frame)
  (setf (furre-offset *current-object*)
        (list :x (read16-be buffer)
              :y (read16-be buffer))))

(define-fox5-writer (frame furre-offset buffer)
  (writeu8-be #x66 buffer)
  (write16-be (getf furre-offset :x) buffer)
  (write16-be (getf furre-offset :y) buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sprite commands

;;; Sprite > Purpose
(defparameter *fox5-sprite-purpose*
  '((#x00 . nil) (#x20 . :remapping-data)
    (#x40 . :shadow-layer) (#x80 . :markup-layer)))

(define-fox5-reader (#x43 buffer 'sprite)
  (flet ((pur (byte) (assoc-value-or-die *fox5-sprite-purpose*
                                         (logand byte #xEF))))
    (setf (purpose *current-object*)
          (pur (read16-be buffer)))))

(define-fox5-writer (sprite purpose buffer)
  (writeu8-be #x43 buffer)
  (let ((byte (rassoc-value-or-die *fox5-sprite-purpose* purpose)))
    (write16-be byte buffer)))

;;; Sprite > Image ID
(define-fox5-reader (#x63 buffer 'sprite)
  (setf (image-id *current-object*)
        (readu16-be buffer)))

(define-fox5-writer (sprite image-id buffer)
  (writeu8-be #x63 buffer)
  (writeu16-be image-id buffer))

;;; Sprite > Offset
(define-fox5-reader (#x4F buffer 'sprite)
  (setf (offset *current-object*)
        (list :x (read16-be buffer)
              :y (read16-be buffer))))

(define-fox5-writer (sprite offset buffer)
  (writeu8-be #x4F buffer)
  (write16-be (getf offset :x) buffer)
  (write16-be (getf offset :y) buffer))
