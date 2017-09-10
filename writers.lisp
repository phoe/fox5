;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; writers.lisp

(in-package #:fox5)

(defgeneric fox5-write (object buffer))

(defmethod fox5-write :after ((object fox5-class) buffer)
  (when-let ((children (children object)))
    (writeu8-be #x4C buffer)
    (writeu8-be (position (class-name (class-of object)) *list-levels*) buffer)
    (writeu32-be (length children) buffer)
    (mapc (rcurry #'fox5-write buffer) children))
  (fast-io:write8 #x3C buffer))

;;; File writers

(define-writer file generator (buffer)
  (writeu8-be #x67 buffer)
  (writeu8-be (second generator) buffer))

(define-writer file image-list (buffer)
  (writeu8-be #x53 buffer)
  (writeu32-be (length image-list) buffer)
  (loop for image in image-list
        do (with-accessors ((compressed-size compressed-size)
                            (width width) (height height) (format image-format))
               image
             (writeu32-be compressed-size buffer)
             (writeu16-be width buffer)
             (writeu16-be height buffer)
             (writeu8-be (ecase format (:8-bit 0) (:32-bit 1))
                         buffer))))

;;; Object writers

(define-writer object revisions (buffer)
  (writeu8-be #x72 buffer)
  (writeu8-be revisions buffer))

(define-writer object authors (buffer)
  (writeu8-be #x61 buffer)
  (flet ((octetize (string) (flexi-streams:string-to-octets
                             string :external-format :utf-8)))
    (loop for string in authors
          for octets = (octetize string)
          do (writeu16-be (length octets) buffer)
             (fast-write-sequence octets buffer))))

(define-writer object license (buffer)
  (writeu8-be #x6C buffer)
  (let ((byte (case license
                (:fc-by-sa 0) (:fc0 1)
                (:fc-by-nc-sa 2) (:fc-nd-nc-sa 3)
                (:fc-private-sa 4) (:fc-by-x-sa 5)
                (t (second license)))))
    (writeu8-be byte buffer)))

(define-writer object keywords (buffer)
  (writeu8-be #x6B buffer)
  (loop for string in keywords
        for octets = (octetize string)
        do (writeu16-be (length octets) buffer)
           (fast-write-sequence octets buffer)))

(define-writer object name (buffer)
  (writeu8-be #x6E buffer)
  (let ((octets (octetize name)))
    (writeu16-be (length octets) buffer)
    (fast-write-sequence octets buffer)))

(define-writer object description (buffer)
  (writeu8-be #x64 buffer)
  (let ((octets (octetize description)))
    (writeu16-be (length octets) buffer)
    (fast-write-sequence octets buffer)))

(define-writer object flags (buffer)
  (let ((list'(:walkable :gettable :sittable :flyable
               :swimmable :clickable :highlightable :kickable))
        (byte 0))
    (loop for flag in list
          do (setf byte (dpb 1 (byte 1 (position flag flags)) byte)))
    (writeu8-be #x21 buffer)
    (writeu8-be byte buffer)))

(define-writer object portal (buffer)
  (writeu8-be #x50 buffer)
  ;; TODO replace this with a write-string-to-buffer function everywhere
  (let ((octets (octetize portal :iso-8859-1)))
    (writeu16-be (length octets) buffer)
    (fast-write-sequence octets buffer)))

(define-writer object more-flags (buffer)
  (writeu8-be #x3F buffer)
  (writeu32-be more-flags buffer))

(define-writer object object-id (buffer)
  (writeu8-be #x69 buffer)
  (let ((byte (if (eq object-id :default) -1 object-id)))
    (writeu32-be byte buffer)))

(define-writer object edit-type (buffer)
  (writeu8-be #x74 buffer)
  (writeu8-be edit-type buffer))

(define-writer object fx-filter (buffer)
  (writeu8-be #x46 buffer)
  (let* ((target-layer (getf fx-filter :target-layer))
         (blend-mode (getf fx-filter :blend-mode))
         (target-layer (ecase target-layer (:vb 0) (:sfx 1)))
         (blend-mode (ecase blend-mode
                       ((nil) 0) (:addition 1) (:darken 4) (:difference 5)
                       (:hard-light 7) (:lighten 8) (:multiply 9) (:normal 10)
                       (:overlay 11) (:screen 12) (:subtract 14) (:alpha 15)
                       (:erase 16))))
    (writeu8-be target-layer buffer)
    (writeu8-be blend-mode buffer)))

;;; Shape writers

(define-writer shape purpose (buffer)
  (writeu8-be #x70 buffer)
  (let ((byte (case purpose
                ((nil) 0) (:menu-icon 1) (:ui-button 2) (:butler 3)
                (:portrait 4) (:ds-button 5) (:avatar 11) (:floor 21)
                (:item 22) (:wall 23) (:region 24) (:effect 25)
                (:pad-item 28) (:portal-item 29) (:specitag 35)
                (:lighting 41) (:ambience 42))))
    (writeu8-be byte buffer)))

(define-writer shape state (buffer)
  (writeu8-be #x73 buffer)
  (writeu8-be state buffer))

(define-writer shape direction (buffer)
  (writeu8-be #x44 buffer)
  (let ((byte (case direction
                ((nil) 0) (:sw 1) (:s 2) (:se 3) (:w 4) (:none 5) (:e 6)
                (:nw 7) (:n 8) (:ne 9) (:up 10) (:down 11))))
    (writeu8-be byte buffer)))

(define-writer shape ratio (buffer)
  (writeu8-be #x52 buffer)
  (writeu8-be (first ratio) buffer)
  (writeu8-be (second ratio) buffer))

(define-writer shape kitterspeak (buffer)
  (writeu8-be #x4B buffer)
  (writeu16-be (length kitterspeak) buffer)
  (loop for (i j k) in kitterspeak
        do (writeu16-be i buffer)
           (write16-be j buffer)
           (write16-be k buffer)))

;;; Frame writers

(define-writer frame frame-offset (buffer)
  (writeu8-be #x6F buffer)
  (write16-be (getf frame-offset :x) buffer)
  (write16-be (getf frame-offset :y) buffer))

(define-writer frame furre-offset (buffer)
  (writeu8-be #x66 buffer)
  (write16-be (getf furre-offset :x) buffer)
  (write16-be (getf furre-offset :y) buffer))

;;; Sprite writers

(define-writer sprite purpose (buffer)
  (writeu8-be #x43 buffer)
  (let ((byte (ecase purpose
                ((nil) #x00) (:remapping-data #x20)
                (:shadow-layer #x40) (:markup-layer #x80))))
    (write16-be byte buffer)))

(define-writer sprite image-id (buffer)
  (writeu8-be #x63 buffer)
  (writeu16-be image-id buffer))

(define-writer sprite offset (buffer)
  (writeu8-be #x4F buffer)
  (write16-be (getf offset :x) buffer)
  (write16-be (getf offset :y) buffer))
