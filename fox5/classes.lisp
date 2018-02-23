;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; classes.lisp

(in-package :fox5)

(defclass fox5-class ()
  ((%children :initarg :children
              :accessor children
              :initform '())
   (%parent :initarg :parent
            :accessor parent
            :initform nil))
  (:documentation "Mixin for all FOX5 classes."))

(defclass file (fox5-class)
  (;; data-handling slots
   (%footer :initarg :footer
            :accessor footer)
   (%filepath :initarg :filepath
              :accessor filepath)
   ;; fox5 slots
   (%images :initarg :images
            :accessor images
            :initform '())
   (%generator :initarg :generator
               :accessor generator
               :initform (list :third-party *fox5-generator-number*)))
  (:documentation "FOX5 file class, symbolizing a complete, parsed FOX5 file,
including all images. Note that images may be loaded on demand by #'DATA to
conserve resources."))

(defclass object (fox5-class)
  ((%id :accessor id
        :initarg :id
        :initform -1)
   (%name :accessor name
          :initarg :name
          :initform "")
   (%description :accessor description
                 :initform "")
   (%authors :initarg :authors
             :accessor authors
             :initform '())
   (%revisions :accessor revisions
               :initform 0)
   (%keywords :accessor keywords
              :initform '())
   (%license :accessor license
             :initform :fc-by-sa)
   (%portal :accessor portal
            :initform "")
   (%edit-type :initarg :edit-type
               :accessor edit-type
               :initform nil)
   (%flags :initarg :flags
           :accessor flags
           :initform '())
   (%more-flags :accessor more-flags
                :initform 0) ;; TODO parse and print this in PRINT-OBJECT OBJECT
   (%fx-filter :accessor fx-filter
               :initform (list :target-layer :vb :blend-mode nil)))
  (:documentation "FOX5 object class."))

(defclass shape (fox5-class)
  ((%purpose :initarg :purpose
             :accessor purpose
             :initform nil)
   (%direction :initarg :direction
               :accessor direction
               :initform nil)
   (%state :initarg :state
           :accessor state
           :initform 0)
   (%ratio :initarg :ratio
           :accessor ratio
           :initform '(0 0))
   (%kitterspeak :initarg :kitterspeak
                 :accessor kitterspeak
                 :initform '()))
  (:documentation "FOX5 shape class."))

(defclass frame (fox5-class)
  ((%frame-offset :initarg :frame-offset
                  :accessor frame-offset
                  :initform (list :x 0 :y 0))
   (%furre-offset :initarg :furre-offset
                  :accessor furre-offset
                  :initform (list :x 0 :y 0)))
  (:documentation "FOX5 frame class."))

(defclass sprite (fox5-class)
  ((%purpose :initarg :purpose
             :accessor purpose
             :initform 0)
   (%image-id :initarg :image-id
              :accessor image-id
              :initform nil)
   (%image :initarg :image
           :accessor image
           :initform nil)
   (%offset :initarg :offset
            :accessor offset
            :initform (list :x 0 :y 0)))
  (:documentation "FOX5 sprite class."))

(defclass image (fox5-class)
  ((%file :accessor file
          :initarg :file)
   (%compressed-size :writer (setf compressed-size)
                     :initarg :compressed-size)
   (%compressed-data :accessor compressed-data
                     :initarg :compressed-data)
   (%width :accessor width
           :initarg :width)
   (%height :accessor height
            :initarg :height)
   (%data :writer (setf data)
          :initarg :data))
  (:documentation "FOX5 image class."))

(defmethod compressed-size ((image image))
  "Method that falls back to computing the length of COMPRESSED-DATA if the
compressed size slot is not set."
  (if (slot-boundp image '%compressed-data)
      (length (compressed-data image))
      (slot-value image '%compressed-size)))

(defmethod data ((image image))
  "Method automatically embedding image data into FOX5 files, in case it has not
been embedded before."
  (when (and (not (slot-boundp image '%data))
             (slot-boundp (file image) '%filepath))
    (embed-image image))
  (slot-value image '%data))

(defmethod (setf data) :after (new-value (image image))
  (slot-makunbound image '%compressed-data))
