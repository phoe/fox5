;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; classes.lisp

(in-package :fox5/base)

(defclass fox5-class ()
  ((%children :initarg :children
              :accessor children
              :initform nil))
  (:documentation "Mixin for all FOX5 classes."))

(defclass file (fox5-class)
  (;; data-handling slots
   (%footer :initarg :footer
            :accessor footer)
   (%filepath :initarg :filepath
              :accessor filepath)
   ;; fox5 slots
   (%image-list :initarg :image-list
                :accessor image-list)
   (%generator :initarg :generator
               :accessor generator))
  (:documentation #.(format nil "FOX5 file class, symbolizing a complete, ~
parsed FOX5 file, including all images. Note that images may be loaded on ~
demand by #'DATA to conserve resources.")))

(defclass object (fox5-class)
  ((%id :accessor id
        :initarg :id)
   (%name :accessor name
          :initarg :name)
   (%description :accessor description)
   (%authors :initarg :authors
             :accessor authors)
   (%revisions :accessor revisions)
   (%keywords :accessor keywords)
   (%license :accessor license)
   (%portal :accessor portal)
   (%edit-type :initarg :edit-type
               :accessor edit-type)
   (%flags :initarg :flags
           :accessor flags)
   (%more-flags :accessor more-flags) ;; TODO parse this
   (%fx-filter :accessor fx-filter))
  (:documentation "FOX5 object class."))

(defclass shape (fox5-class)
  ((%purpose :initarg :purpose
             :accessor purpose)
   (%direction :initarg :direction
               :accessor direction)
   (%state :initarg :state
           :accessor state)
   (%ratio :initarg :ratio
           :accessor ratio)
   (%kitterspeak :initarg :kitterspeak
                 :accessor kitterspeak))
  (:documentation "FOX5 shape class."))

(defclass frame (fox5-class)
  ((%frame-offset :initarg :frame-offset
                  :accessor frame-offset
                  :initform '(:x 0 :y 0))
   (%furre-offset :initarg :furre-offset
                  :accessor furre-offset
                  :initform '(:x 0 :y 0)))
  (:documentation "FOX5 frame class."))

(defclass sprite (fox5-class)
  ((%purpose :initarg :purpose
             :accessor purpose)
   (%image-id :initarg :image-id
              :accessor image-id)
   (%offset :initarg :offset
            :accessor offset))
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
   (%format :accessor image-format
            :initarg :format)
   (%data :writer (setf data)
          :initarg :data))
  (:documentation "FOX5 image class."))

(defmethod compressed-size ((image image))
  (if (slot-boundp image 'compressed-data)
      (length (compressed-data image))
      (slot-value image '%compressed-size)))

(defmethod data ((image image))
  (when (and (not (slot-boundp image '%data))
             (slot-boundp (file image) '%filepath))
    (embed-image image))
  (slot-value image '%data))
