;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; classes.lisp

(in-package #:fox5)

(defclass fox5-class ()
  ((%children :initarg :children
              :accessor children
              :initform nil)))

(defclass file (fox5-class)
  ((%image-list :initarg :image-list
                :accessor image-list)
   (%generator :initarg :generator
               :accessor generator)))

(defclass object (fox5-class)
  ((%object-id :accessor object-id
               :initarg :object-id)
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
   (%fx-filter :accessor fx-filter)))

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
                 :accessor kitterspeak)))

(defclass frame (fox5-class)
  ((%frame-offset :initarg :frame-offset
                  :accessor frame-offset
                  :initform '(:x 0 :y 0))
   (%furre-offset :initarg :furre-offset
                  :accessor furre-offset
                  :initform '(:x 0 :y 0))))

(defclass sprite (fox5-class)
  ((%purpose :initarg :purpose
             :accessor purpose)
   (%image-id :initarg :image-id
              :accessor image-id)
   (%offset :initarg :offset
            :accessor offset)))

(defclass image (fox5-class)
  ((%compressed-size :accessor compressed-size
                     :initarg :compressed-size)
   (%width :accessor width
           :initarg :width)
   (%height :accessor height
            :initarg :height)
   (%format :accessor image-format
            :initarg :format)
   (%data :accessor data
          :initarg :data)))
