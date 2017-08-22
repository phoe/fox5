;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; classes.lisp

(in-package #:fox5)

(defclass fox5-class ()
  ((%children :accessor children
              ;; :initform nil
              )))

(defclass file (fox5-class)
  ((%image-list :accessor image-list
                ;; :initform nil
                )
   (%generator :accessor generator
               ;; :initform 0
               )))

(defclass object (fox5-class)
  ((%object-id :accessor object-id
               ;; :initform -1
               )
   (%name :accessor name
          ;; :initform ""
          )
   (%description :accessor description
                 ;; :initform ""
                 )
   (%authors :accessor authors
             ;; :initform '()
             )
   (%revisions :accessor revisions
               ;; :initform 0
               )
   (%keywords :accessor keywords
              ;; :initform '()
              )
   (%license :accessor license
             ;; :initform :fc-by-sa
             )
   (%portal :accessor portal
            ;; :initform ""
            )
   (%edit-type :accessor edit-type
               ;; :initform 0
               )
   (%flags :accessor flags
           ;; :initform '()
           )
   (%more-flags :accessor more-flags
                ;; :initform 0
                ) ;; TODO parse this
   (%fx-filter :accessor fx-filter
               ;; :initform '(:target-layer :vb
               ;;             :blend-mode 0)
               )))

(defclass shape (fox5-class)
  ((%purpose :accessor purpose
             ;; :initform 0
             )
   (%direction :accessor direction
               ;; :initform nil
               )
   (%state :accessor state
           ;; :initform 0
           )
   (%ratio :accessor ratio
           ;; :initform '(0 0)
           )
   (%kitterspeak :accessor kitterspeak
                 ;; :initform ()
                 )))

(defclass frame (fox5-class)
  ((%frame-offset :accessor frame-offset
                  ;; :initform '(:x 0 :y 0)
                  )
   (%furre-offset :accessor furre-offset
                  ;; :initform '(:x 0 :y 0)
                  )))

(defclass sprite (fox5-class)
  ((%purpose :accessor purpose
             ;; :initform 0
             )
   (%image-id :accessor image-id
              ;; :initform 0
              )
   (%offset :accessor offset
            ;; :initform '(:x 0 :y 0)
            )))

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
          :initform nil)))
