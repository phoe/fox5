;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:fox5/fox1
  (:use
   #:cl
   #:fast-io
   #:phoe-toolbox
   #:alexandria
   #:cl-furcadia/constants
   #:fox5/base)
  (:export
   ;; file
   #:file #:version #:nshapes #:generator #:encryption #:shapes
   ;; shape
   #:shape #:walkablep #:gettablep #:sittablep #:index #:frames #:steps
   ;; frame
   #:frame #:width #:height #:position-x #:position-y #:furre-position-x
   #:furre-position-y #:image-format #:image
   ;; functions
   #:read-fox1))
