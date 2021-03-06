;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:fox5
  (:use
   #:cl
   #:fast-io
   #:phoe-toolbox
   #:alexandria
   #:cl-lzma
   #:static-vectors
   #:trivial-garbage
   #:cl-furcadia/remap
   #:cl-furcadia/constants)
  (:shadow #:ratio)
  (:export
   ;; classes
   #:file #:object #:shape #:frame #:sprite #:image
   ;; accessors - generic
   #:children #:parent
   ;; accessors - file
   #:footer #:filepath #:images #:generator
   ;; accessors - object
   #:id #:name #:description #:authors #:revisions #:keywords #:license
   #:portal #:edit-type #:flags #:more-flags #:fx-filter
   ;; accessors - shape
   #:purpose #:direction #:state #:ratio #:kitterspeak
   ;; accessors - frame
   #:frame-offset #:furre-offset
   ;; accessors - sprite
   #:purpose #:image-id #:offset
   ;; accessors - image
   #:file #:compressed-size #:compressed-data #:width #:height #:data
   ;; constants
   #:*fox5-footer-magic-string* #:*fox5-list-levels* #:*fox5-generator-number*
   ;; shape types
   #:shape-type #:*shape-types*
   ;; read/write
   #:read-fox5 #:write-fox5
   ;; utils
   #:parent-push))
