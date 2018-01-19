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
   #:cl-furcadia/ws)
  (:shadow #:ratio)
  (:export
   ;; classes
   #:file #:object #:shape #:frame #:sprite #:image
   ;; accessors - generic
   #:children
   ;; accessors - file
   #:footer #:filepath #:image-list #:generator
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
   #:file #:compressed-size #:compressed-data #:width #:height #:image-format
   #:data
   ;; constants
   #:*fox5-footer-magic-string* #:*fox5-list-levels* #:*fox5-generator-number*
   ;; read/write
   #:read-fox5 #:write-fox5))
