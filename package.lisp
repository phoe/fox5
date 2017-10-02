;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:fox5
  (:use #:cl
        #:fast-io
        #:alexandria
        #:cl-lzma)
  (:shadow #:ratio))

(defpackage #:fox5/gif-fox5
  (:use #:cl
        #:fox5))
