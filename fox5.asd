;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.asd

(asdf:defsystem #:fox5
  :description "Library for encoding/decoding Furcadia FOX5 format"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "MIT"
  :serial t
  :depends-on (#:fast-io
               #:cl-lzma
               #:flexi-streams
               #:alexandria)
  :components ((:file "package")
               (:file "fox5")))
