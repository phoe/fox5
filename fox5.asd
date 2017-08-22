;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
               #:zpng
               #:skippy
               #:alexandria)
  :components ((:file "package")
               (:file "constants")
               (:file "utils")
               (:file "header")
               (:file "classes")
               (:file "command-block")
               (:file "parsers")
               (:file "images")
               (:file "fox5")))
