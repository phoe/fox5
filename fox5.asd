;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.asd

(asdf:defsystem #:fox5
  :description "Library for encoding/decoding Furcadia FOX5 format"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:fast-io
               #:cl-lzma
               #:flexi-streams
               #:zpng
               #:closer-mop
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
               (:file "gif")
               (:file "writers")
               (:file "fox5")))
