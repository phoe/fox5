;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.asd

(asdf:defsystem #:fox5
  :description "Library for encoding/decoding/manipulating Furcadia FOX5 files"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:fast-io
               #:cl-lzma
               #:flexi-streams
               #:zpng
               #:vecto
               #:closer-mop
               #:skippy
               #:alexandria)
  :components ((:file "package")
               (:file "classes")
               (:file "utils")
               (:file "constants")
               ;; READ-WRITE
               (:file "read-write/footer")
               (:file "read-write/command-block")
               (:file "read-write/images")
               (:file "read-write/framework")
               (:file "read-write/parsers")
               (:file "read-write/file")
               ;; GIF-FOX5
               (:file "gif-fox5/skippy-renderer")
               (:file "gif-fox5/gif-fox5")
               ;; REMAP
               (:file "remap/constants")
               (:file "remap/remap")
               ))
