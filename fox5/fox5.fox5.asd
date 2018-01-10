;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.fox5.asd

(asdf:defsystem #:fox5.fox5
  :description "Library for manipulating Furcadia FOX5 files"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:alexandria
               #:fast-io
               #:cl-lzma
               #:flexi-streams
               #:phoe-toolbox
               #:zpng
               #:vecto
               #:static-vectors
               #:trivial-garbage
               #:closer-mop)
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
               ))
