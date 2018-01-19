;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.fs2.asd

(asdf:defsystem #:fox5.fs2
  :description "Library for reading Furcadia FS2 files"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:alexandria
               #:fast-io
               #:phoe-toolbox
               #:flexi-streams
               #:closer-mop
               #:cl-furcadia.remap
               #:fox5.base)
  :components ((:file "package")
               (:file "fs2")))
