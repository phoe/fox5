;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.fsh.asd

(asdf:defsystem #:fox5.fsh
  :description "Library for reading Furcadia FSH files"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:alexandria
               #:fast-io
               #:phoe-toolbox
               #:flexi-streams
               #:closer-mop)
  :components ((:file "package")
               (:file "fsh")))
