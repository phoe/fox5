;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.fox1.asd

(asdf:defsystem #:fox5.fox1
  :description "Library for reading Furcadia FOX1 files"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:alexandria
               #:fast-io
               #:phoe-toolbox
               #:flexi-streams
               #:closer-mop
               #:fox5.base)
  :components ((:file "package")
               (:file "fox1")))
