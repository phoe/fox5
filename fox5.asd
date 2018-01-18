;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.asd

(asdf:defsystem #:fox5
  :description "Library for manipulating Furcadia FOX5 files - full system"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:fox5.base
               #:fox5.base.qt
               #:fox5.fox5
               #:fox5.fox1
               #:fox5.fsh
               #:fox5.fs2
               #:fox5.gif-fox5)
  :components ((:file "package")))
