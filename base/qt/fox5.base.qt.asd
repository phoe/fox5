;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.base.qt.asd

(asdf:defsystem #:fox5.base.qt
  :description "Qt functions for FOX5"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:static-vectors
               #:qtools
               #:qtcore
               #:qtgui
               #:fox5.base)
  :components ((:file "package")
               (:file "qt")))
