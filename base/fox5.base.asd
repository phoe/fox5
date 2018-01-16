;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.base.asd

(asdf:defsystem #:fox5.base
  :description "Classes and protocols for FOX5"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:closer-mop)
  :components ((:file "package")
               (:file "base")))
