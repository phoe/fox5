;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.gif-fox5.asd

(asdf:defsystem #:fox5.gif-fox5
  :description "Plugin for converting animated GIF files into FOX5 files"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:skippy-renderer
               #:fox5)
  :components ((:file "gif-fox5")))
