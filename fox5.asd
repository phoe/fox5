;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; fox5.asd

(asdf:defsystem #:fox5
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
               #:closer-mop
               #:hunchentoot
               #:cl-furcadia.ws
               #:cl-furcadia.remap)
  :components ((:file "fox5/package")
               (:file "fox5/utils")
               (:file "fox5/constants")
               (:file "fox5/classes")
               (:file "fox5/shape-types")
               (:file "fox5/read-write/framework")
               (:file "fox5/read-write/footer")
               (:file "fox5/read-write/command-block")
               (:file "fox5/read-write/images")
               (:file "fox5/read-write/parsers")
               (:file "fox5/read-write/file")))

(asdf:defsystem #:fox5/qt
  :description "Display FOX5 files with Qtools (for development)"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:static-vectors
               #:qtools
               #:qtcore
               #:qtgui
               #:qtools-ui-flow-layout
               #:qtools-ui-drag-and-drop
               #:fox5
               #:cl-furcadia.remap)
  :components ((:file "qt/qt")))

(asdf:defsystem #:fox5/convert
  :description "Library for converting Furcadia FSH/FS2/FOX1 files into FOX5"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:fox5
               #:skippy-renderer)
  :components ((:file "convert/utils")
               (:file "convert/fsh-fs2")
               (:file "convert/fox1")
               (:file "convert/gif")
               (:file "convert/specialize")))
