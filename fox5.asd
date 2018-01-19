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
               #:cl-furcadia.ws)
  :components ((:file "fox5/package")
               (:file "fox5/classes")
               (:file "fox5/utils")
               (:file "fox5/constants")
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
               #:fox5
               #:cl-furcadia.remap)
  :components ((:file "qt/qt")))

;; (asdf:defsystem #:fox5/qt
;;   :description "Display FOX5 files with Qtools (for development)"
;;   :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
;;   :license "BSD 3-clause"
;;   :serial t
;;   :components ((:file "qt/qt")))

;; (asdf:defsystem #:fox5/converters
;;   :description "Library for converting Furcadia FSH/FS2/FOX1 files into FOX5"
;;   :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
;;   :license "BSD 3-clause"
;;   :serial t
;;   :depends-on (#:fox5)
;;   :components ((:file "fsh/fsh")
;;                (:file "fs2/fs2")
;;                (:file "fox1/fox1")
;;                (:file "gif/gif")))
