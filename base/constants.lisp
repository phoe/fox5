;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; constants.lisp

(in-package :fox5/base)

(defvar *footer-magic-string*
  (flexi-streams:string-to-octets "FOX5.1.1")
  "FOX5 magic string, denoting the format version.")

(defvar *fox5-list-levels*
  '(file object shape frame sprite)
  "The currently supported FOX5 list levels, 0-indexed.")

(defparameter *fox5-generator-number* 200
  "The file generator number for FOX5 library.")
