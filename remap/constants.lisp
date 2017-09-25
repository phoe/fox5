;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; constants.lisp

(in-package :fox5)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *color-types*
    '(fur markings hair eyes badge vest bracers
      cape boots trousers wings accent)
    "All valid remappable color types in a Furcadia (sans outline)."))

(deftype color ()
  "A symbol denoting a Furcadia color."
  '#.`(member ,@*color-types*))

(defvar *gradients* (read-data-file "remap/data/gradients.lisp")
  "Hash-table containing gradient data. The keys are in form (SYMBOL STRING),
where SYMBOL is taken from *COLOR-TYPES* and STRING is a valid color name.")

(defvar *color-names* (read-data-file "remap/data/color-names.lisp")
  "Hash-table containing color names. Keys are symbols from *COLOR-TYPES*.")

(defvar *color-values* (read-data-file "remap/data/color-values.lisp")
  "Hash table between integers and their respective remap types.")

(defvar *color-code-indices*
  '(version fur markings hair eyes badge vest bracers cape
    boots trousers wings accent gender species reserved)
  "Indices of a Furcadia color code.")

(defvar *genders*
  '(female male unspecified)
  "Gender values possible in a Furcadia color code.")
