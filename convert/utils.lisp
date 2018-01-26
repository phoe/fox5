;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda
;;;; utils.lisp

(in-package :fox5)

(export 'parent-push :fox5)

(defun parent-push (parent child)
  (push child (children parent))
  (setf (parent child) parent)
  (values))
