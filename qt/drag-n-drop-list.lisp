;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; drag-n-drop-list.lisp

(in-package :fox5/qt)
(in-readtable :qtools)

;;; MIME data containing a Lisp data reference

(defvar *mime-data-with-object-type*
  "application/qtools-mime-data-with-object")

(define-widget mime-data-with-object (qmimedata)
  ((object :accessor object
           :initarg :object
           :initform nil)))

;;; Draggable

(define-widget draggable (QWidget)
  ((mime-type :accessor mime-type
              :initarg :mime-type
              :initform nil)))

(define-override (draggable mouse-press-event) (event)
  (when (eq (enum-value (q+:button event)) (q+:qt.left-button))
    (let ((drag (q+:make-qdrag draggable))
          (mime-data (make-instance 'mime-data-with-object
                                    :object draggable)))
      (setf (q+:data mime-data *mime-data-with-object-type*) ""
            (q+:mime-data drag) mime-data)
      (q+:exec drag)))
  (stop-overriding))

;;; Drop target

(define-widget drop-target (QWidget) ())

(define-initializer (drop-target initialize-drop-target)
  (setf (q+:accept-drops drop-target) t))

(define-override (drop-target drag-enter-event) (event)
  (when (q+:has-format (q+:mime-data event) *mime-data-with-object-type*)
    (q+:accept-proposed-action event))
  (stop-overriding))

(define-override (drop-target drop-event) (event)
  (let ((mime-data (q+:mime-data event)))
    (drop mime-data drop-target))
  (stop-overriding))

;;; GFs and methods

(defgeneric drop (item target))

(defgeneric drop-acceptable-p (item target)
  (:method (item target) NIL))

(defmethod drop :around (item target)
  (when (drop-acceptable-p item target)
    (call-next-method)))

;;; Logic for MIME-DATA-WITH-OBJECT

(defmethod drop-acceptable-p ((item mime-data-with-object) target)
  T)

(defmethod drop ((item mime-data-with-object) target)
  (drop (object item) target))

;;; Logic for DRAGGABLE

(defmethod drop-acceptable-p ((item draggable) target)
  T)

(defmethod drop ((item draggable) target)
  (print item)) ;; for debugging only

;;; Test - drag from top of the window to the bottom of the window

(defun test ()
  (with-main-window (layout 'qui:flow-layout)
    (qui:add-widget (make-instance 'draggable) layout)
    (qui:add-widget (make-instance 'drop-target) layout)))
