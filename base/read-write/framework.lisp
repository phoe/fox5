;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; read-write-base.lisp

(in-package :fox5/base)

(defvar *current-object* nil
  #.(format nil "The FOX5 object that is currently being read.
\
Expected to be dynamically rebound whenever the currently read object changes, ~
usually to a child of the read object."))

(defvar *parent-object* nil
  #.(format nil "The parent of *CURRENT-OBJECT*, or NIL if there is none.
\
Expected to be dynamically rebound whenever *CURRENT-OBJECT* changes."))

(defvar *footer* nil
  #.(format nil "The footer of the currently written FOX5 file.

Expected to be bound to an actual instance of FOX5-FOOTER and modified during ~
WRITE-FOX5 function call."))

(defgeneric write-fox5-to-buffer (object buffer)
  (:documentation #.(format nil "Writes the provided FOX5 object into the ~
provided buffer.")))

(defgeneric read-command (command buffer)
  (:documentation #.(format nil "Reads the contents of the provided FOX5 ~
command from the provided buffer and updates the FOX5 object at
*CURRENT-OBJECT*.
\
Methods on this generic function are expected to be established using the ~
DEFINE-FOX5-READER macro.")))

(defgeneric write-command (object slot-name buffer)
  (:documentation #.(format nil "Writes the FOX5 command representing the slot ~
value of SLOT-NAME in OBJECT into the provided bugger.
\
Methods on this generic function are expected to be established using the ~
DEFINE-FOX5-WRITER macro.")))

;;; TODO update all reader methods for misplaced commands, right now they
;;; will blow up on encountering one
(defmacro define-fox5-reader ((byte buffer
                               &optional (object-class 't)) &body body)
  #.(format nil "Defines a reader for the FOX5 command designated by the ~
provided BYTE. The command details are read from the provided buffer.
\
In the current implementation, providing OBJECT-CLASS will signal an error ~
whenever the command is read with *CURRENT-OBJECT* not being an instance of ~
the provided class.")
  `(defmethod read-command ((command (eql ,(code-char byte)))
                            ,buffer)
     (unless (typep *current-object* ,object-class)
       (error "Encountered command ~A (~D) while reading object of class ~A."
              (code-char command) command ,object-class))
     ,@body))

(defmacro define-fox5-writer ((class-name accessor-name buffer-var) &body body)
  #.(format nil "Defines a writer for the given accessor of the given FOX5 ~
class. Contents will be written to the provided buffer.")
  (let ((slot-name (symbolicate "%" accessor-name)))
    `(defmethod write-command ((,class-name ,class-name)
                               (slot-name (eql ',slot-name))
                               ,buffer-var)
       (let ((,accessor-name (slot-value ,class-name slot-name)))
         ,@body))))
