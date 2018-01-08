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

;; TODO make PARENT slot inside class FOX5-CLASS to get rid of *PARENT-OBJECT*
(defgeneric postprocess (object)
  (:documentation #.(format nil "Postprocesses the contents of the FOX5 ~
object after the initial reading of the FOX5 file. Postprocessing is required, ~
as the results of some FOX5 commands may depend on other commands inside the ~
same object or parent objects, and the required processing cannot be done ~
as a part of the reader, since the order in which FOX5 commands appear is not ~
specified. Therefore it is possible for the list of children to appear before ~
the parent's properties are set, and the parent's properties may influence how ~
the children's properties should be set.
\
Client code calling this function must appropriately bind *PARENT-OBJECT* ~
to the parent of OBJECT.
\
Methods on this generic function are expected to be established using the ~
DEFINE-FOX5-POSTPROCESSOR macro.")))

(defmethod postprocess :after ((object fox5-class))
  "The default :AFTER method for all FOX5 objects. All children of the object ~
are postprocessed after its parent is postprocessed."
  (let ((*parent-object* object))
    (mapc #'postprocess (children object))))

(defgeneric write-command (object slot-name buffer)
  (:documentation #.(format nil "Writes the FOX5 command representing the slot ~
value of SLOT-NAME in OBJECT into the provided bugger.
\
Methods on this generic function are expected to be established using the ~
DEFINE-FOX5-WRITER macro.")))

(defmethod write-command (object slot-name buffer)
  "The base WRITE-COMMAND method that does nothing. Used for slots that are
not defined to be written."
  (declare (ignore object slot-name buffer)))

;;; TODO update all reader methods for misplaced commands, right now they
;;; will blow up on encountering one
;;; TODO maybe just define a generic method that calls CL:WARN to print a
;;; message to the user, and otherwise does nothing?
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
