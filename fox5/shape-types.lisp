;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; shape-types.lisp

(in-package :fox5)

;;; Framework - Readers

(defgeneric shape-type (shape)
  (:method ((shape shape))
    (if-let ((object (parent shape)))
      (handler-case
          (if-let ((edit-type (edit-type object)))
            (cons edit-type (%shape-type object shape edit-type)))
        (error (e) `(:error ,(princ-to-string e))))
      '(:unknown)))
  (:documentation "Returns the shape type of the provided shape. The returned
value may be NIL or a member of *SHAPE-TYPES*."))

(defgeneric %shape-type (object shape edit-type))

(defmacro define-shape-type-get ((&rest edit-types) &body body)
  "Defines a function computing the shape type for EDIT-TYPES.
\
The available immutable variables are EDIT-TYPE, PURPOSE, STATE, DIRECTION,
NUM and DEN (numerator and denominator from RATIO)."
  `(progn ,@(mapcar (lambda (x) `(%define-shape-type-get ,x ,@body))
                    edit-types)))

(defmacro %define-shape-type-get (edit-type &body body)
  (with-gensyms (object shape)
    `(defmethod %shape-type
         ((,object object) (,shape shape) (edit-type (eql ',edit-type)))
       (let ((purpose (purpose ,shape)) (state (state ,shape))
             (direction (direction ,shape)) (num (first (ratio ,shape)))
             (den (second (ratio ,shape))))
         (declare (ignorable purpose state direction num den))
         ,@body))))

;;; Framework - Writers

(defgeneric (setf shape-type) (new-value shape)
  (:method (new-value (shape shape))
    (when (not (null new-value))
      (assert (member new-value *shape-types* :test #'equal) ()
              "~A is not a valid shape type." new-value))
    (if-let ((object (parent shape)))
      (setf (%shape-type object shape (car new-value)) (cdr new-value))
      (error "SHAPE has no parent object."))
    new-value)
  (:documentation "Sets the shape type of the provided shape. The set value
may be NIL or a member of *SHAPE-TYPES*."))

(defgeneric (setf %shape-type) (new-value object shape edit-type))

(defmacro define-shape-type-set ((&rest edit-types) &body body)
  "Defines a function computing the shape type for EDIT-TYPES.
\
The available places, bound via SYMBOL-MACROLET, are PURPOSE, STATE, DIRECTION,
NUM and DEN (numerator and denominator from RATIO). The immutable variables are
EDIT-TYPE and NEW-VALUE, which contains the new value to be set."
  `(progn ,@(mapcar (lambda (x) `(%define-shape-type-set ,x ,@body))
                    edit-types)))

(defmacro %define-shape-type-set (edit-type &body body)
  (with-gensyms (object shape)
    `(defmethod (setf %shape-type) (new-value (,object object) (,shape shape)
                                    (edit-type (eql ',edit-type)))
       (symbol-macrolet ((purpose (purpose ,shape))
                         (state (state ,shape))
                         (direction (direction ,shape))
                         (num (first (ratio ,shape)))
                         (den (second (ratio ,shape))))
         (setf (edit-type ,object) edit-type
               purpose (first new-value) direction nil state 0 num 0 den 0)
         ,@body
         new-value))))

;;; Print definitions

(define-print (object stream) (princ (edit-type object) stream))

(define-print (shape stream) (princ (shape-type shape) stream))

;;; TODO alists for all values

;;; Empty edit type

(define-shape-type-get (nil))

(define-shape-type-set (nil))

;;; FLOOR ITEM EFFECT REGION LIGHTING AMBIENCE

(define-shape-type-get (:floor :item :effect :region :lighting :ambience)
  (ecase purpose
    (:menu-icon `(:menu-icon
                  ,(ecase num (1 :tiny) (2 :small) (3 :large) (4 :showcase))))
    ((:floor :item :effect :region :lighting :ambience)
     `(,edit-type ,(ecase num (1 :small) (2 :large))))))

(define-shape-type-set (:floor :item :effect :region :lighting :ambience)
  (ecase (first new-value)
    (:menu-icon (setf num (ecase (second new-value)
                            (:tiny 1) (:small 2) (:large 3) (:showcase 4))))
    ((:floor :item :effect :region :lighting :ambience)
     (setf num (ecase (second new-value) (:small 1) (:large 2))))))

;;; WALL

(define-shape-type-get (:wall)
  (ecase purpose
    (:menu-icon `(:menu-icon
                  ,(ecase num (1 :tiny) (2 :small) (3 :large) (4 :showcase))))
    (:wall `(:wall ,(ecase direction (:nw :right) (:ne :left))
                   ,(ecase num (1 :small) (2 :large))))))

(define-shape-type-set (:wall)
  (ecase (first new-value)
    (:menu-icon (setf num (ecase (second new-value)
                            (:tiny 1) (:small 2) (:large 3) (:showcase 4))))
    (:wall (setf direction (ecase (second new-value) (:right :nw) (:left :ne))
                 num (ecase (third new-value) (:small 1) (:large 2))
                 den 1))))

;;; PORTAL

(define-shape-type-get (:portal)
  (ecase purpose
    (:menu-icon `(:menu-icon
                  ,(ecase num (1 :tiny) (2 :small) (3 :large) (4 :showcase))))
    (:pad `(:pad ,(ecase num (1 :small) (2 :large))))
    (:portal `(:portal ,(ecase state
                          (0 :standard) (1 :silver-sponsor) (2 :gold-sponsor)
                          (8 :group-package) (9 :high-group-package)
                          (16 :staff) (17 :special))
                       ,(ecase num (1 :small) (2 :large))))))

(define-shape-type-set (:portal)
  (ecase (first new-value)
    (:menu-icon (setf num (ecase (second new-value)
                            (:tiny 1) (:small 2) (:large 3) (:showcase 4))))
    (:pad (setf num (ecase (second new-value) (:small 1) (:large 2))
                den 1))
    (:portal (setf state (ecase (second new-value)
                           (:standard 0) (:silver-sponsor 1) (:gold-sponsor 2)
                           (:group-package 8) (:high-group-package 9)
                           (:staff 16) (:special 17))
                   num (ecase (third new-value) (:small 1) (:large 2))
                   den 1))))

;;; PORTRAIT-SET

(define-shape-type-get (:portrait-set)
  `(,(ecase state (1 :female) (2 :male) (4 :unspecified))))

(define-shape-type-set (:portrait-set)
  (setf state (ecase (first new-value)
                (:female 1) (:male 2) (:unspecified 4))))

;;; AVATAR GENDERED-AVATAR

(define-shape-type-get (:avatar :gendered-avatar)
  (cons purpose
        (ecase purpose
          (:menu-icon
           `(,(ecase num (1 :tiny) (2 :small) (3 :large) (4 :showcase))))
          ((:butler :portrait :specitag)
           `(,(ecase state (1 :female) (2 :male) (4 :unspecified))))
          (:avatar
           `(,direction
             ,(ecase (ldb (byte 4 0) state)
                (0 nil) (1 :female) (2 :male) (4 :unspecified))
             ,(case num (1 :small) (2 :large) (t num))
             ,(ecase (ldb (byte 4 4) state)
                (1 :walk-right) (2 :lie) (3 :walk) (4 :sit) (5 :walk-left)))))))

(define-shape-type-set (:avatar :gendered-avatar)
  (ecase (first new-value)
    (:menu-icon (setf num (ecase (second new-value)
                            (:tiny 1) (:small 2) (:large 3) (:showcase 4))))
    ((:butler :portrait :specitag)
     (setf state (ecase (second new-value)
                   (:female 1) (:male 2) (:unspecified 4))))
    (:avatar
     (setf direction (second new-value))
     (setf (ldb (byte 4 0) state)
           (ecase (third new-value) (:female 1) (:male 2) (:unspecified 4)))
     (setf num (ecase (fourth new-value) (:small 1) (:large 2)))
     (setf den 1)
     (setf (ldb (byte 4 4) state)
           (ecase (fifth new-value)
             (:walk-right 1) (:lie 2) (:walk 3) (:sit 4) (:walk-left 5))))))

;;; BUTTON DS-BUTTON

(define-shape-type-get (:button :ds-button)
  `(,(ecase state (0 :normal) (1 :clicked) (2 :hover) (4 :toggled))))

(define-shape-type-set (:button :ds-button)
  (setf state (ecase (first new-value)
                (:normal 0) (:clicked 1) (:hover 2) (:toggled 4))))

;;; Unit tests

(defun test-define-shape-type (&key loudp (data *shape-types*))
  (let ((file (make-instance 'file))
        (object (make-instance 'object))
        (shape (make-instance 'shape)))
    (parent-push file object)
    (parent-push object shape)
    (assert (eq nil (shape-type shape)))
    (flet ((test (shape-type)
             (when loudp (format t "Testing for ~A.~%" shape-type))
             (setf (shape-type shape) shape-type)
             (assert (equal shape-type (shape-type shape)))
             (setf (shape-type shape) nil)
             (assert (eq nil (shape-type shape)))))
      (mapc #'test data))
    (handler-case (progn (setf (shape-type shape) '(:nothing))
                         (error "Expected an error to be signaled."))
      (error ()))
    (values)))

(defvar *shape-types*
  (uiop:while-collecting (collect)
    ;; FLOOR ITEM EFFECT REGION LIGHTING AMBIENCE
    (dolist (i '(:floor :item :effect :region :lighting :ambience))
      (dolist (j `((:menu-icon :tiny) (:menu-icon :small) (:menu-icon :large)
                   (:menu-icon :showcase) (,i :small) (,i :large)))
        (collect (list* i j))))
    ;; WALL
    (dolist (i '(:tiny :small :large :showcase))
      (collect (list :wall :menu-icon i)))
    (dolist (i '(:right :left))
      (dolist (j '(:small :large))
        (collect (list :wall :wall i j))))
    ;; PORTAL
    (dolist (i '(:tiny :small :large :showcase))
      (collect (list :portal :menu-icon i)))
    (dolist (i '(:small :large))
      (collect (list :portal :pad i))
      (dolist (j '(:standard :silver-sponsor :gold-sponsor :group-package
                   :high-group-package :staff :special))
        (collect (list :portal :portal j i))))
    ;; PORTRAIT SET
    (dolist (i '(:female :male :unspecified))
      (collect (list :portrait-set i)))
    ;; AVATAR GENDERED-AVATAR
    (dolist (i '(:tiny :small :large :showcase))
      (collect (list :avatar :menu-icon i)))
    (dolist (i '(:butler :portrait :specitag))
      (dolist (j '(:female :male :unspecified))
        (collect (list :avatar i j))))
    (dolist (i '(:nw :ne :sw :se))
      (dolist (j '(:female :male :unspecified))
        (dolist (k '(:small :large))
          (dolist (l '(:walk-right :lie :walk :sit :walk-left))
            (collect (list :avatar :avatar i j k l))))))
    ;; BUTTON DS-BUTTON
    (dolist (i '(:button :ds-button))
      (dolist (j '(:normal :clicked :hover :toggled))
        (collect (list i j)))))
  "A list of all shape types legal in FOX5.")
