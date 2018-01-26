;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; new.lisp

(in-package :fox5)

;; TODO exports
;; TODO make objects point to their parents so shapes can print their SHAPE-TYPE
;; TODO objects -//- EDIT-TYPE

;; TODO describe somewhere what shape type is

(defgeneric shape-type (object shape)
  "Returns the shape type of the provided shape."
  ;; TODO description here, this needs to be protocolized
  (:method ((object object) (shape shape))
    (%shape-type object shape (edit-type object))))

(defgeneric %shape-type (object shape edit-type))

(defmacro define-shape-type ((&rest edit-types) &body body)
  ;; TODO description here, this needs to be protocolized
  `(progn ,@(mapcar (lambda (x) `(%define-shape-type ,x ,@body)) edit-types)))

(defmacro %define-shape-type (edit-type &body body)
  (with-gensyms (object shape)
    `(defmethod %shape-type
         ((,object object) (,shape shape) (edit-type (eql ',edit-type)))
       (with-accessors ((purpose purpose) (state state) (direction direction)
                        (num numerator) (den denominator))
           ,shape
         ,@body))))

;; TODO when defining writer, use SLOT-DEFINITION-INITFORM to check if the set
;; value matches the default - in this case, SLOT-MAKUNBOUND the slot just to be
;; sure

(define-shape-type (nil))

(define-shape-type (:floor :item :effect :region :lighting :ambience)
  (case purpose
    (1 `(:icon ,(case num (1 :tiny) (2 :small) (3 :large) (4 :showcase))))
    (t `(,edit-type ,(case num (1 :small) (2 :large))))))

(define-shape-type (:wall)
  (case purpose
    (1 `(:icon ,(case num (1 :tiny) (2 :small) (3 :large) (4 :showcase))))
    (23 `(:wall ,(case direction (7 :right) (9 :left))
                ,(case num (1 :small) (2 :large))))))

(define-shape-type (:portal)
  (case purpose
    (1 `(:icon ,(case num (1 :tiny) (2 :small) (3 :large) (4 :showcase))))
    (28 `(:pad ,(case num (1 :small) (2 :large))))
    (29 `(:portal ,(case state
                     (0 :standard) (1 :silver-sponsor) (2 :gold-sponsor)
                     (8 :group-package) (9 :high-group-package)
                     (16 :staff) (17 :special))
                  ,(case num (1 :small) (2 :large))))))

(define-shape-type (:portrait-set)
  (case purpose
    (4 `(:portrait ,(case state (1 :female) (2 :male) (4 :unspecified))))
    (35 `(:specitag ,(case state (1 :female) (2 :male) (4 :unspecified))))))

(define-shape-type (:avatar :gendered-avatar)
  (case purpose
    (1 `(:icon ,(case num (1 :tiny) (2 :small) (3 :large) (4 :showcase))))
    (3 `(:butler ,(case state (1 :female) (2 :male) (4 :unspecified))))
    (4 `(:portrait ,(case state (1 :female) (2 :male) (4 :unspecified))))
    (11 `(:avatar
          ,(case direction (1 :sw) (3 :se) (7 :nw) (9 :ne))
          ,(case (ldb (byte 4 0) state) (1 :female) (2 :male) (4 :unspecified))
          ,(case num (1 :small) (2 :large))
          ,(case (ldb (byte 4 4) state)
             (1 :walk-right) (2 :lie) (3 :walk) (4 :sit) (5 :walk-left))))
    (35 `(:specitag ,(case state (1 :female) (2 :male) (4 :unspecified))))))

(define-shape-type (:button :ds-button)
  `(,edit-type ,(case state (0 :normal) (1 :clicked) (2 :hover) (4 :toggled))))
