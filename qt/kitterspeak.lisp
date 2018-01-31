;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; kitterspeak.lisp

(in-package :fox5/qt)
(in-readtable :qtools)

;;; Widget

(define-widget animation (qwidget)
  ((shapes :accessor shapes
           :initarg :shapes
           :initform '())
   (current-shape :accessor current-shape
                  :initarg :current-shape
                  :initform 0)
   (current-step :accessor current-step
                 :initform 0)
   (delay-min :accessor delay-min
              :initform 100)
   (delay-max :accessor delay-max
              :initform 100)
   (auto-delay-p :accessor auto-delay-p
                 :initform t)
   (color-code :accessor color-code
               :initarg :color-code
               :initform nil)
   (scene :reader scene
          :initform (q+:make-qgraphicsscene))
   (timer :reader timer
          :initform (q+:make-qtimer))
   (layers :reader layers
           :initform (make-array 8 :initial-element nil)))
  (:documentation "A widget capable of displaying animated FOX5 shapes.
LAYERS is an eight-element array that is meant to contain the currently ~
displayed items, generated from sprites. The ordering is:
0: BG, shadow
1: behind, shadow
2: front, shadow
3: FG, shadow
4: BG, data
5: behind, data
6: front, data
7: FG, data"))

(define-finalizer (animation finalize-animation)
  (finalize (scene animation))
  (finalize (timer animation)))

(define-subwidget (animation layout) (q+:make-qgridlayout)
  (setf (q+:layout animation) layout
        (q+:contents-margins layout) (values 0 0 0 0)))

(define-subwidget (animation preview) (q+:make-qgraphicsview (scene animation))
  (q+:add-widget layout preview 0 0 1 2)
  (setf (q+:row-stretch layout 0) 9001))

(defun make-text-qtoolbutton (text)
  (let ((button (q+:make-qtoolbutton)))
    (setf (q+:text button) text
          (q+:tool-button-style button) (q+:qt.tool-button-text-only))
    button))

(define-subwidget (animation prev-button) (make-text-qtoolbutton "<")
  (q+:add-widget layout prev-button 1 0)
  (setf (q+:size-policy prev-button)
        (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.expanding))))

(define-subwidget (animation next-button) (make-text-qtoolbutton ">")
  (q+:add-widget layout next-button 1 1)
  (setf (q+:size-policy next-button)
        (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.expanding))))

;;; Methods

;; (defmethod animate ((animation animation) (object object)
;;                     &key (predicate (constantly t)) color-code)
;;   (let ((shapes (remove-if-not predicate (children object))))
;;     (setf (shapes animation) shapes
;;           (current-shape animation) 0
;;           (color-code animation) color-code)
;;     ;; We want to display this shape. This means that we need to:
;;     ;; * set the defaults,
;;     (initialize animation)
;;     ;; * begin parsing Kitterspeak.
;;     ))

;; (defmethod initialize ((animation animation))
;;   (with-slots-bound (animation animation)
;;     (when-let ((shape (nth current-shape shapes)))
;;       (let* ((frame (first (children shape)))
;;              (sprite (find-if (rcurry #'member '(nil :remapping-data))
;;                               (children frame) :key #'purpose))
;;              (frame-x-offset (getf (frame-offset frame) :x))
;;              (frame-y-offset (getf (frame-offset frame) :y))
;;              (sprite-x-offset (getf (offset sprite) :x))
;;              (sprite-y-offset (getf (offset sprite) :y))
;;              (item (sprite-item sprite color-code
;;                                 (+ frame-x-offset sprite-x-offset)
;;                                 (+ frame-y-offset sprite-y-offset))))
;;         (setf (q+:zvalue item) current-shape
;;               (aref layers current-shape) item)
;;         (q+:add-item scene item)))))

;; (defmethod update ((animation animation))
;;   (with-slots-bound (animation animation)
;;     ;; We need to clear the scratch space.
;;     (q+:clear preview)
;;     (loop for i below 8 do (setf (aref layers i) nil)) ;; TODO optimize this
;;     ;; We need to figure out which shape needs to be displayed now.
;;     (let ((shape (nth current-shape shapes)))
;;       (when (null shape) (setf shape (first shapes)))
;;       )))

;; (lambda (x) (and (eq (third (shape-type x)) :male)
;;                  (eq (second (shape-type x)) :avatar)))

;; (defun add-sprite (scene sprite layer &key (x-offset 0) (y-offset 0) color-code)
;;   (let ((purpose (purpose sprite))
;;         (item (sprite-qgraphicsitem sprite color-code x-offset y-offset))
;;         (zvalue (ecase layer (:fg 40) (:front 30) (:behind 20) (:bg 10))))
;;     (cond ((eq purpose :shadow-layer)
;;            (setf (q+:opacity item) 0.5
;;                  (q+:zvalue item) (- zvalue 5)))
;;           (t (setf (q+:zvalue item) zvalue)))
;;     (q+:add-item scene item)))

;; (defun show-frame (frame &optional color-code)
;;   (with-finalizing ((scene (q+:make-qgraphicsscene)))
;;     (with-main-window (view (q+:make-qgraphicsview scene))
;;       (dolist (sprite (children frame))
;;         (let ((purpose (purpose sprite)))
;;           (when (member purpose '(nil :remapping-data :shadow-layer))
;;             (let ((item (sprite-qgraphicsitem sprite :color-code color-code)))
;;               (q+:add-item scene item)
;;               (setf (q+:zvalue item) 10)
;;               (when (eq purpose :shadow-layer)
;;                 (setf (q+:opacity item) 0.5)
;;                 (setf (q+:zvalue item) 0)))))))))

;; (defgeneric execute-kitterspeak (shape animation type arg1 arg2)
;;   (:documentation "Executes the provided kitterspeak line. Dispatch is done
;; based on TYPE argument, which must be one of the CDRs of *KITTERSPEAK*."))

;; (defmethod execute-kitterspeak
;;     ((shape shape) (animation animation)
;;      (type symbol) (arg1 integer) (arg2 integer))
;;   ;; Fallback method - we currently ignore unknown kitterspeak lines.
;;   ;; The below assertion is to make sure that we do not have typos in type name.
;;   (assert (member type *kitterspeak* :key #'cdr)))

;; (defmethod execute-kitterspeak
;;     ((shape shape) (animation animation)
;;      (type (eql :show-frame)) (arg1 integer) (arg2 integer))
;;   (with-slots-bound (anmation animation)
;;     (let* ((frame (nth arg1 (children shape)))
;;            (x-offset (getf (frame-offset frame) :x))
;;            (y-offset (getf (frame-offset frame) :y)))
;;       (flet ((pred (purposes) (lambda (x) (member (purpose x) purposes))))
;;         (when-let ((sprite (find-if (pred '(nil :remapping-data))
;;                                     (children frame))))
;;           (let ((item (sprite-item sprite color-code x-offset y-offset)))
;;             (setf (aref ))))))))

;; TODO optimize: keep all sprites precomputed as items in a weak hash table(?)

(defun update (animation)
  (with-slots-bound (animation animation)
    (q+:clear scene)
    ;; TODO multiple shadows will not get drawn correctly because 0.5 dark
    ;; opacity * 0.5 dark opacity will turn into 0.75 dark opacity, not 0.5
    (loop for item across layers when item do (q+:add-item scene item))))

(defun draw-shape (animation shape)
  (with-slots-bound (animation animation)
    (setf current-shape shape)
    (draw-frame animation (first (children shape)) :behind)
    (execute-kitterspeak animation)))

(defun draw-frame (animation frame layer)
  (with-slots-bound (animation animation)
    (let* ((shadow-index (ecase layer (:bg 0) (:behind 1) (:front 2) (:fg 3)))
           (data-index (+ shadow-index 4))
           (x-offset (getf (frame-offset frame) :x))
           (y-offset (getf (frame-offset frame) :y)))
      (dolist (sprite (children frame))
        (let ((item (sprite-item sprite color-code x-offset y-offset)))
          (case (purpose sprite)
            ((nil :remapping-data)
             (setf (aref layers data-index) item
                   (q+:zvalue item) (- 8 data-index)))
            ((:shadow-layer)
             (setf (aref layers shadow-index) item
                   (q+:zvalue item) (- 8 shadow-index)
                   (q+:opacity item) 0.5))))))))

;; TODO https://stackoverflow.com/questions/7451183/

(defun sprite-item (sprite &optional color-code x-offset y-offset)
  (let* ((offset (offset sprite))
         (image-id (1- (image-id sprite)))
         (file (nth-funcall #'parent 4 sprite))
         (image (nth image-id (images file)))
         (pixmap (image-qpixmap image color-code))
         (item (q+:make-qgraphicspixmapitem pixmap)))
    (setf (q+:offset item) (values (+ (or x-offset 0) (getf offset :x))
                                   (+ (or y-offset 0) (getf offset :y))))
    item))

(defvar *max-kitterspeak-steps* 50
  "The maximum number of steps to execute.")

(defvar *kitterspeak-stop-delay* 2000
  "Defines if Kitterspeak :STOP step should stop Kitterspeak execution forever
or rather induce a delay before restarting it.
If NIL, then STOP commands completely stop Kitterspeak execution.
If non-NIL, then the value of this variable is the number of milliseconds
that will pass before KS execution is restarted from the beginning.")

(defun execute-kitterspeak (animation)
  "Begins Kitterspeak execution in the provided animation."
  (with-slots-bound (animation animation)
    (with-accessors ((kitterspeak kitterspeak)) current-shape
      (when kitterspeak
        (when (null (nth current-step kitterspeak))
          (setf current-step 0))
        (loop repeat *max-kitterspeak-steps*
              for step = (nth current-step kitterspeak) ;; TODO optimize NTH
              for (type arg1 arg2) = step
              for continuep = (funcall #'execute-step animation type arg1 arg2)
              if continuep
                do (incf current-step)
              else
                return nil)
        (update animation)))))

(defgeneric execute-step (animation type arg1 arg2)
  (:documentation "Executes the Kitterspeak step denoted by TYPE, ARG1 and ARG2.
Must return non-NIL if the execution is meant to be continued after this step,
or NIL if it should be paused."))

(defmethod execute-step
    ((animation animation) (type null) (arg1 null) (arg2 null))
  "This method is called when the end of KS has been reached. Returns NIL.")

(defmacro define-kitterspeak
    (type continuep (&optional (animation 'animation) (arg1 'arg1) (arg2 'arg2))
     &body body)
  "Defines a method for parsing Kitterspeak for line TYPE. Return value of BODY
is ignored.  CONTINUEP states if Kitterspeak processing should continue after
the body of this method (in case of most steps), or if it should pause (forever,
in case of :STOP, or until the timer fires next time, in case of delays)."
  (when type
    (assert (member type *kitterspeak* :key #'cdr) ()
            "~S is not a valid Kitterspeak type." type))
  `(defmethod execute-step
       ((,animation animation) (type (eql ,type)) ,arg1 ,arg2)
     ,@(if (and body (stringp (first body)))
           `(,(car body)
             (with-slots-bound (,animation animation)
               ,@(cdr body)))
           `(with-slots-bound (,animation animation)
              ,@body))
     ,(if continuep t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Null case

;; 0 - NIL

(define-kitterspeak nil nil ()
  "Null method. Called each time the Kitterspeak reaches its end. This method's
 sole purpose is to return NIL, so Kitterspeak execution is halted.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame order

;; 23 - :SHOW-BG-FRAME

(define-kitterspeak :show-bg-frame t (animation nframe)
  "Shows frame number NFRAME on layer BG."
  (let ((frame (nth nframe (children current-shape))))
    (draw-frame animation frame :bg)))

;; 29 - :SHOW-BEHIND-FRAME

(define-kitterspeak :show-behind-frame t (animation nframe)
  "Shows frame number NFRAME on layer BEHIND."
  (let ((frame (nth nframe (children current-shape))))
    (draw-frame animation frame :behind)))

;; 30 - :SHOW-FRONT-FRAME

(define-kitterspeak :show-front-frame t (animation nframe)
  "Shows frame number NFRAME on layer FRONT."
  (let ((frame (nth nframe (children current-shape))))
    (draw-frame animation frame :front)))

;; 24 - :SHOW-FG-FRAME

(define-kitterspeak :show-fg-frame t (animation nframe)
  "Shows frame number NFRAME on layer FG."
  (let ((frame (nth nframe (children current-shape))))
    (draw-frame animation frame :fg)))

;; 31 - :MOVE-FORWARD

;; TODO

;; 32 - :MOVE-BACKWARD

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flow control

;; 2 - :DELAY

;; TODO

;; 15 - :RANDOM-FRAME-DELAY

;; TODO

;; 11 - :AUTO-FRAME-DELAY

;; TODO

;; 14 - :RANDOM-FRAME-AUTO-DELAY

;; TODO

;; 3 - :LOOP

;; TODO

;; 4 - :JUMP

(define-kitterspeak :jump t (animation nstep)
  "Jumps to step N."
  (setf current-step (1- nstep)))

;; 12 - :STOP

(define-kitterspeak :stop nil (animation)
  "Stops Kitterspeak execution. If *KITTERSPEAK-STOP-DELAY* is set, executes
a delay defined by that variable instead."
  (when-let ((delay *kitterspeak-stop-delay*))
    (execute-step animation :delay delay 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Offset - set

;; 5 - :FRAME-X

;; TODO

;; 6 - :FRAME-Y

;; TODO

;; 7 - :FURRE-X

(define-kitterspeak :furre-x t ()
  "Set furre's X offset.
Not implemented.")

;; 8 - :FURRE-Y

(define-kitterspeak :furre-y t ()
  "Set furre's Y offset.
Not implemented.")

;; 17 - :OPACITY

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Offset - slide

;; 18 - :SLIDE-FRAME-X

;; TODO

;; 19 - :SLIDE-FRAME-Y

;; TODO

;; 20 - :SLIDE-FURREX

(define-kitterspeak :slide-furre-x t ()
  "Slide furre's X offset.
Not implemented.")

;; 21 - :SLIDE-FURREY

(define-kitterspeak :slide-furre-y t ()
  "Slide furre's X offset.
Not implemented.")

;; 22 - :SLIDE-OPACITY

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Camera control

;; 13 - :CAMERA-FOLLOW-FURRE-P

(define-kitterspeak :camera-follow-furre-p t ()
  "Sets if the camera should follow furre position.
Not implemented.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Effect layer

;; 33 - :EFFECT-LAYER-MODE

(define-kitterspeak :effect-layer-mode t ()
  "Sets if effect frames should wrap shape frames or be wrapped by them.
Not implemented.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Legacy rules

;; 1 - :SHOW-FRAME

(define-kitterspeak :show-behind-frame t (animation nframe)
  "Shows frame number NFRAME on layer BEHIND.
Legacy - replaced by :SHOW-BEHIND-FRAME."
  (let ((frame (nth nframe (children current-shape))))
    (draw-frame animation frame :behind)))

;; 9 - :DRAW-FRONT

;; TODO

;; 10 - :DRAW-BEHIND

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hic sunt dracones

;; 16 - :SHAPE-FRAME

(define-kitterspeak :shape-frame t ()
  "Deprecated, not implemented.")

;; 25 - :SHOW-BG-OBJECT

(define-kitterspeak :show-bg-object t ()
  "Deprecated, not implemented.")

;; 26 - :SHOW-FG-OBJECT

(define-kitterspeak :show-fg-object t ()
  "Deprecated, not implemented.")

;; 27 - :HIDE-BG

(define-kitterspeak :hide-bg t ()
  "Deprecated, not implemented.")

;; 28 - :HIDE-FG

(define-kitterspeak :hide-fg t ()
  "Deprecated, not implemented.")
