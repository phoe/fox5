;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; kitterspeak.lisp

(in-package :fox5/qt)
(in-readtable :qtools)

;;; Utils

(defun make-text-qtoolbutton (text)
  (let ((button (q+:make-qtoolbutton)))
    (setf (q+:text button) text
          (q+:tool-button-style button) (q+:qt.tool-button-text-only))
    button))

;;; Widget

(define-widget animator (qwidget)
  (;; Shape and color code for remapping
   (shape :accessor shape
          :initarg :shape
          :initform nil)
   (color-code :accessor color-code
               :initarg :color-code
               :initform nil)
   ;; Layers
   (frame-layers :reader frame-layers
                 :initform (make-array 4 :initial-element nil))
   (layers :reader layers
           :initform (make-array 8 :initial-element nil))
   ;; Animation state
   (current-step :accessor current-step
                 :initform 0)
   ;; (group :accessor group
   ;;        :initform nil)
   ;; (delay-min :accessor delay-min
   ;;            :initform 0)
   ;; (delay-max :accessor delay-max
   ;;            :initform 0)
   ;; (auto-delay-p :accessor auto-delay-p
   ;;               :initform t)
   )
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

(define-subwidget (animator scene) (q+:make-qgraphicsscene))

;; (define-subwidget (animator x-animation)
;;     (q+:make-qpropertyanimation group "x"))

;; (define-subwidget (animator y-animation)
;;     (q+:make-qpropertyanimation group "y"))

;; (define-subwidget (animator opacity) (q+:make-qgraphicsopacityeffect)
;;   (setf (q+:opacity opacity) 1))

;; (define-subwidget (animator a-animation)
;;     (q+:make-qpropertyanimation opacity "opacity"))

(define-finalizer (animator finalize-animator)
  (finalize scene)
  ;; (finalize x-animation)
  ;; (finalize y-animation)
  ;; (finalize a-animation)
  )

(define-subwidget (animator layout) (q+:make-qgridlayout)
  (setf (q+:layout animator) layout
        (q+:contents-margins layout) (values 0 0 0 0)))

(define-subwidget (animator preview) (q+:make-qgraphicsview scene)
  (q+:add-widget layout preview 0 0 1 2)
  (setf (q+:row-stretch layout 0) 9001))

;; (define-subwidget (animator prev-button) (make-text-qtoolbutton "<")
;;   (q+:add-widget layout prev-button 1 0)
;;   (setf (q+:size-policy prev-button)
;;         (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.expanding))))

;; (define-subwidget (animator next-button) (make-text-qtoolbutton ">")
;;   (q+:add-widget layout next-button 1 1)
;;   (setf (q+:size-policy next-button)
;;         (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.expanding))))

;;; Functions

;; (defun ensure-group (animator)
;;   (with-slots-bound (animator animator)
;;     (unless group
;;       (setf group (q+:make-qgraphicsitemgroup)))))

(defun update (animator)
  (with-slots-bound (animator animator)
    ;; TODO optimize
    (q+:clear scene)
    ;; TODO multiple shadows will not get drawn correctly because 0.5 dark
    ;; opacity * 0.5 dark opacity will turn into 0.75 dark opacity, not 0.5
    (loop for item across layers when item do (q+:add-item scene item))))

(defun draw-shape (animator shape)
  (setf (shape animator) shape)
  (with-slots-bound (animator animator)
    (cond
      ((kitterspeak shape)
       (execute-kitterspeak animator))
      ((eq (edit-type (parent shape)) :effect)
       (draw-frame animator (first (children shape)) :front))
      (t
       (draw-frame animator (first (children shape)) :behind)))))

(defun draw-frame (animator frame layer)
  (with-slots-bound (animator animator)
    (let* ((shadow-index (ecase layer (:bg 0) (:behind 1) (:front 2) (:fg 3)))
           (data-index (+ shadow-index 4))
           (x-offset (getf (frame-offset frame) :x))
           (y-offset (getf (frame-offset frame) :y)))
      ;; remove frame from layers and frame-layers
      ;; (loop for frame-layer across frame-layers
      ;;       for i from 0
      ;;       if (eq frame-layer frame)
      ;;         do (setf (aref frame-layers i) nil
      ;;                  (aref layers i) nil
      ;;                  (aref layers (+ 4 i)) nil))
      ;; add frame to frame-layers
      ;; (setf (aref frame-layers shadow-index) frame)
      ;; add items to layers
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

(defun execute-kitterspeak (animator)
  "Begins Kitterspeak execution in the provided animator."
  (with-slots-bound (animator animator)
    (with-accessors ((kitterspeak kitterspeak)) shape
      (when kitterspeak
        (when (null (nth current-step kitterspeak))
          (setf current-step 0))
        (loop repeat *max-kitterspeak-steps*
              for step = (nth current-step kitterspeak) ;; TODO optimize NTH
              for (type arg1 arg2) = step
              for continuep = (funcall #'execute-step animator type arg1 arg2)
              if continuep
                do (incf current-step)
              else
                return nil)
        (update animator)))))

(defgeneric execute-step (animator type arg1 arg2)
  (:documentation "Executes the Kitterspeak step denoted by TYPE, ARG1 and ARG2.
Must return non-NIL if the execution is meant to be continued after this step,
or NIL if it should be paused."))

(defmethod execute-step
    ((animator animator) (type null) (arg1 null) (arg2 null))
  "This method is called when the end of KS has been reached. Returns NIL.")

(defmacro define-kitterspeak
    (type continuep (&optional (animator 'animator) (arg1 'arg1) (arg2 'arg2))
     &body body)
  "Defines a method for parsing Kitterspeak for line TYPE. Return value of BODY
is ignored.  CONTINUEP states if Kitterspeak processing should continue after
the body of this method (in case of most steps), or if it should pause (forever,
in case of :STOP, or until the timer fires next time, in case of delays)."
  (when type
    (assert (member type *kitterspeak* :key #'cdr) ()
            "~S is not a valid Kitterspeak type." type))
  `(defmethod execute-step
       ((,animator animator) (type (eql ,type)) ,arg1 ,arg2)
     ,@(if (and body (stringp (first body)))
           `(,(car body)
             (with-slots-bound (,animator animator)
               ,@(cdr body)))
           `(with-slots-bound (,animator animator)
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

(define-kitterspeak :show-bg-frame t (animator nframe)
  "Shows frame number NFRAME on layer BG."
  (let ((frame (nth nframe (children shape))))
    (draw-frame animator frame :bg)))

;; 29 - :SHOW-BEHIND-FRAME

(define-kitterspeak :show-behind-frame t (animator nframe)
  "Shows frame number NFRAME on layer BEHIND."
  (let ((frame (nth nframe (children shape))))
    (draw-frame animator frame :behind)))

;; 30 - :SHOW-FRONT-FRAME

(define-kitterspeak :show-front-frame t (animator nframe)
  "Shows frame number NFRAME on layer FRONT."
  (let ((frame (nth nframe (children shape))))
    (draw-frame animator frame :front)))

;; 24 - :SHOW-FG-FRAME

(define-kitterspeak :show-fg-frame t (animator nframe)
  "Shows frame number NFRAME on layer FG."
  (let ((frame (nth nframe (children shape))))
    (draw-frame animator frame :fg)))

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

(define-kitterspeak :jump t (animator nstep)
  "Jumps to step N."
  (setf current-step (1- nstep)))

;; 12 - :STOP

(define-kitterspeak :stop nil (animator)
  "Stops Kitterspeak execution. If *KITTERSPEAK-STOP-DELAY* is set, executes
a delay defined by that variable instead."
  (when-let ((delay *kitterspeak-stop-delay*))
    (execute-step animator :delay delay 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Offset - set

;; 5 - :FRAME-X

(define-kitterspeak :frame-x t (animator offset)
  "Overrides the X offset for all frames."
  ;; TODO
  )

;; 6 - :FRAME-Y

(define-kitterspeak :frame-y t (animator offset)
  "Overrides the Y offset for all frames."
  ;; TODO
  )

;; 17 - :OPACITY

(define-kitterspeak :opacity t (animator opacity)
  "Overrides the total opacity of the image."
  ;; TODO
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Offset - slide

;; 18 - :SLIDE-FRAME-X

;; TODO

;; 19 - :SLIDE-FRAME-Y

;; TODO

;; 22 - :SLIDE-OPACITY

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Legacy rules

;; 1 - :SHOW-FRAME

(define-kitterspeak :show-frame t (animator nframe)
  "Shows frame number NFRAME on layer BEHIND.
Legacy - replaced by :SHOW-BEHIND-FRAME."
  ;; TODO
  )

;; 9 - :DRAW-FRONT

;; TODO

;; 10 - :DRAW-BEHIND

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Not implemented

;; 7 - :FURRE-X

(define-kitterspeak :furre-x t ()
  "Set furre's X offset.
Not implemented.")

;; 8 - :FURRE-Y

(define-kitterspeak :furre-y t ()
  "Set furre's Y offset.
Not implemented.")

;; 20 - :SLIDE-FURREX

(define-kitterspeak :slide-furre-x t ()
  "Slide furre's X offset.
Not implemented.")

;; 21 - :SLIDE-FURREY

(define-kitterspeak :slide-furre-y t ()
  "Slide furre's X offset.
Not implemented.")

;; 13 - :CAMERA-FOLLOW-FURRE-P

(define-kitterspeak :camera-follow-furre-p t ()
  "Sets if the camera should follow furre position.
Not implemented.")

;; 33 - :EFFECT-LAYER-MODE

(define-kitterspeak :effect-layer-mode t ()
  "Sets if effect frames should wrap shape frames or be wrapped by them.
Not implemented.")

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
