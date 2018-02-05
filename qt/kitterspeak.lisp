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
   (auto-delay-min :accessor auto-delay-min
                   :initform 0)
   (auto-delay-max :accessor auto-delay-max
                   :initform 0)
   (auto-delay-p :accessor auto-delay-p
                 :initform t))
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
      (loop for frame-layer across frame-layers
            for i from 0
            if (eq frame-layer frame)
              do (setf (aref frame-layers i) nil
                       (aref layers i) nil
                       (aref layers (+ 4 i)) nil))
      ;; add frame to frame-layers
      (setf (aref frame-layers shadow-index) frame)
      ;; add items to layers
      (dolist (sprite (children frame))
        (let ((item (sprite-item sprite color-code x-offset y-offset)))
          (case (purpose sprite)
            ((nil :remapping-data)
             ;; delete any former contents of the frame
             (when-let ((item (aref layers data-index)))
               (finalize item))
             ;; set new contents
             (setf (aref layers data-index) item
                   (q+:zvalue item) (- 8 data-index)))
            ((:shadow-layer)
             ;; delete any former contents of the frame
             (when-let ((item (aref layers shadow-index)))
               (finalize item))
             ;; set new contents
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
        ;; TODO turn kitterspeak into array one day
        (loop repeat *max-kitterspeak-steps*
              for step = (nth current-step kitterspeak)
              for (type arg1 arg2) = step
              for continuep = (funcall #'execute-step animator type arg1 arg2)
              do (incf current-step)
              unless continuep
                return nil)
        (update animator)))))

(define-slot (animator execute-kitterspeak) ()
  (execute-kitterspeak animator))

(defgeneric execute-step (animator type arg1 arg2)
  (:documentation "Executes the Kitterspeak step denoted by TYPE, ARG1 and ARG2.
Must return non-NIL if the execution is meant to be continued after this step,
or NIL if it should be paused."))

(defmacro check-kitterspeak-type (type)
  "Checks if the provided symbol is a valid Kitterspeak type."
  `(when ,type (assert (member ,type *kitterspeak* :key #'cdr) ()
                       "~S is not a valid Kitterspeak type." ,type)))

(defmacro define-kitterspeak
    (type (&optional (animator 'animator) (arg1 'arg1) (arg2 'arg2))
     &body body)
  "Defines a method for parsing Kitterspeak for line TYPE. Return value of BODY
states if Kitterspeak processing should continue after the body of this method
\(in case of most steps), or if it should pause (forever, in case of :STOP, or
until the timer fires next time, in case of delays)."
  `(progn
     (check-kitterspeak-type ,type)
     (defmethod execute-step
         ((,animator animator) (type (eql ,type)) ,arg1 ,arg2)
       ,@(if (and body (stringp (first body)))
             `(,(car body)
               (with-slots-bound (,animator animator)
                 ,@(cdr body)))
             `(with-slots-bound (,animator animator)
                ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Null case

;; 0 - NIL

(define-kitterspeak nil (animator)
  "Null method. Called each time the Kitterspeak reaches its end. This method's
 sole purpose is to loop back to the beginning."
  (execute-step animator :jump 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame order

;; 23 - :SHOW-BG-FRAME

(define-kitterspeak :show-bg-frame (animator nframe)
  "Shows frame number NFRAME on layer BG."
  (let ((frame (nth nframe (children shape))))
    (draw-frame animator frame :bg))
  t)

;; 29 - :SHOW-BEHIND-FRAME

(define-kitterspeak :show-behind-frame (animator nframe)
  "Shows frame number NFRAME on layer BEHIND."
  (let ((frame (nth nframe (children shape))))
    (draw-frame animator frame :behind))
  t)

;; 30 - :SHOW-FRONT-FRAME

(define-kitterspeak :show-front-frame (animator nframe)
  "Shows frame number NFRAME on layer FRONT."
  (let ((frame (nth nframe (children shape))))
    (draw-frame animator frame :front))
  t)

;; 24 - :SHOW-FG-FRAME

(define-kitterspeak :show-fg-frame (animator nframe)
  "Shows frame number NFRAME on layer FG."
  (let ((frame (nth nframe (children shape))))
    (draw-frame animator frame :fg))
  t)

;; 31 - :MOVE-FORWARD

;; TODO

;; 32 - :MOVE-BACKWARD

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flow control

;; 2 - :DELAY

(define-kitterspeak :delay (animator msec)
  "Delays Kitterspeak execution for X milliseconds."
  (q+:qtimer-single-shot msec animator (qslot "executeKitterspeak()"))
  nil)

;; 15 - :RANDOM-DELAY

(define-kitterspeak :random-delay (animator msec1 msec2)
  "Delays Kitterspeak execution for a random amount of time between X and Y
milliseconds."
  (let* ((diff (1+ (- msec2 msec1)))
         (time (if (positive-real-p diff) (+ msec1 (random diff)) 0)))
    (execute-step animator :delay time 0))
  nil)

;; 11 - :AUTO-FRAME-DELAY

;; TODO

;; 14 - :RANDOM-AUTO-DELAY

;; TODO

;; 3 - :LOOP

;; TODO

;; 4 - :JUMP

(define-kitterspeak :jump (animator nstep)
  "Jumps to step N."
  (setf current-step (1- nstep))
  t)

;; 12 - :STOP

(define-kitterspeak :stop (animator)
  "Stops Kitterspeak execution. If *KITTERSPEAK-STOP-DELAY* is set, executes
a delay defined by that variable instead."
  (when-let ((delay *kitterspeak-stop-delay*))
    (execute-step animator :delay delay 0))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Offset - set

;; 5 - :FRAME-X

(define-kitterspeak :frame-x (animator offset)
  "Overrides the X offset for all frames."
  ;; TODO
  t)

;; 6 - :FRAME-Y

(define-kitterspeak :frame-y (animator offset)
  "Overrides the Y offset for all frames."
  ;; TODO
  t)

;; 17 - :OPACITY

(define-kitterspeak :opacity (animator opacity)
  "Overrides the total opacity of the image."
  ;; TODO
  t)

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

(define-kitterspeak :show-frame (animator nframe)
  "Shows frame number NFRAME on layer BEHIND.
Legacy - replaced by :SHOW-BEHIND-FRAME."
  ;; TODO
  t)

;; 9 - :DRAW-FRONT

;; TODO

;; 10 - :DRAW-BEHIND

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Not implemented

;; 7 - :FURRE-X

(define-kitterspeak :furre-x ()
  "Set furre's X offset. Not implemented."
  t)

;; 8 - :FURRE-Y

(define-kitterspeak :furre-y ()
  "Set furre's Y offset. Not implemented."
  t)

;; 20 - :SLIDE-FURREX

(define-kitterspeak :slide-furre-x ()
  "Slide furre's X offset. Not implemented."
  t)

;; 21 - :SLIDE-FURREY

(define-kitterspeak :slide-furre-y ()
  "Slide furre's X offset. Not implemented."
  t)

;; 13 - :CAMERA-FOLLOW-FURRE-P

(define-kitterspeak :camera-follow-furre-p ()
  "Sets if the camera should follow furre position. Not implemented."
  t)

;; 33 - :EFFECT-LAYER-MODE

(define-kitterspeak :effect-layer-mode ()
  "Sets if effect frames should wrap shape frames or be wrapped by them. Not
implemented."
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hic sunt dracones

;; 16 - :SHAPE-FRAME

(define-kitterspeak :shape-frame ()
  "Deprecated, not implemented."
  t)

;; 25 - :SHOW-BG-OBJECT

(define-kitterspeak :show-bg-object ()
  "Deprecated, not implemented."
  t)

;; 26 - :SHOW-FG-OBJECT

(define-kitterspeak :show-fg-object ()
  "Deprecated, not implemented."
  t)

;; 27 - :HIDE-BG

(define-kitterspeak :hide-bg ()
  "Deprecated, not implemented."
  t)

;; 28 - :HIDE-FG

(define-kitterspeak :hide-fg ()
  "Deprecated, not implemented."
  t)
