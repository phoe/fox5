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

(define-widget slow-animator (qwidget)
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
   (x-offsets :reader x-offsets
              :initform (make-array 8 :initial-element 0))
   (y-offsets :reader y-offsets
              :initform (make-array 8 :initial-element 0))
   ;; Animation state
   (current-step :accessor current-step
                 :initform 0)
   ;; (group :accessor group
   ;;        :initform nil)
   (auto-delay-min :accessor auto-delay-min
                   :initform 0)
   (auto-delay-max :accessor auto-delay-max
                   :initform 0)
   (loop-counters :accessor loop-counters
                  :initform (make-hash-table))
   ;; (active-legacy-frame :accessor active-legacy-frame
   ;;                      :initform :behind)
   (execute-kitterspeak-p :accessor execute-kitterspeak-p
                          :initform t)
   ;; X animation
   (x-start :accessor x-start
            :initform 0)
   (x-end :accessor x-end
          :initform 0)
   (x-value :accessor x-value
            :initform nil)
   (x-start-time :accessor x-start-time
                 :initform 0)
   (x-duration :accessor x-end-time
               :initform 0)
   ;; Y animation
   (y-start :accessor y-start
            :initform 0)
   (y-end :accessor y-end
          :initform 0)
   (y-value :accessor y-value
            :initform nil)
   (y-start-time :accessor y-start-time
                 :initform 0)
   (y-duration :accessor y-end-time
               :initform 0)
   ;; Opacity animation
   (opacity-start :accessor opacity-start
                  :initform 0)
   (opacity-end :accessor opacity-end
                :initform 0)
   (opacity-value :accessor opacity-value
                  :initform nil)
   (opacity-start-time :accessor opacity-start-time
                       :initform 0)
   (opacity-duration :accessor opacity-end-time
                     :initform 0))
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

(define-subwidget (slow-animator scene) (q+:make-qgraphicsscene))

(define-subwidget (slow-animator layout) (q+:make-qgridlayout)
  (setf (q+:layout slow-animator) layout
        (q+:contents-margins layout) (values 0 0 0 0)))

(define-subwidget (slow-animator preview) (q+:make-qgraphicsview scene)
  (q+:add-widget layout preview 0 0 1 1)
  ;; (setf (q+:scene-rect preview) (values 0 0 100 100))
  )

(define-subwidget (slow-animator opacity) (q+:make-qgraphicsopacityeffect)
  (setf (q+:opacity opacity) 1
        (q+:graphics-effect preview) opacity))

(defparameter *timer-interval* 10)

(define-subwidget (slow-animator x-timer) (q+:make-qtimer)
  (setf (q+:interval x-timer) *timer-interval*))

;; TODO write a Kitterspeak compiler that unrolls loops, upgrades legacy rules
;; and auto-resolves clashing slide durations

;; TODO the slots should be defined on a per-shape object, so animations
;; for different shapes do not clash with each other
(define-slot (slow-animator update-x-animation) ()
  (declare (connected x-timer (timeout)))
  (let* ((now (get-internal-real-time))
         (diff (/ (* (- now x-start-time) 1000) internal-time-units-per-second))
         (percentage (min 1.0 (/ diff (max x-duration 1))))
         (x (lerp percentage x-start x-end)))
    (setf x-value x)
    (loop for item across layers
          for offset across x-offsets
          when item do (setf (q+:x item) (+ offset x)))
    (unless (= percentage 1.0)
      (q+:start x-timer))))

(define-subwidget (slow-animator y-timer) (q+:make-qtimer)
  (setf (q+:interval y-timer) *timer-interval*))

(define-slot (slow-animator update-y-animation) ()
  (declare (connected y-timer (timeout)))
  (let* ((now (get-internal-real-time))
         (diff (/ (* (- now y-start-time) 1000)
                  internal-time-units-per-second))
         (percentage (min 1 (/ diff (max y-duration 1))))
         (y (lerp percentage y-start y-end)))
    (setf y-value y)
    (loop for item across layers
          for offset across y-offsets
          when item do (setf (q+:y item) (+ offset y)))
    (unless (= percentage 1)
      (q+:start y-timer))))

(define-subwidget (slow-animator opacity-timer) (q+:make-qtimer)
  (setf (q+:interval opacity-timer) *timer-interval*))

(define-slot (slow-animator update-opacity-animation) ()
  ;; TODO broken, make it work better one day
  (declare (connected opacity-timer (timeout)))
  (let* ((now (get-internal-real-time))
         (diff (/ (* (- now opacity-start-time) 1000)
                  internal-time-units-per-second))
         (percentage (min 1 (/ diff (max opacity-duration 1))))
         (value (lerp percentage opacity-start opacity-end)))
    (setf opacity-value value)
    (loop for item across layers when item
          do (setf (q+:opacity item) opacity-value))
    (unless (= percentage 1)
      (q+:start opacity-timer))))

;;; Functions

;; (defun ensure-group (slow-animator)
;;   (with-slots-bound (slow-animator slow-animator)
;;     (unless group
;;       (setf group (q+:make-qgraphicsitemgroup)))))

(defun update (slow-animator)
  (with-slots-bound (slow-animator slow-animator)
    (let ((items (q+:items scene)))
      ;; TODO optimize
      (dolist (item items)
        (unless (find item layers)
          (finalize item)))
      ;; TODO compress shadows into one group and apply an opacity effect to it.
      ;; multiple shadows will not get drawn correctly because 0.5 dark
      ;; opacity * 0.5 dark opacity will turn into 0.75 dark opacity, not 0.5
      (loop for item across layers
            for i from 0
            when item unless (find item items)
              do (q+:add-item scene item)
                 (setf (q+:zvalue item) i)))))

(defun legacy-layer (shape)
  (if (eq (edit-type (parent shape)) :effect) :front :behind))

(defun draw-shape (slow-animator shape)
  (setf (shape slow-animator) shape)
  (with-slots-bound (slow-animator slow-animator)
    (draw-frame slow-animator (first (children shape)) (legacy-layer shape))
    (if (kitterspeak shape)
        (execute-kitterspeak slow-animator)
        (update slow-animator))))

(defun draw-frame (slow-animator frame layer)
  (with-slots-bound (slow-animator slow-animator)
    (let* ((shadow-index (ecase layer (:bg 0) (:behind 1) (:front 2) (:fg 3)))
           (data-index (+ shadow-index 4))
           (x-offset (or x-value (getf (frame-offset frame) :x)))
           (y-offset (or y-value (getf (frame-offset frame) :y))))
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
        (multiple-value-bind (item sprite-x sprite-y)
            (sprite-item sprite color-code x-offset y-offset)
          (case (purpose sprite)
            ((nil :remapping-data)
             ;; delete any former contents of the frame
             (when-let ((item (aref layers data-index)))
               (finalize item))
             ;; set new contents
             (setf (aref layers data-index) item
                   (aref x-offsets data-index) sprite-x
                   (aref y-offsets data-index) sprite-y))
            ((:shadow-layer)
             ;; delete any former contents of the frame
             (when-let ((item (aref layers shadow-index)))
               (finalize item))
             ;; set new contents
             (setf (aref layers shadow-index) item
                   (aref x-offsets shadow-index) sprite-x
                   (aref y-offsets shadow-index) sprite-y
                   (q+:opacity item) 0.5))))))))

;; TODO https://stackoverflow.com/questions/7451183/

(defun sprite-item (sprite &optional color-code x-offset y-offset)
  (let* ((offset (offset sprite))
         (image-id (1- (image-id sprite)))
         (file (nth-funcall #'parent 4 sprite))
         (image (nth image-id (images file)))
         (pixmap (image-qpixmap image color-code))
         (item (q+:make-qgraphicspixmapitem pixmap))
         (sprite-x (getf offset :x))
         (sprite-y (getf offset :y)))
    (setf (q+:offset item) (values (+ (or x-offset 0) sprite-x)
                                   (+ (or y-offset 0) sprite-y)))
    (values item sprite-x sprite-y)))

;; TODO do something with it perhaps
(defun sprite-proxyitem (sprite &optional color-code x-offset y-offset)
  (let* ((offset (offset sprite))
         (image-id (1- (image-id sprite)))
         (file (nth-funcall #'parent 4 sprite))
         (image (nth image-id (images file)))
         (pixmap (image-qpixmap image color-code))
         (label (q+:make-qlabel))
         (item (q+:make-qgraphicsproxywidget)))
    (setf (q+:widget item) label
          (q+:pixmap label) pixmap
          (q+:margin label) 0
          (q+:style-sheet label) "background: transparent;")
    (setf (q+:pos item) (values (+ (or x-offset 0) (getf offset :x))
                                (+ (or y-offset 0) (getf offset :y))))
    item))

;; TODO remove
(defvar *sprite*)

;; TODO remove
(defun test-property-animation ()
  (with-finalizing* ((item (sprite-proxyitem *sprite*))
                     (label (q+:widget item))
                     (scene (q+:make-qgraphicsscene))
                     (animation (q+:make-qpropertyanimation label "geometry")))
    (let* ((pixmap (q+:pixmap (q+:widget item)))
           (width (q+:width pixmap))
           (height (q+:height pixmap)))
      (with-main-window (view (q+:make-qgraphicsview scene))
        (q+:add-item scene item)
        (setf (q+:duration animation) 20000
              (q+:start-value animation) (q+:make-qrectf -600 0 width height)
              (q+:end-value animation) (q+:make-qrectf 600 0 width height))
        (q+:start animation)))))

;; TODO remove
(defun make-test-animation ()
  (let* ((item (sprite-proxyitem *sprite*))
         (label (q+:widget item))
         (scene (q+:make-qgraphicsscene))
         (animation (q+:make-qpropertyanimation label "geometry"))
         (pixmap (q+:pixmap (q+:widget item)))
         (width (q+:width pixmap))
         (height (q+:height pixmap))
         (view (q+:make-qgraphicsview scene)))
    (q+:add-item scene item)
    (setf (q+:duration animation) 20000
          (q+:start-value animation) (q+:make-qrectf -600 0 width height)
          (q+:end-value animation) (q+:make-qrectf 600 0 width height))
    (q+:start animation)
    (setf (q+:minimum-width view) 1500)
    view))

;; TODO remove
(defun test-animation ()
  (with-main-window (main-window (q+:make-qwidget))
    (let ((layout (q+:make-qvboxlayout)))
      (setf (q+:layout main-window) layout)
      (dotimes (i 10)
        (q+:add-widget layout (make-test-animation))))))

(defvar *max-kitterspeak-steps* 50
  "The maximum number of steps to execute.")

(defvar *kitterspeak-stop-delay* 2000
  "Defines if Kitterspeak :STOP step should stop Kitterspeak execution forever
or rather induce a delay before restarting it.
If NIL, then STOP commands completely stop Kitterspeak execution.
If non-NIL, then the value of this variable is the number of milliseconds
that will pass before KS execution is restarted from the beginning.")

(defun execute-kitterspeak (slow-animator)
  "Begins Kitterspeak execution in the provided slow-animator."
  (with-slots-bound (slow-animator slow-animator)
    (with-accessors ((kitterspeak kitterspeak)) shape
      (analyze-kitterspeak slow-animator)
      (when (and kitterspeak execute-kitterspeak-p)
        ;; TODO optimize simple kitterspeak use cases
        ;; TODO turn kitterspeak into array one day
        (loop
          repeat *max-kitterspeak-steps*
          for step = (nth current-step kitterspeak)
          for (type arg1 arg2) = step
          for continuep = (funcall #'execute-step slow-animator type arg1 arg2)
          for next = (nth (1+ current-step) kitterspeak)
          do (incf current-step)
          when (member type '(:show-bg-frame :show-behind-frame
                              :show-front-frame :show-fg-frame
                              :show-frame))
            unless (member (car next) '(:delay :random-delay))
              do (execute-step slow-animator :random-delay
                               auto-delay-min auto-delay-max)
                 (return nil)
          unless continuep
            return nil)
        (update slow-animator)))))

(defvar *kitterspeak-ignored-keywords*
  '(:furre-x :furre-y :slide-furre-x :slide-furre-y :camera-follow-furre-p
    :effect-layer-mode :shape-frame :show-bg-object :show-fg-object :hide-bg
    :hide-fg)
  "Kitterspeak rule types that are ignored by the implementation.")

(defvar *kitterspeak-execute-once-keywords*
  `(,@*kitterspeak-ignored-keywords*
    ,@'(:show-bg-frame :show-behind-frame :show-front-frame :show-fg-frame
        :move-forward :move-backward :loop :jump :frame-x :frame-y :opacity
        :show-frame :draw-front :draw-behind))
  "Kitterspeak rule types allowed in script that is meant to be executed once.")

(defvar *kitterspeak-execute-nil-keywords*
  `(,@*kitterspeak-ignored-keywords*
    ,@'(:delay :random-delay :auto-delay :random-auto-delay :loop :jump :stop
        :frame-x :frame-y :opacity))
  "Kitterspeak rule types allowed in script that is meant not to be executed at
all.")

(defun analyze-kitterspeak (slow-animator)
  "Analyzes the kitterspeak for possible optimizations and sets the
EXECUTE-KITTERSPEAK-P variable to NIL (no execution), :ONCE (execute but don't
loop at end), and T (full execution)."
  (with-slots-bound (slow-animator slow-animator)
    (with-accessors ((kitterspeak kitterspeak)) shape
      (let ((types (mapcar #'car kitterspeak)))
        (cond
          ((every (rcurry #'member *kitterspeak-execute-once-keywords*) types)
           (setf execute-kitterspeak-p :once))
          ((every (rcurry #'member *kitterspeak-execute-nil-keywords*) types)
           (setf execute-kitterspeak-p nil)))))))

(define-slot (slow-animator execute-kitterspeak) ()
  ;; TODO differentiate based on some serial, so we don't execute kitterspeak
  ;; if the current shape differs from the one for which the slot was called
  (execute-kitterspeak slow-animator))

(defgeneric execute-step (slow-animator type arg1 arg2)
  (:documentation "Executes the Kitterspeak step denoted by TYPE, ARG1 and ARG2.
Must return non-NIL if the execution is meant to be continued after this step,
or NIL if it should be paused."))

(defmethod execute-step :before (animator type a1 a2)
  ;;(sleep 0.1)
  )

(defmacro check-kitterspeak-type (type)
  "Checks if the provided symbol is a valid Kitterspeak type."
  `(when ,type (assert (member ,type *kitterspeak* :key #'cdr) ()
                       "~S is not a valid Kitterspeak type." ,type)))

(defmacro define-kitterspeak
    (type (&optional (slow-animator 'slow-animator) (arg1 'arg1) (arg2 'arg2))
     &body body)
  "Defines a method for parsing Kitterspeak for line TYPE. Return value of BODY
states if Kitterspeak processing should continue after the body of this method
\(in case of most steps), or if it should pause (forever, in case of :STOP, or
until the timer fires next time, in case of delays)."
  `(progn
     (check-kitterspeak-type ,type)
     (defmethod execute-step
         ((,slow-animator slow-animator) (type (eql ,type)) ,arg1 ,arg2)
       ,@(if (and body (stringp (first body)))
             `(,(car body)
               (with-slots-bound (,slow-animator slow-animator)
                 ,@(cdr body)))
             `((with-slots-bound (,slow-animator slow-animator)
                 ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Null case

;; 0 - NIL

(define-kitterspeak nil (slow-animator)
  "Null method. Called each time the Kitterspeak reaches its end. This method's
 sole purpose is to loop back to the beginning."
  (unless (eq execute-kitterspeak-p :once)
    (execute-step slow-animator :jump 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame order

;; 23 - :SHOW-BG-FRAME

(define-kitterspeak :show-bg-frame (slow-animator nframe)
  "Shows frame number NFRAME on layer BG."
  (when-let ((frame (nth nframe (children shape))))
    (draw-frame slow-animator frame :bg))
  t)

;; 29 - :SHOW-BEHIND-FRAME

(define-kitterspeak :show-behind-frame (slow-animator nframe)
  "Shows frame number NFRAME on layer BEHIND."
  (when-let ((frame (nth nframe (children shape))))
    (draw-frame slow-animator frame :behind))
  t)

;; 30 - :SHOW-FRONT-FRAME

(define-kitterspeak :show-front-frame (slow-animator nframe)
  "Shows frame number NFRAME on layer FRONT."
  (when-let ((frame (nth nframe (children shape))))
    (draw-frame slow-animator frame :front))
  t)

;; 24 - :SHOW-FG-FRAME

(define-kitterspeak :show-fg-frame (slow-animator nframe)
  "Shows frame number NFRAME on layer FG."
  (when-let ((frame (nth nframe (children shape))))
    (draw-frame slow-animator frame :fg))
  t)

;; 31 - :MOVE-FORWARD

(define-kitterspeak :move-forward (slow-animator nframe)
  "Moves frame NFRAME forward using the following conditions."
  (when-let* ((frame (nth nframe (children shape)))
              (position (position frame frame-layers)))
    (let ((shadow-position (+ 4 position)))
      (cond
        ;; layer is already in the foreground - do nothing
        ((= 3 position))
        ;; there is free space 1 layer before us
        ((and (>= 2 position)
              (null (aref frame-layers (+ 1 position))))
         (rotatef (aref layers position)
                  (aref layers (+ 1 position)))
         (rotatef (aref layers shadow-position)
                  (aref layers (+ 1 shadow-position)))
         (rotatef (aref frame-layers position)
                  (aref frame-layers (+ 1 position))))
        ;; there is free space 2 layers before us
        ((and (>= 1 position)
              (null (aref frame-layers (+ 2 position))))
         (rotatef (aref layers position)
                  (aref layers (+ 1 position))
                  (aref layers (+ 2 position)))
         (rotatef (aref layers shadow-position)
                  (aref layers (+ 1 shadow-position))
                  (aref layers (+ 2 shadow-position)))
         (rotatef (aref frame-layers position)
                  (aref frame-layers (+ 1 position))
                  (aref frame-layers (+ 2 position))))
        ;; there is free space 3 layers before us
        ((and (>= 0 position)
              (null (aref frame-layers (+ 3 position))))
         (rotatef (aref layers position)
                  (aref layers (+ 1 position))
                  (aref layers (+ 2 position))
                  (aref layers (+ 3 position)))
         (rotatef (aref layers shadow-position)
                  (aref layers (+ 1 shadow-position))
                  (aref layers (+ 2 shadow-position))
                  (aref layers (+ 3 shadow-position)))
         (rotatef (aref frame-layers position)
                  (aref frame-layers (+ 1 position))
                  (aref frame-layers (+ 2 position))
                  (aref frame-layers (+ 3 position))))
        ;; no free space, swap layers.
        (t
         (rotatef (aref frame-layers position)
                  (aref frame-layers (+ 1 position))))))))

;; 32 - :MOVE-BACKWARD

(define-kitterspeak :move-backward (slow-animator nframe)
  "Moves frame NFRAME backward using the following conditions."
  (when-let* ((frame (nth nframe (children shape)))
              (position (position frame frame-layers)))
    (let ((shadow-position (+ 4 position)))
      (cond
        ;; layer is already in the background - do nothing
        ((= 0 position))
        ;; there is free space 1 layer after us
        ((and (<= 1 position)
              (null (aref frame-layers (- position 1))))
         (rotatef (aref layers position)
                  (aref layers (- position 1)))
         (rotatef (aref layers shadow-position)
                  (aref layers (- shadow-position 1)))
         (rotatef (aref frame-layers position)
                  (aref frame-layers (- position 1))))
        ;; there is free space 2 layers after us
        ((and (<= 2 position)
              (null (aref frame-layers (- position 2))))
         (rotatef (aref layers position)
                  (aref layers (- position 1))
                  (aref layers (- position 2)))
         (rotatef (aref layers shadow-position)
                  (aref layers (- shadow-position 1))
                  (aref layers (- shadow-position 2)))
         (rotatef (aref frame-layers position)
                  (aref frame-layers (- position 1))
                  (aref frame-layers (- position 2))))
        ;; there is free space 3 layers after us
        ((and (<= 3 position)
              (null (aref frame-layers (- position 3))))
         (rotatef (aref layers position)
                  (aref layers (- position 1))
                  (aref layers (- position 2))
                  (aref layers (- position 3)))
         (rotatef (aref layers shadow-position)
                  (aref layers (- shadow-position 1))
                  (aref layers (- shadow-position 2))
                  (aref layers (- shadow-position 3)))
         (rotatef (aref frame-layers position)
                  (aref frame-layers (- position 1))
                  (aref frame-layers (- position 2))
                  (aref frame-layers (- position 3))))
        ;; no free space, swap layers.
        (t
         (rotatef (aref frame-layers position)
                  (aref frame-layers (- position 1))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flow control

;; 2 - :DELAY

(define-kitterspeak :delay (slow-animator msec)
  "Delays Kitterspeak execution for X milliseconds."
  (q+:qtimer-single-shot msec slow-animator (qslot "executeKitterspeak()"))
  nil)

;; 15 - :RANDOM-DELAY

(define-kitterspeak :random-delay (slow-animator msec1 msec2)
  "Delays Kitterspeak execution for a random amount of time between X and Y
milliseconds."
  (let* ((diff (1+ (- msec2 msec1)))
         (sum (+ msec1 (random diff)))
         (time (if (positive-real-p sum) sum 0)))
    (execute-step slow-animator :delay time 0))
  nil)

;; 11 - :AUTO-DELAY

(define-kitterspeak :auto-delay (slow-animator msec)
  "Sets the automatic delay to the following value."
  (setf auto-delay-min msec
        auto-delay-max msec)
  t)

;; 14 - :RANDOM-AUTO-DELAY

(define-kitterspeak :random-auto-delay (slow-animator min max)
  "Sets the random automatic delay to the following values."
  (setf auto-delay-min min
        auto-delay-max max)
  t)

;; 3 - :LOOP

(define-kitterspeak :loop (slow-animator nstep count)
  "Loops to step NSTEP COUNT times, then continues."
  (let ((current (ensure-gethash current-step loop-counters 0)))
    (cond ((<= count current)
           (setf (gethash current-step loop-counters) 0))
          (t
           (incf (gethash current-step loop-counters))
           (execute-step slow-animator :jump nstep 0))))
  t)

;; 4 - :JUMP

(define-kitterspeak :jump (slow-animator nstep)
  "Jumps to step N."
  (setf current-step (1- nstep))
  t)

;; 12 - :STOP

(define-kitterspeak :stop (slow-animator)
  "Stops Kitterspeak execution. If *KITTERSPEAK-STOP-DELAY* is set, executes
a delay defined by that variable instead."
  (when-let ((delay *kitterspeak-stop-delay*))
    ;; TODO better cleaning of slow-animator state
    (execute-step slow-animator :jump 0 0)
    (execute-step slow-animator :delay delay 0))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Offset - set

;; 5 - :FRAME-X

(define-kitterspeak :frame-x (slow-animator offset)
  "Overrides the X offset for all frames."
  (execute-step slow-animator :slide-frame-x offset 0)
  t)

;; 6 - :FRAME-Y

(define-kitterspeak :frame-y (slow-animator offset)
  "Overrides the Y offset for all frames."
  (execute-step slow-animator :slide-frame-y offset 0)
  t)

;; 17 - :OPACITY

(define-kitterspeak :opacity (slow-animator value)
  "Overrides the total opacity of the image."
  (execute-step slow-animator :slide-opacity value 0)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Offset - slide

;; 18 - :SLIDE-FRAME-X

(define-kitterspeak :slide-frame-x (slow-animator offset msec)
  "Overrides the X offset for all frames."
  (let* ((layer (legacy-layer shape))
         (item (aref layers (ecase layer (:behind 5) (:front 6))))
         (start (or x-value (when item (q+:x item)) 0)))
    (setf x-start start
          x-end offset
          x-start-time (get-internal-real-time)
          x-duration msec)
    (q+:start x-timer))
  t)

;; 19 - :SLIDE-FRAME-Y

(define-kitterspeak :slide-frame-y (slow-animator offset msec)
  "Overrides the Y offset for all frames."
  (let* ((layer (legacy-layer shape))
         (item (aref layers (ecase layer (:behind 5) (:front 6))))
         (start (or y-value (when item (q+:y item)) 0)))
    (setf y-start start
          y-end offset
          y-start-time (get-internal-real-time)
          y-duration msec)
    (q+:start y-timer))
  t)

;; 22 - :SLIDE-OPACITY

(define-kitterspeak :slide-opacity (slow-animator value msec)
  "Overrides the opacity for whole image."
  (let* ((value (/ value 255.0))
         (start (or opacity-value (q+:opacity opacity))))
    (setf opacity-start start
          opacity-end value
          opacity-start-time (get-internal-real-time)
          opacity-duration msec)
    (q+:start opacity-timer))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Legacy rules

;; 1 - :SHOW-FRAME

(define-kitterspeak :show-frame (slow-animator nframe)
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









;; Object showcase

(define-widget showcase (qwidget)
  ((object :reader object
           :initarg :object
           :initform (error "Must specify an object."))
   (color-code :accessor color-code
               :initarg :color-code
               :initform nil)
   (current-shape :accessor current-shape
                  :initarg :current-shape
                  :initform 0)
   (shapes :accessor shapes
           :initarg shapes)
   (predicate :reader predicate
              :initarg :predicate
              :initform #'identity)))

(define-subwidget (showcase layout) (q+:make-qgridlayout)
  (setf (q+:layout showcase) layout
        (q+:row-stretch layout 0) 9001
        (q+:contents-margins layout) (values 4 4 4 4)))

(define-subwidget (showcase animator) (make-instance 'slow-animator)
  (q+:add-widget layout animator 0 0 1 2))

(define-subwidget (showcase prev-button) (make-text-qtoolbutton "<")
  (q+:add-widget layout prev-button 1 0)
  (setf (q+:size-policy prev-button)
        (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.expanding))))

(define-subwidget (showcase next-button) (make-text-qtoolbutton ">")
  (q+:add-widget layout next-button 1 1)
  (setf (q+:size-policy next-button)
        (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.expanding))))

(defun reset-animator (showcase)
  (with-slots-bound (showcase showcase)
    (finalize animator)
    (setf animator (make-instance 'slow-animator))
    (setf (color-code animator) color-code)
    (q+:add-widget layout animator 0 0 1 2)))

(define-slot (showcase prev-shape) ()
  (declare (connected prev-button (clicked)))
  (reset-animator showcase)
  (draw-object showcase :nshape (1- current-shape)))

(define-slot (showcase next-shape) ()
  (declare (connected next-button (clicked)))
  (reset-animator showcase)
  (draw-object showcase :nshape (1+ current-shape)))

(define-constructor (showcase complicate-shapes-p gender)
  (with-slots-bound (showcase showcase)
    (setf (color-code animator) color-code)
    (if complicate-shapes-p
        (setf (shapes showcase) (object-complex-avatar-shapes object gender))
        (setf (shapes showcase) (remove-if-not predicate (children object))))
    (draw-object showcase)))

(defun draw-object (showcase &key (nshape 0))
  (when-let ((shapes (shapes showcase)))
    (let* ((nshapes (length shapes))
           (nshape (mod nshape nshapes))
           (shape (nth nshape shapes)))
      (setf (current-shape showcase) nshape)
      (draw-shape (slot-value showcase 'animator) shape)
      (when (eq (class-of shape) (find-class 'walk-shape))
        (q+:qtimer-single-shot *walk-speed*
                               showcase (qslot "animateComplex()"))))))

(defun only-avatar-shapes (&optional gender)
  (lambda (shape)
    (let ((shape-type (shape-type shape)))
      (case (first shape-type)
        (:avatar (eq (second shape-type) :avatar))
        (:gendered-avatar
         (and (eq (second shape-type) :avatar)
              (if gender (eq (third shape-type) gender) t)))))))

(defclass walk-shape (shape)
  ((walk-left :reader walk-left
              :initarg :walk-left
              :initform nil)
   (walk-right :reader walk-right
               :initarg :walk-right
               :initform nil)
   (walk-parent :reader walk-parent
                :initarg :walk-parent
                :initform nil)
   (left-now-p :accessor left-now-p
               :initform t)))

(defparameter *walk-speed* 200
  "Walking speed in milliseconds.")

(define-slot (showcase animate-complex) ()
  (with-slots (shape) animator
    (when (eq (class-of shape) (find-class 'walk-shape))
      (reset-animator showcase)
      (cond ((walk-parent shape)
             (setf (left-now-p (walk-parent shape))
                   (not (left-now-p (walk-parent shape))))
             (draw-shape animator (walk-parent shape)))
            ((left-now-p shape)
             (draw-shape animator (walk-left shape)))
            (t
             (draw-shape animator (walk-right shape))))
      (q+:qtimer-single-shot *walk-speed* showcase (qslot "animateComplex()")))))

(defun object-complex-avatar-shapes (object &optional gender)
  "Returns a fresh list containing complex avatar shapes."
  (%object-complex-avatar-shapes object (edit-type object) gender))

(defgeneric %object-complex-avatar-shapes (object edit-type gender))

;; TODO test for non-gendered avatars
(defmethod %object-complex-avatar-shapes
    (object (edit-type (eql :avatar)) gender)
  (declare (ignore gender))
  (loop with shapes = (remove-if-not (only-avatar-shapes)
                                     (children object))
        for shape in shapes
        for (x y direction size pose) = (shape-type shape)
        if (and (eq pose :walk) (eq (class-of shape) (find-class 'shape)))
          collect (let* ((fn (lambda (shape)
                               (let ((shape-type (shape-type shape)))
                                 (and (eq (third shape-type) direction)
                                      (eq (fourth shape-type) size)
                                      (fifth shape-type)))))
                         (right (find :walk-left shapes :key fn))
                         (left (find :walk-right shapes :key fn)))
                    (change-class right 'walk-shape :walk-parent shape)
                    (change-class left 'walk-shape :walk-parent shape)
                    (change-class shape 'walk-shape :walk-left left
                                                    :walk-right right))
        else if (member pose '(:walk-left :walk-right))
               do (progn)
        else collect shape))

(defmethod %object-complex-avatar-shapes
    (object (edit-type (eql :gendered-avatar)) gender)
  (loop with shapes = (remove-if-not (only-avatar-shapes gender)
                                     (children object))
        for shape in shapes
        for (x y gender direction size pose) = (shape-type shape)
        if (and (eq pose :walk) (eq (class-of shape) (find-class 'shape)))
          collect (let* ((fn (lambda (shape)
                               (let ((shape-type (shape-type shape)))
                                 (and (eq (third shape-type) gender)
                                      (eq (fourth shape-type) direction)
                                      (eq (fifth shape-type) size)
                                      (sixth shape-type)))))
                         (right (find :walk-left shapes :key fn))
                         (left (find :walk-right shapes :key fn)))
                    (change-class right 'walk-shape :walk-parent shape)
                    (change-class left 'walk-shape :walk-parent shape)
                    (change-class shape 'walk-shape
                                  :walk-left left :walk-right right))
        else if (member pose '(:walk-left :walk-right))
               do (progn)
        else collect shape))

#|
(with-main-window (layout (make-instance 'qui:flow-layout))
  (dolist (object (children (parent *object*)))
    (let ((showcase (make-instance 'showcase :object object
                                             :color-code "w&#L&@(=(='###$#")))
      (qui:add-widget showcase layout)
      (draw-object showcase object))))

(with-main-window (showcase (make-instance 'showcase
                                           :object *object*
                                           :color-code "w&#L&@(=(='###$#"
                                           :predicate (only-avatar-shapes)
                                           :complicate-shapes-p t
                                           :gender :male)))
|#
