;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FOX5
;;;; © Michał "phoe" Herda 2017
;;;; infer.lisp

(in-package :fox5)

(defvar *inferrers* (make-hash-table))

(defparameter *specializations*
  '(:portrait :player :avatar :wall :floor :effect :item))

(defmacro define-inferer
    (name (&optional (file (gensym)) (specializations (gensym))) &body body)
  `(setf (gethash ,name *inferrers*)
         (lambda (,file ,specializations)
           (block nil
             (flet ((decline () (return (remove ',name ,specializations)))
                    (pass () (return ,specializations))
                    (accept () (return (list ',name))))
               (declare (ignorable #'pass #'decline #'accept))
               ,@body)))))

(defun infer-specialization
    (file &optional
            (inferrers *inferrers*)
            (initial-specializations *specializations*))
  "Provided a file and, optionally, a list of initial specializations, returns a
list of all specializations from the initial list that still match the file
after"
  (loop for inferrer in (hash-table-values inferrers)
        for specializations = initial-specializations
        do (setf specializations (funcall inferrer file specializations))
        when (= 1 (length specializations))
          return specializations
        finally (return specializations)))

;; (define-inferer :portrait (file)
;;   (when (/= 0 (mod (length (children file)) 3))
;;     (decline)) ;; TODO this won't work for more than three ports
;;   (dolist (object (children file))
;;     (dolist (shape (children object))
;;       (dolist (frame (children shape))
;;         (dolist (sprite (children frame))
;;           (let ((image (image sprite)))
;;             (unless (= 95 (width image) (height image))
;;               (decline)))))))
;;   (accept))

;; (define-inferer :wall (file)
;;   (when (oddp (length (children file)))
;;     (decline))
;;   (pass))
