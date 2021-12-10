;; Unmerged stuff for automatic license modification and format detection.

(defun subdirectories (pathname)
  (uiop:while-collecting (collect)
    (uiop:collect-sub*directories
     pathname (constantly t) (constantly t) #'collect)))

(defun directory-all-files (pathname &optional (types '("fox" "fsh" "fs2")))
  (let* ((directories (subdirectories pathname))
         (files (alexandria:mappend #'uiop:directory-files directories)))
    (flet ((filter (file) (member (pathname-type file) types
                                  :test #'string-equal)))
      (remove-if-not #'filter files))))

(defun fox5-read-error-p (condition)
  (string= (simple-condition-format-control condition) "Not a FOX5 file: ~A"))

(defun read-file (pathname)
  (alexandria:eswitch ((pathname-type pathname) :test #'string-equal)
    ("fsh" (fox5:read-fsh pathname))
    ("fs2" (fox5:read-fs2 pathname))
    ("fox"
     (handler-case (fox5:read-fox5 pathname)
       ((satisfies fox5-read-error-p) () (fox5:read-fox1 pathname))))))

(defun modify-object (object)
  (setf (fox5:license object) '(:reserved 101))
  (pushnew "Dragon's Eye Productions/Catnip Studios" (fox5:authors object)
           :test #'string=)
  object)

(defun process ()
  (let ((pathnames (directory-all-files #p"~/Downloads/DEP_Derivatives/")))
    (dolist (pathname pathnames)
      (let ((file (read-file pathname)))
        (mapc #'modify-object (fox5:children file))
        (fox5:write-fox5 file pathname)))))
