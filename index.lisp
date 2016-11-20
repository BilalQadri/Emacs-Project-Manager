(defun load-init (path)
  (setf clws:*clws-package-path* path)
  (load (merge-pathnames "init.lisp" path)))
