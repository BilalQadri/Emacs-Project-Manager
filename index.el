

(defvar *root* nil)              ;; root folder
(defvar *projects* nil)        ;; projects list
(defvar *adhoc-list* nil)

(defvar *current-project* nil)


(defun set-root (path)
  (if (directory? path)
      (progn (setf *root* path)
	     (load-projects *root*))
    (message "directory  does not exist.")))


(defun load-projects (root)
  (setf *projects* (get-dirs root)))


(defun switch-project (project)
  (interactive "sProject: ")
  (if (member project *projects*)
      (progn
	(setf *current-project* project)
	(kill-open-buffers)
	(open-project-buffers project)
	(compile-project))
    (message "Sorry, Project does not exist")))

(defun compile-project ()
  (let ((str0 (concat "(setf clws:*clws-package-path* \"" (filename-as-path (project-dir) "") "\")"))
	(str1 (concat "(load (compile-file \"" (filename-as-path (project-dir) "init.lisp") "\"))")))
    (cadr (slime-eval `(swank:eval-and-grab-output ,str0)))
    (cadr (slime-eval `(swank:eval-and-grab-output ,str1)))))

(defun open-project-buffers (project)
    (open-buffers (get-files (project-dir))))

(defun open-buffers (buffers-list)
  (first-nil buffers-list
	     (progn (if (and (not (equal (s-right 3 first) "elc")) (not (equal (s-right 4 first) "fasl")))
			(find-file-noselect (filename-as-path (project-dir) first)))
		    (open-buffers (cdr buffers-list)))))

(defun open-files (files-list)
  (first-nil files-list
	     (progn
	       (find-file-noselect first)
	       (open-files (cdr files-list)))))


(defun get-buffers ()
  (filter-buffers (buffer-list)))

(defun filter-buffers (buffer-list)
  (first-nil buffer-list
	     (if (special-buffer (s-trim (buffer-name first)))
		 (filter-buffers (cdr buffer-list))
	       (cons first (filter-buffers (cdr buffer-list))))))

(defun kill-open-buffers ()
  (buffers-kill (get-buffers)))

(defun buffers-kill (list-of-buffers)
  (first-nil list-of-buffers
	     (progn (kill-buffer first)
		    (buffers-kill (cdr list-of-buffers)))))

(defun special-buffer (buffer-name)
  (if (and (equal (substring buffer-name 0 1) "*") (equal (substring buffer-name -1) "*"))
      t))



(defun set-adhoc ()
  (interactive)
  (setf *adhoc-list* (buffers-paths (get-buffers)))) 

(defun get-adhoc ()
  (interactive)
  (kill-open-buffers)
  (open-files *adhoc-list*))

(defun buffers-paths (buffer-list)
  (first-nil buffer-list
	     (cons (buffer-file-name first) (buffers-paths (cdr buffer-list)))))


(defun project-dir ()
  (filename-as-path *root* *current-project*))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun new-project (type name)					   ;;
;;   "sequence of making new project"					   ;;
;;   (interactive (list							   ;;
;; 		(read-string "Type: ")					   ;;
;; 		(read-string "Name: ")))				   ;;
;;   (let ((type (intern type)) (name (intern name)))			   ;;
;;     (push (cons name '()) (cassoc type *projects*)))			   ;;
;;     (mk-dir (concat type "/" name))					   ;;
;;   									   ;;
;;   (message "Project name  is \"%s\" of \"%s\" type." name type))	   ;;
;;   									   ;;
;; 									   ;;
;; 									   ;;
;; (defun move-project-to-type ())					   ;;
;; 									   ;;
;; 									   ;;
;; (defun add-type (type)						   ;;
;;   "add type of project"						   ;;
;;   									   ;;
;;   (interactive "sType: ")						   ;;
;;   									   ;;
;;   (let ((type (intern type)))					   ;;
;;     (push type *types*)						   ;;
;;     (push (cons type '()) *projects*))				   ;;
;;   									   ;;
;;     (mk-dir type))							   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defmacro cassoc (key value)
  "cassoc macro"
  `(cdr (assoc ,key ,value)))



