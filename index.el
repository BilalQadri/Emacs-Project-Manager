

(defvar *root* nil)              ;; root folder
(defvar *projects* nil)        ;; projects list
(defvar *adhoc-list* nil)
(defvar *buffers* nil)
(defvar *current-project* nil)


(defun set-root (path)
  (if (directory? path)
      (progn (setf *root* path)
	     (load-projects *root*))
    (message "directory  does not exist.")))

;(eval-file (filename-as-path (project-dir) ".project"))

(defun eval-file (file)
  "Execute FILE and return the result of the last expression."

      (with-temp-buffer
        (insert-file-contents file)
        (read (buffer-string))))

(defun load-projects (root)
  (setf *projects* (get-dirs root)))

(defun current-buffer-name ()
   (buffer-name (current-buffer)))

(defun buffer-path ()
  (buffer-file-name (current-buffer)))




(defun add-buffer ()
  (interactive)
  
  (when (null *current-project*)                   ;; if no current-project selected
    (error "Please, select project to proceed."))

  
  (let ((init-file (filename-as-path (project-dir) ".project")))
    
    (when  (not (null (file? init-file)))                        ;; if init file, eval content and delete it
      (setf *buffers* (eval-file init-file))
      (delete-file init-file))

    (cond ((not (special-buffer (current-buffer-name)))
	   (when (not (member (buffer-path) *buffers*)) 
	     (push (buffer-path) *buffers*)))               ;; push buffer's path to buffers list
	  (t
	   (message "It is a special buffer.")))
    
    
    (with-temp-file init-file                ;; writing buffers list to init file
     ; (insert-file-contents init-file)
      
      (insert (pp *buffers*)))))

(defun remove-buffer ()
  (interactive)
  (when (null *current-project*)                   ;; if no current-project selected
    (error "Please, select project to proceed."))

  
  (let ((init-file (filename-as-path (project-dir) ".project")))
    
    (when (not (null (file? init-file)))                        ;; if init file, eval content and delete it
      (setf *buffers* (eval-file init-file))
      (delete-file init-file))

    (delete (buffer-path) *buffers*)
    
    
    (with-temp-file init-file                ;; writing buffers list to init file
      ;(insert-file-contents init-file)
      
      (insert (pp *buffers*)))))


(defun switch-project (project)
  (interactive "sProject: ")
  (if (member project *projects*)
      (progn
	(setf *current-project* project)
	(kill-open-buffers)
	(cond ((not (null (file? (filename-as-path (project-dir) ".project"))))
	       (setf *buffers* (eval-file (filename-as-path (project-dir) ".project")))
	       (open-project-buffers *buffers*))
	      (t
	       (setf *buffers* '()))))
    ;;	(compile-project))
    (message (format "Sorry, Project \'%s\'  does not exist or may be Root is not set accordingly." project))))

(defun s-right (idx str)
  (let ((len (length str)))    
    (substring str (* idx -1) len)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun compile-project ()										        ;;
;;   (let ((str0 (concat "(setf clws:*clws-package-path* \"" (filename-as-path (project-dir) "") "\")"))        ;;
;; 	(str1 (concat "(load (compile-file \"" (filename-as-path (project-dir) "init.lisp") "\"))")))	        ;;
;;     (cadr (slime-eval `(swank:eval-and-grab-output ,str0)))						        ;;
;;     (cadr (slime-eval `(swank:eval-and-grab-output ,str1)))))					        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-project-buffers (buffers-list)
  (first-nil buffers-list
	     (progn (if (and (not (equal (s-right 3 first) "elc")) (not (equal (s-right 4 first) "fasl")))
			(find-file-noselect first))
		    (open-project-buffers (cdr buffers-list)))))

(defun open-files (files-list)
  (first-nil files-list
	     (progn
	       (find-file-noselect first)
	       (open-files (cdr files-list)))))

(defun s-trim (str)
  (require 'subr-x)
  (string-trim-right (string-trim-left str)))



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



