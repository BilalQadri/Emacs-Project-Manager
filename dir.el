(defvar *deff* 3)



(defmacro first-of (items &rest body)
  `(let ((first (car ,items)))
     ,@body))

(defmacro first-nil (items-list &rest body)
  `(first-of ,items-list
	     (if (null first)
		 nil
	       ,@body)))

(defun directory? (dir)
  (file-directory-p dir))

(defun file? (file)
  (file-regular-p file))

(defun get-dir-name (dir)
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory (file-name-as-directory dir)))))


(defun clean-directory (contents)
  (first-of contents
	    (if (null first)
		nil
	      (if (or (equal first ".") (equal first "..") (backup-file-name-p first))
		  (clean-directory (cdr contents))
		(cons first (clean-directory (cdr contents)))))))

(defun get-contents (path )
  (clean-directory (directory-files path)))

(defun get-contents-path (path)
  (mapcar #'(lambda (item)
	      (concat (file-name-as-directory path)
		      item))
	  (get-contents path)))

(defun filename-as-path (path name)
  (concat (file-name-as-directory path)
	  name))

(defun get-dirs (path)
  (filter-dir (get-contents path) path))

(defun filter-dir (contents path)
  (first-of contents
	    (if (null first)
		nil
	      (if (directory? (filename-as-path path first))
		  (cons first (filter-dir (cdr contents) path))
		(filter-dir (cdr contents) path)))))
		    
(defun get-files (path)
  (filter-file (get-contents path) path))		   

(defun filter-file (contents path)
  (first-of contents
	    (if (null first)
		nil
	      (if (file? (filename-as-path path first))
		  (cons first (filter-file (cdr contents) path))
		(filter-file (cdr contents) path)))))

(defun get-name (path)
  (if (directory? path)
      (get-dir-name path)
    (file-name-nondirectory path)))


