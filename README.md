# Emacs-Project-manager
A tiny project manager for Emacs , just to switch projects and add/remove buffers from project.


-   Put `(load-file "~/.emacs.d/start.el")` into `.emacs`

-   Extract all files to `~/.emacs.d/`

-  `(setf *root* "/PATH/TO/ROOT/Folder")
   (load-projects *root*)
   (switch-project (read-from-minibuffer "Select project: "))`   into `start.el`
