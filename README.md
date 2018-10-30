# Emacs-Project-manager
A (tiny) project manager for Emacs editor to make work flow better and intuitive.

-   Extract files into to initialization folder i.e `.emacs.d`.
-   Put `(load-file "~/.emacs.d/start.el")` into init file `.emacs` ... thats it

-   Extract all files to `~/.emacs.d/`

# Work-Flow

- Project manage will ask for Root folder where your projects resides and a project name(name of your projects's directory)
- Files can be added to workspace(current project) with `M-x add-buffer`.. Current buffer will be added to current project.       Manager will record these files and open them for you when you will load this project next time.
- Root can be set with `M-x set-root` and project will be changed with `M-x switch-project`.
- Currently there is no facility for creating project, but you add new project in Root directory then you will have to `M-x       refresh-root` 



# API 

- add-buffer
- remove-buffer
- set-root
- switch-project
- refresh-root
