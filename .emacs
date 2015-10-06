;; OS specific settings
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
     (message "Microsoft Windows")

    ;; set lisp system
     (setq inferior-lisp-program "C:/sbcl/1.2.7/sbcl.exe")

    ;; env PATH
    (defun set-exec-path-from-shell-PATH ()
      (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
	(setenv "PATH" path-from-shell)
	(setq exec-path (split-string path-from-shell path-separator))))

    ;; spell checking
    (custom-set-variables
     '(ispell-dictionary "english")
     '(ispell-program-name "C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe"))))
 
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (message "Mac OS X")
    
    ;; setup mac environment variables
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))
    
    ;; use MIT scheme
    (setq scheme-program-name
	  "/Applications/MIT:GNUScheme.app/Contents/Resources/mit-scheme")
    (require 'xscheme)
    
    ;; set lisp system
    (setq inferior-lisp-program "sbcl")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux")

    ;; set lisp system
    (setq inferior-lisp-program "/usr/local/bin/sbcl"))))

;; remove toolbar
(tool-bar-mode -1)

;; packages
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)


;; org mode setup
(require 'org-install)
(require 'ox-md)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . true) (python . true) (lisp . true) (scheme . true) (ditaa . true))
)

;; slime
(setq slime-contribs '(slime-fancy))

;; theme
(load-theme 'twilight t)

;; hippie expand - provides a variety of completions and expansions
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))


