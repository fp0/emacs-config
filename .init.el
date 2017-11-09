;; OS specific settings
(cond
  ((string-equal system-type "windows-nt")
    (progn
      (message "Microsoft Windows")

      ;; env PATH
      (defun set-exec-path-from-shell-PATH ()
        (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
	  (setenv "PATH" path-from-shell)
	  (setq exec-path (split-string path-from-shell path-separator))))

      ;; spell checking
      ;;(custom-set-variables
      ;; '(ispell-dictionary "english")
      ;; '(ispell-program-name "C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe"))))

      ;; ditaa org mode ascii diagrams
      (setq org-ditaa-jar-path "d:/bin/ditaa0_9.jar")
   )
 )
 ((string-equal system-type "darwin")
   (progn
     (message "Mac OS X")
    
     ;; setup mac environment variables
     (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
     (setq exec-path (append exec-path '("/usr/local/bin")))))

 ((string-equal system-type "gnu/linux")
   (progn
     (message "Linux"))))

;; remove toolbar
;;(tool-bar-mode -1)

;; packages

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)


;; org mode setup
(require 'org-install)
(require 'ox-md)
(require 'ox-odt)


(org-babel-do-load-languages 'org-babel-load-languages '( 
        (ditaa . t)))

;; don't show validate link in html export footer
(setq org-html-validation-link nil)

(setq org-src-fontify-natively t)

;; line wrap
(setq line-move-visual t)

;; always have spellchecking on
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; visual word wrap
(global-visual-line-mode 1)

;; theme
(load-theme 'twilight t)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))
