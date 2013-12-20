;; Make the font big
(set-face-attribute 'default nil :height 240)

;; Turn off toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

(when load-in-progress
  (setq mp-config-directory (file-name-directory load-file-name)
        mp-thirdparty-directory (concat mp-config-directory "thirdparty/")))

;; Add everything in this directory to the load-path
(let* ((default-directory mp-config-directory)
       (orig-load-path load-path))
  (setq load-path (cons default-directory nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(setq
 ;; Turn off startup message
 inhibit-startup-message t

 ;; Use 4-wide tabs
 tab-width 4

 ;; Always show line and column numbers in mode-line
 line-number-mode t
 column-number-mode t

 ;; Setup backups
 backup-directory-alist `(("." . "~/.emacs.d/saves"))
 backup-by-copying t

 ;; Make scrolling not suck.
 scroll-margin 0
 scroll-conservatively 100000
 scroll-up-aggressively 0.0
 scroll-down-aggressively 0.0)

;; Setup emacsclient
(server-start)
(setenv "EDITOR" "emacsclient")

;; Highlight matching parens
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(set-face-foreground 'show-paren-match "Orange")
(set-face-background 'show-paren-match nil)
(set-face-bold-p 'show-paren-match t)

;; Use "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Prevent annoying "Active processes exist" query when Emacs is quit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-flet ((process-list ())) ad-do-it))

(add-to-list 'custom-theme-load-path (concat mp-thirdparty-directory "zenburn-emacs"))
(load-theme 'zenburn t)

;; Save the session
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
