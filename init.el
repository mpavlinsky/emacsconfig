;; Set up directories?
(when load-in-progress
  (setq mp-config-directory (file-name-directory load-file-name)
        mp-thirdparty-directory (concat mp-config-directory "thirdparty/")))

;; Add everything in this directory to the load-path
(let* ((default-directory mp-config-directory)
       (orig-load-path load-path))
  (setq load-path (cons default-directory nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

;; Zenburn!
(add-to-list 'custom-theme-load-path (concat mp-thirdparty-directory "zenburn-emacs"))
(load-theme 'zenburn t)

;; Make the font big
(set-face-attribute 'default nil :font "Consolas" :height 180)

;; Turn off toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

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

;; Save the session
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)

;;Multi-termq
(require 'multi-term)
(setq multi-term-program "/bin/bash")

(defun mp-multi-term-dedicated-toggle ()
  "Toggle the multi-term dedicated window. Then, if the window
was created, select it. If the window was dismissed, kill the
multi-term dedicated buffer without prompting."
  (interactive)
  (multi-term-dedicated-toggle)
  (if (multi-term-dedicated-exist-p)
      (multi-term-dedicated-select)
    (set-process-query-on-exit-flag (get-buffer-process multi-term-dedicated-buffer) nil)
    (kill-buffer multi-term-dedicated-buffer)))

(global-set-key (kbd "s-t") 'mp-multi-term-dedicated-toggle)

;; magit
(require 'magit)
(require 'magit-blame)

(global-set-key (kbd "C-x g") 'magit-status)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; experimental
(load "sandbox.el")
