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

;;Multi-term
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

;; yank in multi-term
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "s-v") 'term-paste)))



;; magit
(require 'magit)
(require 'magit-blame)

;; Magit
(setq magit-process-popup-time 5)
(set-face-attribute 'magit-item-highlight nil :inherit nil :background nil)
;; "q" always kills magit buffers
(define-key magit-mode-map "q" (lambda () (interactive) (magit-mode-quit-window 'kill-buffer)))
(define-key magit-mode-map ";" 'magit-toggle-section)
;; Use j and k for navigation in magit-mode.
;; Remap "k" to be magit-goto-previous-section everywhere
(define-key magit-status-mode-map "k" 'magit-goto-previous-section)
(define-key magit-branch-manager-mode-map "k" 'magit-goto-previous-section)
(define-key magit-mode-map "k" 'magit-goto-previous-section)
;; Remap "K" to do what "k" used to do, wherever "k" used to be defined
(define-key magit-status-mode-map "K" 'magit-discard-item)
(define-key magit-branch-manager-mode-map "K" 'magit-discard-item)
;; Map "j" to magit-goto-next-section in eveywhere
(defun gcs-magit-j ()
  (interactive)
  (let ((next (magit-find-section-after (point))))
    (if next
        (magit-goto-section next)
      (goto-char (+ -1 (magit-section-end (magit-current-section)))))))
(define-key magit-status-mode-map "j" 'gcs-magit-j)
(define-key magit-mode-map "j" 'gcs-magit-j)

(define-key git-rebase-mode-map "j" 'forward-line)
(define-key git-rebase-mode-map "k" 'git-rebase-backward-line)
(define-key git-rebase-mode-map "p" 'git-rebase-pick)
(define-key git-rebase-mode-map "K" 'git-rebase-kill-line)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;
;;;; elisp commenting
;;(defun mp-comment-or-uncomment-region (start end)
;;  (interactive "r")
;;  ; Fix the start and end if we aren't actually in a region (single line).
;;  (if (not (region-active-p))
;;	  (setq start (point)
;;			end (point)))
;;  (goto-char start)
;;  (save-excursion
;;	(move-beginning-of-line nil)
;;	(setq non-comment-found nil)
;;	(loop while (and (<= (point) end) (not non-comment-found)) do
;;		  (if (not (looking-at "^\s*;"))
;;			  (setq non-comment-found t))
;;		  (move-beginning-of-line 2))
;;	; Now that we know what to do add the semicolons or remove them.
;;	(goto-char start)
;;	(move-beginning-of-line nil)
;;	(loop while (<= (point) end) do
;;		  (if non-comment-found
;;			  (insert ";;")
;;			(if (re-search-forward "^\\(\s*\\);+" (line-end-position))
;;				(replace-match "\\1" nil nil)))
;;		  (move-beginning-of-line 2))))

(defun comment-dwim-line-or-toggle-term-mode (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line.
   Also, toggles between term-line-mode and term-char-mode in multi-term"
  (interactive "*P")
  (if (equal 'term-mode major-mode)
      (if (term-in-line-mode)
          (progn (term-char-mode) (message "CHAR MODE"))
        (term-line-mode) (message "LINE MODE"))
    
    (comment-normalize-vars)
    (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (comment-dwim arg))))

(global-set-key (kbd "C-;") 'comment-dwim-line-or-toggle-term-mode)



;; Reinitialize...
(defun mp-reinit ()
  (interactive)
  (let* ((buffer-name "~/.emacs.d/init.el")
		 (init-buffer (get-file-buffer buffer-name)))
	(message (buffer-name init-buffer))
	(save-buffer init-buffer)
	(load-file buffer-name)))


;; evil mode
(require 'evil)
(evil-mode 1)

;; Use tab to move between links in help mode.
(evil-define-key 'motion help-mode-map (read-kbd-macro "TAB") 'forward-button)

;; Make cursor red in Emacs mode.
(setq evil-emacs-state-cursor '("red" box)
      evil-cross-lines t)

(mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
       '(inferior-emacs-lisp-mode
         comint-mode
         shell-mode
         term-mode
         magit-branch-manager-mode
		 git-rebase-mode
         pianobar-mode))

;; Use css-mode for SASS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; Ediff
(set-face-attribute 'ediff-current-diff-A nil :background "#553333" :foreground nil)
(set-face-attribute 'ediff-current-diff-B nil :background "#335533" :foreground nil)
(set-face-attribute 'ediff-current-diff-C nil :background "#888800" :foreground nil)
(set-face-attribute 'ediff-fine-diff-A    nil :background "#331111" :foreground nil)
(set-face-attribute 'ediff-fine-diff-B    nil :background "#113311" :foreground nil)
(set-face-attribute 'ediff-fine-diff-C    nil :background "#666600" :foreground nil)
(set-face-attribute 'ediff-even-diff-A    nil :background "Grey15"  :foreground nil)
(set-face-attribute 'ediff-even-diff-B    nil :background "Grey15"  :foreground nil)
(set-face-attribute 'ediff-even-diff-C    nil :background "Grey15"  :foreground nil)
(set-face-attribute 'ediff-odd-diff-A     nil :background "Grey10"  :foreground nil)
(set-face-attribute 'ediff-odd-diff-B     nil :background "Grey10"  :foreground nil)
(set-face-attribute 'ediff-odd-diff-C     nil :background "Grey10"  :foreground nil)

(defun generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(defun mp-kill-buffer-command ()
  (interactive)
  (kill-buffer (current-buffer))
  (let ((buffer-menu-buffer (get-buffer "*Ibuffer*")))
    (when buffer-menu-buffer
      (with-current-buffer buffer-menu-buffer
        (ibuffer-update nil)))))

(defun mp-set-font-size (&optional size)
  (interactive (list (if (not current-prefix-arg)
						 (read-number "Font size: ")
                       nil)))
  (set-face-attribute 'default nil :font "Consolas" :height (*  size 10)))

(require 'keybindings)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-ignore-buffers (cons "\\*Buffer List\\*" ido-ignore-buffers)
      ;; Show ido completions vertically
      ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
                              " [No match]" " [Matched]" " [Not readable]"
                              " [Too big]" " [Confirm]")))
(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            ;; Disable line truncation
            (set (make-local-variable 'truncate-lines) nil)
            ;; Delete backward by word with C-w
            (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
            (define-key ido-completion-map (kbd "s-j") 'ido-next-match)
            (define-key ido-completion-map (kbd "s-k") 'ido-prev-match)))


;; Ido
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-ignore-buffers (cons "\\*Buffer List\\*" ido-ignore-buffers)
      ;; Show ido completions vertically
      ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
                              " [No match]" " [Matched]" " [Not readable]"
                              " [Too big]" " [Confirm]")))
(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            ;; Disable line truncation
            (set (make-local-variable 'truncate-lines) nil)
            ;; Delete backward by word with C-w
            (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
            (define-key ido-completion-map (kbd "s-j") 'ido-next-match)
            (define-key ido-completion-map (kbd "s-k") 'ido-prev-match)))

;; Ido
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-ignore-buffers (cons "\\*Buffer List\\*" ido-ignore-buffers)
      ;; Show ido completions vertically
      ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
                              " [No match]" " [Matched]" " [Not readable]"
                              " [Too big]" " [Confirm]")))
(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            ;; Disable line truncation
            (set (make-local-variable 'truncate-lines) nil)
            ;; Delete backward by word with C-w
            (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
            (define-key ido-completion-map (kbd "s-j") 'ido-next-match)
            (define-key ido-completion-map (kbd "s-k") 'ido-prev-match)))

;; experimental
(load "sandbox.el")

(mp-set-font-size 22)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
