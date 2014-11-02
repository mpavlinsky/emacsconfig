(defun map2 (function list)
  (case (length list)
    (0 list)
    (1 (error "map2 got an odd-length list"))
    (t (cons (funcall function (first list) (second list))
             (map2 function (cddr list))))))

(defmacro global-set-keys (&rest bindings)
  `(progn ,@(map2 (lambda (key command)
                    (if (listp command)
                        `(global-set-key (kbd ,key) (lambda () (interactive) ,command))
                      `(global-set-key (kbd ,key) ',command)))
                  bindings)))
 
(global-set-keys
 ;; VIM style search in Emacs Mode
 ;; "/" evil-search-forward
 ;; "n" evil-search-next
 ;; "N" evil-search-previous
 
 ;; Replace normal m-x with smex
 "M-x" smex
 "M-X" smex-major-mode-commands

 ;; "M-SPC" mp-change-around-paren
 
 ;; "s-f" ns-popup-font-panel

 ;; Use cmd-r to compile
 ;; "s-r" mp-compile

 ;; Use ibuffer instead of list-buffers
 ;; "\C-x\C-b" mp-gibuffer

 ;; Use C-w for backward-kill-word in the minibuffer
 "C-w" backward-kill-word

 ;; Use C-s-f to toggle fullscreen
 "C-s-f" ns-toggle-fullscreen

 ;; Use [C-]s-[y, u, i, o] to resize windows
 ;; "s-y"   (shrink-window-horizontally 5)
 ;; "s-u"   (shrink-window 5)
 ;; "s-i"   (enlarge-window 5)
 ;; "s-o"   (enlarge-window-horizontally 5)

 ;; Xcode-like keybindings
 ;; "s-O" mp-find-file-dwim
 ;; "C-s-<up>" ff-find-other-file

 ;; Use s-[h, j, k, l] for window navigation
 "s-h" windmove-left
 "s-l" windmove-right
 "s-k" windmove-up
 "s-j" windmove-down

 ;; Also use C-[arrow keys] for window navigation. Useful in terminal emacs.
 "C-<left>" windmove-left
 "C-<right>" windmove-right
 "C-<up>" windmove-up
 "C-<down>" windmove-down

 ;; Use s-[H, J, K, L] to swap windows
 ;; "s-H" (mp-put-buffer-in-window 'left)
 ;; "s-J" (mp-put-buffer-in-window 'down)
 ;; "s-K" (mp-put-buffer-in-window 'up)
 ;; "s-L" (mp-put-buffer-in-window 'right)

 ;; Make C-M-g the same as C-g - in case 'Esc' is pressed accidentally
 "\C-\M-g" keyboard-quit

 ;; Use C-u to scoll up like vim, move emacs's universal argument to C-S-u
 "C-u" evil-scroll-up
 "C-S-u" universal-argument

 ;; Tab navigation
 "M-j" tabbar-backward-tab
 "M-k" tabbar-forward-tab

 ;; Revert buffer with no confirmation
 "M-s-R" (revert-buffer t t)
 "C-S-t" generate-buffer)




;; Use j and k pressed within .15 seconds to exit insert mode
(defun mp-evil-maybe-exit (entry-key exit-key)
  (let ((modified (buffer-modified-p)))
    (insert entry-key)
    (let ((evt (read-event nil nil 0.15)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt exit-key))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t (push evt unread-command-events))))))

(evil-define-command mp-evil-maybe-exit-j ()
  :repeat change
  (interactive)
  (mp-evil-maybe-exit ?j ?k))
(define-key evil-insert-state-map "j" 'mp-evil-maybe-exit-j)

(evil-define-command mp-evil-maybe-exit-k ()
  :repeat change
  (interactive)
  (mp-evil-maybe-exit ?k ?j))
(define-key evil-insert-state-map "k" 'mp-evil-maybe-exit-k)


(defconst mp-prefix-key-commands
  (mapcar
   (lambda (binding) (list (read-kbd-macro (first binding)) (second binding)))
   '(("q"   quit-window)
     ("k"   mp-kill-buffer-command)
     ("s-k" delete-window)
     ("K"   mp-kill-buffer-and-window)
     ("g"   magit-status)
     ("l"   magit-file-log)
     ("u"   undo-tree-visualize)
     ("x"   smex)
     ("X"   smex-major-mode-commands)
     ("m"   multi-term)
     ("o"   projectile-find-file) 
     ;; ("d"   mp-find-file-dwim)
     ("f"   ido-find-file)
     ("F"   ido-find-alternate-file)
     ;; ("s-f" mp-show-in-finder)
     ;; ("s-x" mp-open-with-external-editor)
     ("a"   ag-project)
     
     ("w" save-buffer)
     ("W" write-file)
     ("b" ibuffer)
     ("v" ido-switch-buffer)
     ("V" ido-switch-buffer-other-frame)

     ("s-v" visual-line-mode)
     ("s-b" magit-blame-mode)

     ("r" eval-buffer)
    
     ("0" delete-window)
     ("7" delete-window)
     ("1" delete-other-windows)
     ("2" split-window-vertically)
     ("3" split-window-horizontally)
     ("4" balance-windows)

     ;; ("<left>"  mp-previous-buffer)
     ;; ("<right>" mp-next-buffer)
     ("\\"      comment-dwim-line-or-toggle-term-mode))))

(defun mp-prefix-key-command ()
  (interactive)
  (let* ((old-cursor-color (prog1 (face-background 'cursor) (set-cursor-color "Green")))
         (old-overriding-local-map overriding-local-map)
         (overriding-local-map (make-sparse-keymap))
         (key (read-key-sequence nil)))
    (setq overriding-local-map old-overriding-local-map)
    (set-cursor-color old-cursor-color)
    (call-interactively (second (assoc key mp-prefix-key-commands)))))

(defconst mp-prefix-key "\\")
(defconst mp-prefix-key-maps (list evil-normal-state-map
                                    evil-motion-state-map
                                    evil-emacs-state-map))
(mapc (lambda (keymap)
        (define-key keymap mp-prefix-key 'mp-prefix-key-command)
        (define-key keymap (read-kbd-macro (concat "s-" mp-prefix-key)) 'mp-prefix-key-command))
      mp-prefix-key-maps)

(defun mp-zz ()
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(define-key evil-normal-state-map "ZZ" 'mp-zz)

;;;;; KEY-CHORD KEYBINDINGS ;;;;;
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)

;; Any prefix key, "\x" can also be triggered with the key chord "jx"
(mapc (lambda (prefix-command)
        (let* ((key-string (first prefix-command))
               (key (aref key-string 0)))
          (when (and (numberp key) (<= key 126) (>= key 32)
                     (not (equal key-string "j"))
                     (not (equal key-string "k")))
            (key-chord-define-global (vector (aref "j" 0) key) (second prefix-command)))))
      mp-prefix-key-commands)

;; (key-chord-define-global "jl" 'mp-helm-dwim)

;; Numbers for window splitting
(key-chord-define-global "89" 'split-window-vertically)
(key-chord-define-global "78" 'split-window-horizontally)

;; First fingers column
(key-chord-define evil-normal-state-map "jk" 'keyboard-quit)
(key-chord-define minibuffer-local-map "jk" 'abort-recursive-edit)
(key-chord-define ibuffer-mode-map "jk" 'ibuffer-quit)
(key-chord-define-global "m," 'smex)

;; K + o or . for killing buffer or window
(key-chord-define-global "k." 'delete-window)
(key-chord-define-global "ko" 'mp-kill-buffer-command)

;; H-chords for help
(key-chord-define-global "hf" 'describe-function)
(key-chord-define-global "hv" 'describe-variable)
(key-chord-define-global "hk" 'describe-key)

;; K + u or m for moving by half-screen
;; (key-chord-define-global "ku" 'mp-smooth-scroll-up-half-screen)
;; (key-chord-define-global "km" 'mp-smooth-scroll-down-half-screen)

(key-chord-define-global "kg" 'evil-goto-line)
(key-chord-define-global "kv" 'evil-visual-line)

;; Semicolon chords for evaluation
(defun mp-eval-dwim ()
  (interactive)
  (if (not mark-active)
      (call-interactively 'eval-last-sexp)
    (call-interactively 'eval-region)
    (message "eval-ed.")))

(require 'flycheckcustomizations)
(defun mp-flycheck-dwim ()
  (interactive)
  (flycheck-buffer))

(key-chord-define-global "j;" 'mp-eval-dwim)
(key-chord-define-global "k;" 'eval-defun)
(key-chord-define-global "l;" 'eval-expression)
(key-chord-define-global "b;" 'mp-flycheck-dwim)

;; Autocomplete
(global-set-key (kbd "C-SPC") 'auto-complete)
(define-key ac-menu-map (kbd "s-j") 'ac-next)
(define-key ac-menu-map (kbd "s-k") 'ac-previous)

;; Company
(define-key company-active-map (kbd "s-j") 'company-select-next)
(define-key company-active-map (kbd "s-k") 'company-select-previous)

;; Error navigation / debugging
(key-chord-define-global "kn" 'flycheck-next-error)
(key-chord-define-global "ky" 'flycheck-previous-error)

(provide 'keybindings)
