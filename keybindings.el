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
 ;; Replace normal m-x with smex
 ;; "M-x" smex
 ;; "M-X" smex-major-mode-commands

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
     ;; ("x"   smex)
     ;; ("X"   smex-major-mode-commands)
     ("a"   mp-ack-in-project)
     ("m"   multi-term)
     
     ;; ("d"   mp-find-file-dwim)
     ("f"   ido-find-file)
     ("F"   ido-find-alternate-file)
     ;; ("s-f" mp-show-in-finder)
     ;; ("s-x" mp-open-with-external-editor)
     
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


(provide 'keybindings)
