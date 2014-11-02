;; C# Flycheck - Stolen from http://bbbscarter.wordpress.com/category/coding/unity/
(require 'flycheck)

(flycheck-define-checker csharp-unity
"Custom checker for Unity projects"
:modes (csharp-mode)
:command ("python" (eval (mp-unity-make-file-path)) "fast" (eval (projectile-project-root)) source-original source)
:error-patterns((warning line-start (file-name) "(" line (zero-or-more not-newline) "): " (message) line-end)
(error line-start (file-name) "(" line (zero-or-more not-newline) "): " (message) line-end))
:predicate (lambda () omnisharp-mode))

(defun mp-unity-make-file-path ()
  (interactive)
  (concat (projectile-project-root) "make.py"))

(defun mp-select-unity-checker-maybe ()
  (interactive)
  (if (file-exists-p (mp-unity-make-file-path))
      (flycheck-select-checker 'csharp-unity)
    (message "did not select checker")))

(add-to-list 'flycheck-checkers 'csharp-unity)

;; elisp checker seems like it has some issues that are more distracting tha productive right now.
(add-to-list 'flycheck-disabled-checkers 'emacs-lisp)

(add-hook 'csharp-mode-hook 'mp-select-unity-checker-maybe)
                              
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-check-syntax-automatically '(save))
(setq flycheck-display-errors-delay 0.0)

(provide 'flycheckcustomizations)
;;; flycheckcustomizations.el ends here
