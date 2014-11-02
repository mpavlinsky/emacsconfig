;; C# Flycheck - Stolen from http://bbbscarter.wordpress.com/category/coding/unity/
(require 'flycheck)
(flycheck-define-checker csharp-unity
"Custom checker for Unity projects"
:modes (csharp-mode)
:command ("python" (eval (concat (projectile-project-root) "make.py")) "fast" (eval (projectile-project-root)) source-original source)
:error-patterns((warning line-start (file-name) "(" line (zero-or-more not-newline) "): " (message) line-end)
(error line-start (file-name) "(" line (zero-or-more not-newline) "): " (message) line-end)))

(setq flycheck-highlighting-mode 'lines)
(setq flycheck-check-syntax-automatically '(save))


(provide 'flycheckcustomizations)
