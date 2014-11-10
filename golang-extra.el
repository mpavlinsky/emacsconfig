(require 'go-mode)

(flycheck-define-checker go-app-build
  "A Go syntax and type checker using the `go build' command.

See URL `http://golang.org/cmd/go'."
  ;; We need to use `temporary-file-name' instead of `null-device', because Go
  ;; can't write to the null device.  It's “too magic”.  See
  ;; https://code.google.com/p/go/issues/detail?id=4851 for details.
  :command ("goapp" "build" "-o" temporary-file-name)
  :error-patterns
  ((error line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end))
  :modes go-mode
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (not (string-suffix-p "_test.go" (buffer-file-name)))))
  :next-checkers ((warning . go-errcheck)))

(add-to-list 'flycheck-checkers 'go-app-build)

;; Always just use the GAE checker for now. Maybe in the future only use GAE if app.yaml is present in the project root?
(add-hook 'go-mode-hook (lambda() (flycheck-select-checker 'go-app-build)))

(provide 'golang-extra)
