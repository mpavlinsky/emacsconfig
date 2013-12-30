;; Focus on the help window immediately
(setq help-window-select t)


;; Focus on the apropos window immediately
(defadvice apropos-command (after mp-focus-apropos (pattern &optional do-all var-predicate))
  (other-window 1))

(ad-activate 'apropos-command)

; debugging
;; (setq debug-on-error t)

