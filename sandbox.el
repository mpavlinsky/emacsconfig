;; Focus on the help window immediately
(setq help-window-select t)


;; Focus on the apropos window immediately
(defadvice apropos-command (after mp-focus-apropos (pattern &optional do-all var-predicate))
  (other-window 1))

(ad-activate 'apropos-command)


(defun mp-reinit ()
  (interactive)
  (message "foo")
  (let* ((buffer-name "~/.emacs.d/init.el")
		 (init-buffer (get-file-buffer buffer-name)))
	(message (buffer-name init-buffer))
	(save-buffer init-buffer)
	(load-file buffer-name)))


; debugging
(setq debug-on-error t)
