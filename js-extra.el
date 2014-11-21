(require 'flymake-jslint)
 
(add-hook 'js-mode-hook
          'flymake-jslint-load
          (setq-local helm-dash-docsets '("JavaScript" "NodeJS")))

(provide 'js-extra)
