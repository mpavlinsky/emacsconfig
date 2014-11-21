(require 'color-theme)
(color-theme-initialize)
(color-theme-taming-mr-arneson)

(set-face-attribute 'magit-diff-hunk-header nil
                    :inherit 'region)

(set-face-attribute 'magit-diff-add nil
                    :foreground "green")

(set-face-attribute 'magit-diff-del nil
                    :foreground "red")

(mp-set-font-size 14)
