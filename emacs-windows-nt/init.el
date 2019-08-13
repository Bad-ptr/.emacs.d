;;; init.el --- windows compatibility. -*- lexical-binding: t; -*-

(add-hook 'my/-config-loaded-hook
          #'(lambda ()
              (setq interprogram-paste-function #'gui-selection-value)
              (windmove-default-keybindings 'control)
              (set-clipboard-coding-system 'utf-16-le)
              ;; (set-selection-coding-system 'utf-16-le)
              ))

;;; init.el ends here
