;;; init.el --- windows compatibility. -*- lexical-binding: t; -*-

(add-hook 'my/-config-loaded-hook
          #'(lambda ()
              (setq interprogram-paste-function #'gui-selection-value)
              (windmove-default-keybindings 'control)))

;;; init.el ends here
