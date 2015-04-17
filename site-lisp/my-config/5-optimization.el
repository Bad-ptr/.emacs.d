;;; 5-optimization.el --- performance settings


;;; Code:


(setq gc-cons-threshold 20000000)

(setq max-lisp-eval-depth 1600
      max-specpdl-size 4680)


(jit-lock-mode t)
(setq jit-lock-stealth-time 0.5
      jit-lock-defer-time 0.5
      ;;jit-lock-contextually nil
      jit-lock-context-time 0.5)
;; defaults
;; (setq jit-lock-stealth-time nil
;;       jit-lock-defer-time nil)

(setq-default bidi-display-reordering nil)
(setq redisplay-dont-pause nil)


;; 5-optimizations.el ends here
