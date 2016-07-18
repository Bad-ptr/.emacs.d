;;; 5-optimization.el --- performance settings


;;; Code:

;; (setq gc-cons-threshold   (* gc-cons-threshold   25)
;;       max-lisp-eval-depth (* max-lisp-eval-depth 2)
;;       max-specpdl-size    (* max-specpdl-size    4))


(setq
 font-lock-support-mode 'jit-lock-mode
 jit-lock-stealth-time 0.5
 jit-lock-defer-time   0.5
 jit-lock-context-time 0.5)


(setq-default bidi-display-reordering nil)
(setq redisplay-dont-pause nil)


;; 5-optimizations.el ends here
