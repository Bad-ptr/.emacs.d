(setq auto-mode-alist
      (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))

;;(add-to-list 'load-path (concat my/-conf-path "/ocaml"))

;; (autoload 'caml-mode "ocaml" (interactive)
;;   "Major mode for editing Caml code." t)
;; (autoload 'camldebug "camldebug" (interactive) "Debug caml mode")
;;(require 'caml-font)
;;(require 'caml-hilit)

(require 'caml-font)
(require 'caml-emacs)
(require 'caml)
