(setq auto-mode-alist
      (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))

(add-to-list 'load-path (locate-user-emacs-file "site-lisp/ocaml"))

;; (autoload 'caml-mode "ocaml" (interactive)
;;   "Major mode for editing Caml code." t)
;; (autoload 'camldebug "camldebug" (interactive) "Debug caml mode")
;;(require 'caml-font)
;;(require 'caml-hilit)

(require 'ocaml)
