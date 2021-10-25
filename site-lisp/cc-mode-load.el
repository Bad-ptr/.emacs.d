(let ((cc-dir (locate-user-emacs-file "site-lisp/cc-mode")))
  (add-to-list 'load-path cc-dir)
  (my/-load-directory cc-dir))
