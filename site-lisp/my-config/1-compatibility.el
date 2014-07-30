;;; 1-compatibility.el --- functions for different emacs versions


;;; Code:


;; ---
;; Support key sequencies in russian layout

(when (and (or (> emacs-major-version 24)
               (and (= emacs-major-version 24) (>= emacs-minor-version 3)))
           my/-non-english-input-method)

  (defun reverse-input-method (input-method)
    "Build the reverse mapping of single letters from INPUT-METHOD."
    (interactive
     (list (read-input-method-name "Use input method (default current): ")))
    (if (and input-method (symbolp input-method))
        (setq input-method (symbol-name input-method)))
    (let ((current current-input-method)
          (modifiers '(nil (control) (meta) (control meta))))
      (when input-method
        (activate-input-method input-method))
      (when (and current-input-method quail-keyboard-layout)
        (dolist (map (cdr (quail-map)))
          (let* ((to (car map))
                 (from (quail-get-translation
                        (cadr map) (char-to-string to) 1)))
            (when (and (characterp from) (characterp to))
              (dolist (mod modifiers)
                (define-key local-function-key-map
                  (vector (append mod (list from)))
                  (vector (append mod (list to)))))))))
      (when input-method
        (activate-input-method current))))

  (if (not (daemonp))
      (reverse-input-method my/-non-english-input-method)
    (defun rev-inp-m-init (f)
      (lexical-let ((frame f))
        (run-at-time nil nil
                     #'(lambda () (unless (and (daemonp) (eq frame terminal-frame))
                                    (with-selected-frame frame
                                      (reverse-input-method my/-non-english-input-method)
                                      ;; instead of around advice
                                      (setq read-passwd-map
                                            (let ((map read-passwd-map))
                                              (set-keymap-parent map minibuffer-local-map)
                                              (define-key map [return] #'exit-minibuffer)
                                              (define-key map [backspace] #'delete-backward-char)
                                              map))
                                      (remove-hook 'after-make-frame-functions #'rev-inp-m-init)))))))
    (add-hook 'after-make-frame-functions #'rev-inp-m-init))
  
  ;; (defadvice read-passwd (around my-read-passwd act)
  ;;   (let ((local-function-key-map nil))
  ;;     ad-do-it))
  ;; (ad-enable-advice #'read-passwd 'around 'my-nullkeymap-read-passwd)
  ;; (ad-activate #'read-passwd)
  )
;;---


;; 1-compatibility.el ends here
