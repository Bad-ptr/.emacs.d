;;; init.el --- my emacs configuration.


;;; Code:


;;load functions that will be used during initialization
(load (concat user-emacs-directory "my-std-lib"))


(defun my/-do-init ()
  
  "Actual init actions."
  
  (add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))

  (defconst my/-conf-path (expand-file-name "site-lisp/my-config/" user-emacs-directory))

  ;; Packages
  (require 'package)
  (package-initialize)


  ;; load custom file
  (setq custom-file (expand-file-name (if my/-multiuser-private
                                          (concat my/-username "-custom.el")
                                        "my-custom.el") user-emacs-directory))
  (load custom-file t t)


  ;; Load other parts of configuration
  (dolist (file (directory-files my/-conf-path t ".+\.el"))
    (load file))

  (when (fboundp 'my/-exec-after-all-parts-of-config-loaded)
    (my/-exec-after-all-parts-of-config-loaded)))


(defcustom my/-username-hook nil
  "Hook to run when `my/-username' changes."
  :group 'initialization
  :type 'hook)

;; Make sure that users set their private settings and load it.
(add-hook 'my/-username-hook
          (lambda ()
            (lexical-let ((ff (alambda ()
                                (let ((priv-file (concat user-emacs-directory
                                                         (if my/-multiuser-private
                                                             my/-username "my") "-private.el")))
                                  (when (condition-case err (load priv-file)
                                          (error
                                           (let ((example-file (concat user-emacs-directory "my-private.example")))
                                             (message "[Warning] You run emacs without your private settings set(first run?).\
 To fix this please create %s file (See %s)." priv-file example-file)
                                             (set-window-buffer (selected-window)
                                                                (with-current-buffer (find-file priv-file)
                                                                  (add-hook 'kill-buffer-hook #'self nil t)
                                                                  (insert-file-contents example-file nil nil nil t)
                                                                  (current-buffer)))
                                             nil)))
                                    (condition-case err (my/-do-init)
                                      (error (message "[INIT_ERROR]: %s." err))))))))
              (if (or noninteractive (and (daemonp) (null (cdr (frame-list)))
                                          (eq (selected-frame) terminal-frame)))
                  (add-hook-that-fire-once 'after-make-frame-functions (fr)
                    (run-at-time 2 nil ff))
                (run-at-time 2 nil ff)))))

(defcustom my/-multiuser-private nil
  "Load different private file for dufferent my/-username or not."
  :group 'initialization
  :type 'bool)

(defcustom my/-username user-login-name
  "This variable could be overriden during emacs launch, to run emacs with different settings."
  :group 'initialization
  :set #'(lambda (sym val)
           (set-default sym val)
           (run-hooks 'my/-username-hook))
  :type 'string)


;; init.el ends here
