;;; init.el --- my emacs configuration. -*- lexical-binding: t; -*-


;;; Code:



(setq inhibit-default-init t
      load-prefer-newer t
      message-log-max 1000)


;;load functions that will be used during initialization
(load (locate-user-emacs-file "my-std-lib"))

;;load compatibility code
(dolist (dirname (mapcar #'(lambda (dirn)
                             (expand-file-name (concat "emacs-" dirn)
                                               user-emacs-directory))
                         (list
                          (number-to-string emacs-major-version)
                          emacs-version
                          (symbol-name system-type))))
  (add-to-list 'load-path dirname)
  (load (expand-file-name "init.el" dirname) t t t))


(defun my/-init-before-private ()
  "Init actions before private information set."
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp/"))

  (defconst my/-common-conf-path (locate-user-emacs-file "site-lisp/my-config/"))

  ;; load custom file
  (setq custom-file (locate-user-emacs-file "my-custom.el"))
  (load custom-file t t)

  (setq package-enable-at-startup t)
  ;; Load other parts of configuration
  (my/-load-directory my/-common-conf-path))


(defcustom my/-config-loaded-hook nil
  "Hook to run after all parts of config have been loaded."
  :group 'initialization
  :type 'hook)

(defun my/-init-after-private ()
  "Init actions after private information set."

  (add-to-list 'load-path (locate-user-emacs-file "site-lisp/"))

  (defconst my/-conf-path (locate-user-emacs-file (concat "site-lisp/" my/-username "-config/")))

  ;; Packages
  (require 'package)
  (package-initialize)

  ;; load custom file
  (setq custom-file (locate-user-emacs-file (concat my/-username "-custom.el")))
  (load custom-file t t)

  ;; Install my packages at first run
  (unless (file-exists-p package-user-dir)
    (when (fboundp 'my/-install-favourite-packages)
      (my/-install-favourite-packages)))

  (my/-load-directory my/-conf-path)

  (run-hooks 'my/-config-loaded-hook)
  (when (fboundp 'my/-exec-after-all-parts-of-config-loaded)
    (my/-exec-after-all-parts-of-config-loaded)))


(defcustom my/-username-hook nil
  "Hook to run when `my/-username' changes."
  :group 'initialization
  :type 'hook)

;; Make sure that users set their private settings and load it.
(add-hook
 'my/-username-hook
 (alambda (&optional arg)
   (unless arg
     (condition-case err (my/-init-before-private)
       (error (my/-init-error-fatal err))))
   (lexical-let ((priv-file (locate-user-emacs-file
                             (concat
                              (if my/-multiuser-private
                                  my/-username "my") "-private.el"))))
     (when (condition-case err (load priv-file)
             (file-error
              (message "%s" err)
              (let ((ff (lambda ()
                          (let ((example-file (locate-user-emacs-file "my-private.example"))
                                (cw (selected-window)))
                            (my/-warning (format "You run emacs without your private settings set(first run?).\n\
 To fix this please create a %s file (See %s as example)." priv-file example-file))
                            (set-window-buffer cw
                                               (let ((template-auto-insert nil))
                                                 (with-current-buffer (find-file priv-file)
                                                   (setq-local lexical-binding t)
                                                   (add-hook 'kill-buffer-hook
                                                             #'(lambda () (funcall self t)) nil t)
                                                   (insert-file-contents example-file nil nil nil t)
                                                   (current-buffer))))
                            (display-buffer-pop-up-window (get-buffer "*my/-errors*") nil)
                            (select-window cw t)
                            nil))))
                (my/-exec-after-interactive-frame-available ()
                  (run-at-time 2 nil ff))))
             (error (my/-init-error-fatal err) nil))
       (condition-case err (progn (my/-init-after-private)
                                  (when arg (run-hooks 'after-init-hook)))
         (error (my/-init-error-fatal err)))))))

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
