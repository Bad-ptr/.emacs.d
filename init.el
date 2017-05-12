;;; init.el --- my emacs configuration. -*- lexical-binding: t; -*-


;;; Code:

;; (profiler-start 'mem)

(setq inhibit-default-init      t
      package-enable-at-startup nil
      load-prefer-newer         t
      message-log-max           1000)


;; https://www.reddit.com/r/emacs/comments/4586eq/quick_emacs_snippet_to_automatically_use/
;; Automagical EmacsClient functionality
;;
;; Basically, if Emacs is already running, this shunts things over to
;; the existing Emacs; otherwise, it readies itself to accept said
;; shunting.
;;
;; This operates by detecting the existence of an Emacs server socket
;; file.  If a socket is found ("/tmp/emacs$UID/server"), Emacs will
;; spin up emacsclient and immediately exit itself.  Otherwise, Emacs
;; will start a new server.
(defun server-already-running-p ()
  "Is Emacs already running?"
  (file-exists-p (format "/tmp/emacs%s/server" (user-uid))))
(defun server-shunt ()
  "Shunts to emacsclient"
  (require 'cl-lib)
  (let ((args (append '("emacsclient" "-a" "''")
                      (cdr command-line-args))))
    (setq args (append args
                       (if window-system
                           '("-n" "-c")
                         '("-n" "-c")
                         ;;'("-t") ;; it doesn't work
                         )))
    (shell-command (substring (format "%S" args) 1 -1))
    (kill-emacs)))
;; this don't work with magit (with-editor)
;; (unless (featurep 'server)
;;   (if (server-already-running-p) (server-shunt) (server-start)))


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
  (push dirname load-path)
  (load (expand-file-name "init.el" dirname) t t t))


(defun my/-init-before-private ()
  "Init actions before private information set."
  (push (locate-user-emacs-file "site-lisp/") load-path)

  (defconst my/-common-conf-path (locate-user-emacs-file "site-lisp/my-config/"))

  ;; load custom file
  (setq custom-file (locate-user-emacs-file "my-custom.el"))
  (load custom-file t t)

  ;; Load other parts of configuration
  (my/-load-directory my/-common-conf-path))


(defun my/-init-after-private ()
  "Init actions after private information set."

  (push (locate-user-emacs-file "site-lisp/") load-path)

  (defconst my/-conf-path
    (locate-user-emacs-file (concat "site-lisp/" my/-username "-config/")))

  ;; Packages
  (require 'package)
  (package-initialize)
  (run-hooks 'my/-packages-initialized-hook)

  ;; load custom file
  (setq custom-file
        (locate-user-emacs-file (concat my/-username "-custom.el")))
  (load custom-file t t)

  ;; Install my packages at first run
  (unless (file-exists-p package-user-dir)
    (when (fboundp 'my/-install-favourite-packages)
      (let ((package-archives package-archives)
            (my/-favourite-packages-list my/-favourite-packages-list))
        (add-to-list 'package-archives '("GNU/ELPA" . "http://elpa.gnu.org/packages/"))
        (push 'rainbow-mode my/-favourite-packages-list)
        (unless (featurep 'cl-lib)
          (push 'cl-lib my/-favourite-packages-list))
        (my/-install-favourite-packages))))

  (my/-load-directory my/-conf-path)

  (run-hooks 'my/-config-loaded-hook)
  (when (fboundp 'my/-exec-after-all-parts-of-config-loaded)
    (my/-exec-after-all-parts-of-config-loaded)))


(defcustom my/-packages-initialized-hook nil
  "Hoot that runs after packages-initialize."
  :group 'initialization
  :type 'hook)

(defcustom my/-config-loaded-hook nil
  "Hook to run after all parts of config have been loaded."
  :group 'initialization
  :type 'hook)

(defcustom my/-username-hook nil
  "Hook to run when `my/-username' changes."
  :group 'initialization
  :type 'hook)

;; Make sure that users set their private settings and load it.
(add-hook
 'my/-username-hook
 (alambda (&optional arg)
   (unless arg
     (condition-case-unless-debug err (my/-init-before-private)
       (error (my/-init-error-fatal err))))
   (lexical-let ((priv-file (locate-user-emacs-file
                             (concat
                              (if my/-multiuser-private
                                  my/-username "my") "-private.el"))))
     (and
      (condition-case-unless-debug err (load priv-file)
        (file-error
         (message "%s" err)
         (let ((ff
                (lambda ()
                  (let ((example-file (locate-user-emacs-file "my-private.example"))
                        (cw (selected-window)))
                    (my/-warning (format "You run emacs without your private settings set(first run?).
 To fix this please create a %s file (See %s as example)." priv-file example-file))
                    (set-window-buffer
                     cw (let ((template-auto-insert nil))
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
      (condition-case-unless-debug err
          (progn (my/-init-after-private)
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
