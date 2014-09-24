;;; my-std-lib.el --- different usefull functions

(if (locate-library "cl-lib")
    (progn
      (require 'cl-lib)
      (unless (fboundp 'lexical-let)
        (defmacro lexical-let (bindings &rest body)
          (unless lexical-binding
            (my/-warning "You are trying to use lexical let with lexical-binding = nil."))
          `(let ,bindings ,@body))))
  (require 'cl)
  (defalias 'cl-labels 'labels))

;;; Code:

(defmacro alambda (arglist &rest body)
  "Anaphoric lambda."
  (declare (indent defun))
  `(cl-labels ((self ,arglist ,@body))
     #'self))

(defmacro add-hook-that-fire-once (hook arglist &rest body)
  "Hook that autoremove itself after first execution"
  (declare (indent defun))
  `(add-hook ,hook (alambda ,arglist
                     ,@body
                     (remove-hook ,hook #'self))))

(defun my/-is-interactive-frame-available ()
  (and (not noninteractive)
       (not (and (daemonp)
                 (null (cdr (frame-list)))
                 (eq (selected-frame) terminal-frame)))))

(defmacro my/-exec-after-interactive-frame-available (&rest body)
  (declare (indent defun))
  `(if (my/-is-interactive-frame-available)
       (progn ,@body)
     (add-hook-that-fire-once 'after-make-frame-functions (frame)
       (with-selected-frame (frame)
         ,@body))))

;; with-eval-after-load for Emacs < 24.4
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))

;; defvar-local for Emacs < 24.2
(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
    (declare (debug defvar) (doc-string 3))
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))


(defun my/-error-message (error-class msg &optional fatal face)
  (let* ((ecs (concat "[" error-class "]"))
         (errbuf (get-buffer-create "*my/-errors*"))
         (errstr (format "%s: %s" (propertize ecs 'face (or face 'compilation-error)) msg)))
    (message "%s" errstr)
    (with-current-buffer errbuf
      (insert-string errstr)
      (newline)
      (font-lock-add-keywords nil `((,(concat "^\\[" error-class "\\]") 0 'error t))))
    (when fatal
      (switch-to-buffer errbuf)
      (search-backward ecs))))
(defun my/-warning (msg)
  (my/-error-message "WARNING" msg nil 'compilation-warning))
(defun my/-init-error-warning (msg)
  (my/-error-message "INIT_ERROR" msg))
(defun my/-init-error-fatal (msg)
  (my/-error-message "INIT_ERROR" msg t))

(defun my/-load-directory (dir)
  (when (file-directory-p dir)
    (dolist (file (directory-files dir t ".+\.el"))
      (load file))))

(with-eval-after-load "package"
  (defun require-package (package &optional min-version no-refresh)
    "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
    (if (package-installed-p package min-version)
        t
      (if (or (assoc package package-archive-contents) no-refresh)
          (condition-case err (package-install package)
            (error (my/-warning (format "Package: %s, Error: %s." package err))))
        (progn
          (package-refresh-contents)
          (require-package package min-version t)))))
  (defun my/-install-favourite-packages ()
    (interactive)
    (when (boundp 'my/-favourite-packages-list)
      (dolist (pkg my/-favourite-packages-list)
        (require-package pkg)))))


(when (or (> emacs-major-version 24)
               (and (= emacs-major-version 24) (>= emacs-minor-version 3)))
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
        (activate-input-method current)))))

;; my-std-lib.el ends here
