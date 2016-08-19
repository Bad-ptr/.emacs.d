;;; my-std-lib.el --- different usefull functions. -*- lexical-binding: t; -*-

(if (locate-library "cl-lib")
    (progn
      (require 'cl-lib)
      (unless (fboundp 'lexical-let)
        (defmacro lexical-let (bindings &rest body)
          (unless lexical-binding
            (my/-warning "You are trying to use lexical let with lexical-binding = nil."))
          `(let ,bindings ,@body)))

      ;; (when (autoloadp (symbol-function 'cl-labels))
      ;;   (autoload-do-load (symbol-function 'cl-labels)))
      ;; (lexical-let ((old-labels (symbol-function 'cl-labels)))
      ;;   (defmacro cl-labels (bindings &rest body)
      ;;     (unless lexical-binding
      ;;       (my/-warning "You are trying to use cl-labels with lexical-binding = nil."))
      ;;     `(,old-labels ,bindings ,@body)))
      )
  (require 'cl)
  (defalias 'cl-letf 'letf)
  (defalias 'cl-labels 'labels)
  (defalias 'cl-defmacro 'defmacro*)
  (defalias 'cl-defun 'defun*))


;;; Code:

(defvar my/-load-in-progress nil)
(defvar my/-load-file-name nil)
(lexical-let ((original-load (symbol-function 'load)))
  (let ((original-doc (documentation 'load t)))
    (defun load (file &optional noerror nomessage nosuffix must-suffix)
      (setq my/-load-in-progress t
            my/-load-file-name file)
      (prog1
          (funcall original-load file noerror nomessage nosuffix must-suffix)
        (setq my/-load-in-progress nil
              my/-load-file-name nil)))
    (put 'load 'function-documentation original-doc)))

;; old and potentially buggy
;; (defmacro alambda (arglist &rest body)
;;   "Anaphoric lambda."
;;   (declare (indent defun))
;;   `(cl-labels ((self ,arglist ,@body))
;;      #'self))

;; new and potentially better than old
(when (version< emacs-version "24.4")
  (unless (fboundp 'self)
    (fset 'self nil)))
(defmacro alambda (args &rest body)
  "Anaphoric lambda."
  (declare (indent defun))
  `(lexical-let ((self))
     (setq self #'(lambda ,args
                    (cl-letf (((symbol-function 'self) self))
                      ,@body)))
     self))

(defmacro add-hook-that-fire-once (hook arglist &rest body)
  "Hook that autoremove itself after first execution"
  (declare (indent defun))
  `(add-hook ,hook (alambda ,arglist
                            ,@body
                            (remove-hook ,hook self))))

(defun my/-is-interactive-frame-available ()
  (and (not noninteractive)
       (not (and (daemonp)
                 (null (cdr (frame-list)))
                 (eq (selected-frame) terminal-frame)))))

(cl-defmacro my/-exec-after-interactive-frame-available ((&rest captures) &rest body)
  (declare (indent defun))
  `(if (my/-is-interactive-frame-available)
       (progn ,@body)
     (lexical-let (,@(mapcar #'(lambda (c) (list c c)) captures))
       (add-hook-that-fire-once 'after-make-frame-functions (frame)
         (with-selected-frame frame
           ,@body)))))

(cl-defmacro my/-exec-after-delay ((&rest captures) delay &rest body)
  (declare (indent defun))
  `(lexical-let (,@(mapcar #'(lambda (c) (list c c)) captures))
     (run-at-time ,delay nil #'(lambda () ,@body))))

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

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    (declare (indent defun))
    "Set variable VAR to value VAL in current buffer."
    `(set (make-local-variable ',var) ,val)))


(when (version< emacs-version "24.4")
  (unless (fboundp 'incf)
    (defmacro incf (n)
      `(setq ,n (1+ ,n))))
  (lexical-let ((original-backtrace-frame (symbol-function 'backtrace-frame)))
    (defun backtrace-frame (nframes &optional base)
      (let ((i (if base
                   (let ((k 8) found bt)
                     (while (and (not found)
                                 (setq bt (cadr (funcall original-backtrace-frame
                                                         (incf k)))))
                       ;; (message "%s:%s" k (backtrace-frame k))
                       (when (eq bt base) (setq found t)))
                     (when found (+ nframes (- k 3))))
                 (+ nframes 6))))
        (when i
          (funcall original-backtrace-frame i))))))
(defun my/-error-message (error-class msg &optional fatal face tracedepth)
  (unless tracedepth (setq tracedepth 8))
  (let* ((ecs (concat "[" error-class "]"))
         (errbuf (get-buffer-create "*my/-errors*"))
         call-stack
         (btf (let ((i 2) ci bf)
                (while (setq ci (backtrace-frame i 'my/-error-message))
                  (when (eq (car ci) t)
                    (push (cdr ci) call-stack))
                  (when (< i (+ 2 tracedepth))
                    (push (cdr ci) bf))
                  (setq i (1+ i)))
                bf))
         (errbrief (concat
                    (format "%s: %s" (propertize ecs 'face (or face 'compilation-error)) msg)
                    (and load-in-progress
                         (format "\n\tIn %s." (or load-file-name (buffer-file-name))))
                    (and my/-load-in-progress
                         (format "\n\tIn %s." my/-load-file-name))))
         (errstr (concat
                  errbrief
                  ;;(format "%s: %s" (propertize ecs 'face (or face 'compilation-error)) msg)
                  (and call-stack
                       (format "\n Call stack:\n\t\t%s"
                               (mapconcat #'(lambda (elt) (format "%s" elt))
                                          call-stack "\n\t\t")))
                  (and btf
                       (format "\n  Backtrace:\n\t\t%s"
                               (mapconcat #'(lambda (elt) (format "%s" elt))
                                          btf "\n\t\t"))))))
    (message "%s" errbrief)
    (with-current-buffer errbuf
      (unless (eq major-mode 'special-mode)
        (special-mode))
      (read-only-mode -1)
      (goto-char (point-min))
      (insert-string errstr)
      (newline)
      (read-only-mode 1)
      (font-lock-add-keywords nil `((,(concat "^\\[" error-class "\\]") 0 'error t))))
    (when (or fatal (not (my/-is-interactive-frame-available)))
      (my/-exec-after-interactive-frame-available (errstr errbuf ecs)
        (my/-exec-after-delay () 2
          (message "%s" errstr)
          (pop-to-buffer errbuf)
          (search-backward ecs))))))
(defun my/-warning (msg)
  (my/-error-message "WARNING" msg nil 'compilation-warning 0))
(defun my/-init-error-warning (msg)
  (my/-error-message "INIT_ERROR" msg nil nil 5))
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
          (condition-case-unless-debug err (package-install package)
            (error (my/-warning (format "Package: %s, Error: %s." package err))))
        (progn
          (package-refresh-contents)
          (require-package package min-version t)))))
  (defun my/-install-favourite-packages ()
    (interactive)
    (when (boundp 'my/-favourite-packages-list)
      (dolist (pkg my/-favourite-packages-list)
        (require-package pkg)))))


(unless (version< emacs-version "24.1")
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
