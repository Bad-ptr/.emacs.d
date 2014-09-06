;;; my-std-lib.el --- different usefull functions

(if (locate-library "cl-lib")
    (progn
      (require 'cl-lib)
      (unless (fboundp 'lexical-let)
        (defmacro lexical-let (bindings &rest body)
          (unless lexical-binding
            (message "[Warning]: You are trying to use lexical let with lexical-binding = nil."))
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

;; my-std-lib.el ends here
