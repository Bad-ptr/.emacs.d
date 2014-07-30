;;; my-std-lib.el --- different usefull functions

(if (locate-library "cl-lib")
    (require 'cl-lib)
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


;; my-std-lib.el ends here
