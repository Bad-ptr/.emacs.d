;;; my-std-lib.el --- different usefull functions

(require 'cl-lib)

;;; Code:

(defmacro alambda (arglist &rest body)
  "Anaphoric lambda."
  (declare (indent defun))
  `(cl-labels ((self ,arglist ,@body))
     #'self))

;; with-eval-after-load for Emacs < 24.4
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))


;; my-std-lib.el ends here
