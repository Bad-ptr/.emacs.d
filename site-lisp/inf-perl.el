;;; inf-perl.el --- run the Perl toplevel in an Emacs buffer.

;; Copyright (C) 2014 Constantin Kulikov
;;
;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 0.1
;; Package-Requires: ()
;; Date: 2014/11/11 17:53:52
;; License: GPL either version 2 or any later version
;; Keywords: perl, repl, inferior
;; X-URL: https://github.com/Bad-ptr/.emacs.d/blob/master/site-lisp/inf-perl.el

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Perl inferior mode.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'inf-perl)

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar inferior-perl-program "re.pl"
  "TODO: Docstring.")

(define-derived-mode inferior-perl-mode comint-mode "Inf-Perl"
  "TODO: Docstring."
  (setq-local comint-prompt-regexp "\$ ")
  (setq-local comment-start "#")
  (setq-local comment-end "\n")
  (setq-local comint-prompt-read-only t))

(defun perl-repl (&optional cmd)
  (interactive (list (read-from-minibuffer "Perl command to run: " inferior-perl-program)))
  (unless cmd (setq cmd inferior-perl-program))
  (if (not (comint-check-proc "*inferior-perl*"))
      (progn
        (switch-to-buffer (make-comint "inferior-perl" cmd))
        (inferior-perl-mode)))
  (switch-to-buffer "*inferior-perl*"))

(provide 'inf-perl)

;;; inf-perl.el ends here

;; (let* ((redirect-buffer (current-buffer))
;;        (redir-proc (get-buffer-process redirect-buffer)))
;;   (let ((comint-redirect-echo-input nil)
;;         (comint-redirect-completed nil)
;;         (comint-redirect-perform-sanity-check nil)
;;         (comint-redirect-insert-matching-regexp t)
;;         (comint-redirect-finished-regexp "r ")
;;         (comint-redirect-output-buffer redirect-buffer))
;;     (set-process-filter redir-proc #'comint-redirect-filter)
;;     (process-send-string redir-proc "$v\t")
;;     (accept-process-output redir-proc 2)
;;     (set-process-filter redir-proc #'comint-output-filter)))

(let* ((redirect-buffer (current-buffer))
       (redir-proc (get-buffer-process redirect-buffer)))
  (let ((comint-redirect-echo-input nil)
        (comint-redirect-completed nil)
        (comint-redirect-perform-sanity-check nil)
        (comint-redirect-insert-matching-regexp t)
        (comint-redirect-finished-regexp "r ")
        (comint-redirect-output-buffer redirect-buffer))
    (process-send-string redir-proc "$v\t")
    (accept-process-output redir-proc 2)
    ))
