;;; wrap-with.el --- wrap selected text with string, pairs, html tag, etc.

;; Copyright (C) 2012 Constantin Kulikov (Bad_ptr)

;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 0.9
;; Keywords: wrap, autopair
;; URL: https://github.com/Bad-ptr/.emacs.d/blob/master/site-lisp/wrap-with.el

;;; License:

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

;; Installation:

;; Put this file into your load-path and the following into your ~/.emacs:
;; (require 'wrap-with)
;; (wrap-with-mode t)

;; Keys:

;; C-c w w -- wrap-with
;; C-c w p -- wrap-with-pair
;; C-c w h t -- wrap-with-html-tag

;;; Code:


(eval-when-compile
  (require 'cl))


(defgroup wrap-with nil
  "Customization of wrap-with-mode."
  :prefix "wrap-with"
  :group 'wrap-with)

(defcustom wrap-with-special-indent-in-brackets t
  "If inside () pressed RET then insert newline, indent,
newline, indent, then go to previous line."
  :group 'wrap-with
  :type 'bool)


(defun w-w/wrap-beg-end ()
  "Get begin and end position of region to wrap."
  (let ((wbeg (mark))
        (wend (point)))
    (cond ((and transient-mark-mode mark-active)
           (when (> wbeg wend)
             (rotatef wbeg wend))
           (list wbeg wend))
          ((looking-at "\\w")
           (save-excursion
             (search-forward-regexp "\\b")
             (skip-syntax-backward "\\w_")
             (setq wbeg (point))
             (skip-syntax-forward "\\w_")
             (setq wend (point))
             (list wbeg wend)))
          (t (list wend wend)))))


(defun* w-w/wrap-with (w-beg-str
                       &optional (w-end-str w-beg-str) (w-beg-end (w-w/wrap-beg-end))
                       (point-pos 'before-closing))
  "General wrapping function.
Inserting w-beg-str at begining of region and w-end-str at ending.
Set point according to point-pos."
  (interactive "sWrap region with: ")
  (multiple-value-bind (w-beg w-end) w-beg-end
    (goto-char w-beg)
    (insert w-beg-str)
    (goto-char (+ w-end (string-width w-beg-str)))
    (insert w-end-str)
    (case point-pos
      ('before-opening (goto-char w-beg))
      ('before-closing (goto-char (+ w-end (string-width w-beg-str))))
      ('after-opening (goto-char (+ w-beg (string-width w-beg-str))))
      ('after-closing (forward-char (string-width w-end-str))))))

;; Wrap with pairs:

(defconst w-w/pairs-predefined
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}"))
  "Predefined wrapping pairs.")

(defvar-local w-w/pairs (list) "Wrapping pairs.")
;; (make-variable-buffer-local 'w-w/pairs)
;; (setq-default w-w/pairs w-w/pairs-predefined)

(defun w-w/set-pairs-from-syntax-table ()
  "Add pairs based on `major-mode'."
  (let ((nplist (list)))
    (dotimes (char 256)
      (let* ((syntax-entry (aref (syntax-table) char))
             (class (and syntax-entry
                         (syntax-class syntax-entry)))
             (pair (and syntax-entry
                        (rest syntax-entry))))
        (cond
         ((and (eq class (car (string-to-syntax "("))) pair)
          (add-to-list 'nplist (cons (string char) (string pair)))))))
    (if (< (length nplist) 1)
        (setq w-w/pairs w-w/pairs-predefined)
      (setq w-w/pairs nplist))))


(defun* w-w/enlarge-region-if-wrapped (p-open p-close
                                              &optional (w-beg-end (w-w/wrap-beg-end)))
  "If region is already wrapped wit p-open/p-close pair then include them in region."
  (multiple-value-bind (w-beg w-end) w-beg-end
    (let ((look-regex (if (= w-beg w-end)
                          "\\s-"
                        "\\(\\s-\\|\n\\)")))
      (flet ((opp ()
                  (save-excursion
                    (goto-char w-beg)
                    (while (looking-back look-regex)
                      (backward-char))
                    (if (not (looking-back (regexp-quote p-open)))
                        nil
                      (- (point) (string-width p-open))))))
        (let ((strt (opp)))
          (save-excursion
            (goto-char w-end)
            (while (looking-at look-regex)
              (forward-char))
            (if (not (and (looking-at (regexp-quote p-close)) strt))
                (list nil w-beg-end)
              (list t (list strt (+ (point) (string-width p-close)))))))))))

(defun* w-w/wrap-with-pair (p-str
                            &optional (w-beg-end (w-w/wrap-beg-end))
                            close-pair)
  "Wrap region with p-str and close-pair.
If close-pair is nil search pair for p-str and wrap region with that pair.
If no pair found then use p-str as opening and closing."
  (interactive "sWrap with pairs: ")
  (let ((p-open p-str)
        (inp-is-closing-pair nil)
        (p-close close-pair))
    (unless p-close
      (setq p-close (cdr (assoc p-str (union w-w/pairs-predefined w-w/pairs)))))
    (unless p-close
      (setq p-close (car (rassoc p-str (union w-w/pairs-predefined w-w/pairs))))
      (if (not p-close)
          (setq p-close p-open
                inp-is-closing-pair t)
        (rotatef p-open p-close)
        (setq inp-is-closing-pair t)))
    (if (not inp-is-closing-pair)
        (w-w/wrap-with p-open p-close w-beg-end 'after-opening)
      (if (looking-at (concat "\\s-*" (regexp-quote p-close)))
          (progn
            (while (looking-at "\\s-")
              (forward-char))
            (forward-char (string-width p-close)))
        (multiple-value-bind (is-enlrgd beg-end)
            (w-w/enlarge-region-if-wrapped p-open p-close w-beg-end)
          (if (not is-enlrgd)
              (w-w/wrap-with p-open p-close beg-end)
            (multiple-value-bind (beg end) beg-end
              (set-mark beg)
              (goto-char end))))))))


;; Wrap with html tag:

(defsubst w-w/get-first-part-of-string (str)
  "Get first part of string before first occurence of space symbol."
  (substring str 0 (string-match "[ \f\t\n\r\v]+" str)))

(defun w-w/trim-html-tag-braces (str)
  "Remove '<' from begining of string and '>' from end."
  (if (and (stringp str) (> (string-width str) 0))
      (let ((str-len (string-width str)))
        (substring str (if (string= "<" (substring str 0 1))
                           1
                         0)
                   (if (string= ">" (substring str (1- str-len) str-len))
                       (1- str-len)
                     str-len)))
    ""))

(defun w-w/get-html-tag-name (str)
  "Get tag name."
  (w-w/get-first-part-of-string (w-w/trim-html-tag-braces str)))

(defun w-w/wrap-with-html-tag (inp-str)
  "Wrap with <tagname> </tagname> pair."
  (interactive "sWrap region with html tag: ")
  (w-w/wrap-with (concat "<" (w-w/trim-html-tag-braces inp-str) ">")
                 (concat "</" (w-w/get-html-tag-name inp-str) ">")))


;; Wrap with dwim:

(defun* w-w/wrap-with-dwim ()
  "Do nothing for now."
  nil)


(defun w-w/between-pair ()
  (let ((open (char-before))
        (close (char-after)))
    (when (and open close)
      (let* ((open (char-to-string open))
             (close (char-to-string close))
             (pairs (assoc open (union w-w/pairs-predefined w-w/pairs))))
        (and pairs (string= close (cdr pairs)))))))


(defun w-w/newline-indent ()
  (interactive)
  (if (or (not wrap-with-special-indent-in-brackets)
          (not (w-w/between-pair)))
      (let ((wrap-with-mode nil))
        (call-interactively (key-binding (kbd "RET"))))
    (newline)
    (save-excursion
      (newline-and-indent))
    (indent-according-to-mode)))

(defun w-w/backspace-current-pair ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (let ((close-pos (1- (save-excursion (forward-list)))))
      (delete-char 1)
      (goto-char close-pos)
      (delete-backward-char 1))))


;; wrap-with-mode:

(defvar wrap-with-mode-map (make-sparse-keymap)
  "Keymap for wrap-with-mode.")
(mapc #'(lambda (p)
          (lexical-let ((op (car p))
                        (cl (cdr p)))
            (when op
              (define-key wrap-with-mode-map (read-kbd-macro op)
                #'(lambda () (interactive) (w-w/wrap-with-pair op (w-w/wrap-beg-end) cl))))
            (when cl
              (define-key wrap-with-mode-map (read-kbd-macro cl)
                #'(lambda () (interactive) (w-w/wrap-with-pair cl))))))
      '(("(" . ")")
        ("[" . "]")
        ;;("<" . ">")
        ("{" . "}")
        ("\"" . nil)))

(define-key wrap-with-mode-map (kbd "C-c w w") #'w-w/wrap-with)
(define-key wrap-with-mode-map (kbd "C-c w p") #'w-w/wrap-with-pair)
(define-key wrap-with-mode-map (kbd "C-c w h t") #'w-w/wrap-with-html-tag)
(define-key wrap-with-mode-map (kbd "M-DEL") #'w-w/backspace-current-pair)
(define-key wrap-with-mode-map (kbd "RET") #'w-w/newline-indent)

;;;###autoload
(define-minor-mode wrap-with-mode

  "Toggle wrap-with-mode"

  :require 'wrap-with
  :group 'wrap-with
  :keymap wrap-with-mode-map
  :init-value nil
  :global t
  :lighter " w-w"

  (if wrap-with-mode
      (progn
        (w-w/set-pairs-from-syntax-table)
        (add-hook 'after-change-major-mode-hook #'w-w/set-pairs-from-syntax-table))
    (remove-hook 'after-change-major-mode-hook #'w-w/set-pairs-from-syntax-table)))


(provide 'wrap-with)

;;; wrap-with.el ends here
