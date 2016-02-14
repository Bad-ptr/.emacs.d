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
      ('before-closing (backward-char (string-width w-end-str)))
      ('after-opening (goto-char (+ w-beg (string-width w-beg-str))))
      ('after-closing (point)))))

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
    (let ((skip-regex (if (= w-beg w-end)
                          "\\s-"
                        "\\(\\s-\\|\n\\)"))
          is-enlarged new-start new-end)

      (when (region-active-p)
        (save-excursion
          (goto-char w-beg)
          (while (looking-back skip-regex)
            (backward-char))
          (when (looking-back (regexp-quote p-open))
            (setq new-start (1- (point)))

            (goto-char w-end)
            (while (looking-at skip-regex)
              (forward-char))
            (when (looking-at (regexp-quote p-close))
              (setq new-end (1+ (point))
                    is-enlarged t)))))

      (list is-enlarged (list new-start new-end)))))

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
      (setq p-close (car (rassoc p-str (union w-w/pairs-predefined w-w/pairs)))
            inp-is-closing-pair t)
      (if (not p-close)
          (setq p-close p-open)
        (rotatef p-open p-close)))
    (if (not inp-is-closing-pair)
        (w-w/wrap-with p-open p-close w-beg-end 'after-opening)
      (multiple-value-bind (is-enlrgd beg-end)
          (w-w/enlarge-region-if-wrapped p-open p-close w-beg-end)
        (if is-enlrgd
            (multiple-value-bind (beg end) beg-end
              (set-mark beg)
              (goto-char end))
          (if (or (region-active-p)
                  (not (looking-at (regexp-quote p-close))))
              (multiple-value-bind (beg end) w-beg-end
                (w-w/wrap-with p-open p-close w-beg-end
                               (if (and (eq beg end)
                                        (not (string= p-open p-close)))
                                   'after-closing
                                 'before-closing)))
            (forward-char)))))))

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

(defun check-string-parens (str mode)
  (let (unmatched-s matched-s (dir))
    (with-temp-buffer
      (funcall mode)
      (goto-char (point-min))
      (insert str)
      (goto-char (point-min))
      (condition-case err (check-parens)
        (error (setq unmatched-s (char-to-string (char-after))))))
    (when unmatched-s
      (unless (setq dir 'right
                    matched-s
                    (cdr (assoc unmatched-s
                                (union w-w/pairs-predefined w-w/pairs))))
        (setq dir 'left
              matched-s (car (rassoc unmatched-s
                                     (union w-w/pairs-predefined w-w/pairs)))))
      (when matched-s
        (list matched-s dir)))))

(defun w-w/cut-region-with-surronding-pair ()
  (interactive)
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (m-d (check-string-parens
                   (buffer-substring beg end) major-mode)))
        (save-excursion
          (when m-d
            (destructuring-bind (matched-s dir) m-d
              (case dir
                ('right (when (> (point) beg)
                          (goto-char beg))
                        (forward-list)
                        (delete-char -1))
                (t (when (< (point) end)
                     (goto-char end))
                   (backward-list)
                   (delete-char 1)
                   (setq beg (1- beg)
                         end (1- end)))))))
        (kill-region beg end))
    (let (beg end)
      (backward-up-list)
      (setq beg (point))
      (forward-list)
      (setq end (point))
      (kill-region beg end))))

(defun w-w/insert-with-surrounding-pair ()
  (interactive)
  (let* ((y-txt (first kill-ring))
         (m-d (check-string-parens y-txt major-mode)))
    (if (and (region-active-p) m-d)
        (let ((beg (region-beginning))
              (end (region-end)))
          (destructuring-bind (matched-s dir) m-d
            (case dir
              ('right (save-excursion
                        (goto-char beg)
                        (yank)
                        (goto-char (+ end (string-width y-txt)))
                        (insert matched-s)))
              (t (save-excursion
                   (goto-char end)
                   (yank)
                   (goto-char beg)
                   (insert matched-s))))))
      (if m-d
          (destructuring-bind (matched-s dir) m-d
            (case dir
              ('right (yank)(insert matched-s))
              (t (insert matched-s)(yank))))
        (yank)))))

;; wrap-with-mode:

(defmacro w-w/with-term-funcs (&rest body)
  `(let ((t-ch-m))
     (when (eq major-mode 'term-mode)
       (setq t-ch-m (term-in-char-mode))
       (term-line-mode))
     (let ((ret (progn ,@body)))
       (when t-ch-m
         (term-char-mode))
       ret)))

(defun w-w/define-key (key body)
  (declare (indent defun))
  ;; `(define-key wrap-with-mode-map ,key
  ;;    #'(lambda () (interactive) (w-w/with-term-funcs ,@body)))
  (let ((fun-name (intern (concat "w-w/insert-for-" key)))
        (key-seq (read-kbd-macro key)))
    (eval
     `(defun ,fun-name ()
        (interactive)
        (w-w/with-term-funcs ,body)))
    (define-key wrap-with-mode-map key-seq fun-name)))

(defvar wrap-with-mode-map (make-sparse-keymap)
  "Keymap for wrap-with-mode.")
(mapc #'(lambda (p)
          (let ((op (car p))
                (cl (cdr p)))
            (when op
              (w-w/define-key op
                `(w-w/wrap-with-pair ,op (w-w/wrap-beg-end) ,cl)))
            (when cl
              (w-w/define-key cl
                `(w-w/wrap-with-pair ,cl)))))
      '(("(" . ")")
        ("[" . "]")
        ;;("<" . ">")
        ("{" . "}")
        ("\"" . nil)))

;;(define-key wrap-with-mode-map (kbd "C-c w w") #'w-w/wrap-with)
;;(define-key wrap-with-mode-map (kbd "C-c w p") #'w-w/wrap-with-pair)
;;(define-key wrap-with-mode-map (kbd "C-c w h t") #'w-w/wrap-with-html-tag)
(define-key wrap-with-mode-map (kbd "M-DEL") #'w-w/backspace-current-pair)
(define-key wrap-with-mode-map (kbd "C-s-w") #'w-w/cut-region-with-surronding-pair)
(define-key wrap-with-mode-map (kbd "C-s-y") #'w-w/insert-with-surrounding-pair)
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
