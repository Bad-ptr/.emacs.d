;;; company-go.el --- company-mode backend for Go (using gocode)

;; Copyright (C) 2012

;; Author: nsf <no.smile.face@gmail.com>
;; Keywords: languages

;; No license, this code is under public domain, do whatever you want.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'company))

(defun company-go--invoke-autocomplete ()
  (let ((temp-buffer (generate-new-buffer "*gocode*")))
    (prog2
        (call-process-region (point-min)
                             (point-max)
                             "gocode"
                             nil
                             temp-buffer
                             nil
                             "-f=csv"
                             "autocomplete"
                             (buffer-file-name)
                             (concat "c" (int-to-string (- (point) 1))))
        (with-current-buffer temp-buffer (buffer-string))
      (kill-buffer temp-buffer))))

(defun company-go--prefix ()
  (let ((symbol (company-grab-word)))
    (if symbol
        (if (save-excursion
              (forward-char (- (length symbol)))
              (looking-back "\\." (- (point) 1)))
            (cons symbol t)
          symbol)
      'stop)))

(defun company-go--get-candidates (strings)
  (mapcar (lambda (str)
            (let ((candidate (split-string str ",,")))
              (let ((class (nth 0 candidate))
                    (name (nth 1 candidate))
                    (type (nth 2 candidate))
                    (comp "") (out ""))
                (if (string-prefix-p "func" type)
                    (setq type (substring type 4)
                          comp (if (or (string= class "type") (string= class "var"))
                                   name
                                 (concat name type))
                          out (concat name " " type))
                  (setq type (concat " " type)
                        comp name
                        out (concat name " " class " " type)))
                (propertize (propertize out 'comp comp) 'meta (concat class " " name type))))) strings))

(defun company-go--candidates ()
  (company-go--get-candidates (split-string (company-go--invoke-autocomplete) "\n" t)))

(defun company-go-post (arg)
  (let ((comp (get-text-property 0 'comp arg)))
    (if comp
        (let* ((end (point-marker))
               (beg (- (point) (length arg)))
               (dif (- end beg)))
          (goto-char end)
          (backward-delete-char dif)
          (insert comp)
          (company-go-templatify comp)))))

(defun company-go-templatify (arg)
  (let ((substrn (string-match ")" arg)))
    (when substrn
      (let* ((string (substring arg 0 (1+ substrn)))
             (end (point-marker))
             (beg (- (point) (length arg)))
             (dif (- (string-width arg) (string-width string))))
        (goto-char end)
        (if (looking-at "(")
            (progn
              (goto-char beg)
              (search-forward "(" end 'move)
              (when (eq (char-before) ?\()
                (backward-char)
                (setq beg (point))
                (goto-char end)
                (backward-delete-char (- end beg))))
          (backward-delete-char dif)
          (goto-char beg)
          (when (search-forward "(" end 'move)
            (if (eq (char-after) ?\))
                (forward-char 1)
              (let ((templ (company-template-declare-template beg end)))
                (while (re-search-forward (concat " *\\([^,)]*\\)[,)]") end t)
                  (let ((sig (match-string 1))
                        (beg (match-beginning 1))
                        (end (match-end 1)))
                    (delete-region beg end)
                    (save-excursion
                      (let ((ttext (if (string-match-p "\\.\\.\\." sig)
                                       " "
                                     (substring sig 0 (or (string-match "\s-*" sig) (string-width sig))))))
                        (company-template-add-field templ beg ttext sig)))))
                (company-template-move-to-first templ)))))))))

(defun company-go (command &optional arg &rest ignored)
  (case command
    (prefix
     (and (eq major-mode 'go-mode)
          (not (company-in-string-or-comment))
          (company-go--prefix)))
    (candidates (company-go--candidates))
    (meta (get-text-property 0 'meta arg))
    (post-completion (company-go-post arg))
    (sorted nil)))

(add-to-list 'company-backends 'company-go)

(provide 'company-go)
;;; company-go.el ends here
