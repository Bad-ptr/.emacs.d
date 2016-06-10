;;; 2-user-funcs.el --- different useful functions. -*- lexical-binding: t; -*-


;;; Code:

(defun my/-create-dir-for-file ()
  (let ((dir (file-name-directory buffer-file-name)))
    (unless (file-directory-p dir)
      (when (y-or-n-p-with-timeout
             (format "No such directory: %s. Do you want to create it?" dir)
             10 t)
        (make-directory dir t)))))
(add-hook 'find-file-not-found-functions #'my/-create-dir-for-file)


(defun move-cfile (dirname)
  (interactive "Dmove to: ")
  (let ( (fname (file-name-nondirectory (buffer-file-name)))
         (pnt (point)) )
    (rename-file (buffer-file-name) (concat dirname fname))
    (kill-buffer (current-buffer))
    (find-file (concat dirname fname))
    (goto-char pnt)))


(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  ;;(let ((m-m major-mode))
    ;;(fundamental-mode)
    (untabify-buffer)
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8)
    ;;(funcall m-m)
    ;;)
  )

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun toggle-mode-line-to header () "toggles the modeline to header"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (progn
          (setq header-line-format (default-value 'header-line-format))
          (default-value 'mode-line-format))
      (let ((hlf header-line-format))
        (setq header-line-format mode-line-format)
      hlf)))
  (redraw-display))


;; convert buffer with codepage
(defun recode-buffer-dangerous (target-coding-system)
  "* Recode buffer as if it were encoded with `target-coding-system'.
If current buffer is write-protected (`buffer-read-only'), temporarily toggle
read-only flag, recode, then turn it back."
  (interactive "zEnter target coding system: ")
  (cl-labels ((do-recode nil
                         (encode-coding-region (point-min)
                                               (point-max)
                                               buffer-file-coding-system)
                         (decode-coding-region (point-min)
                                               (point-max)
                                               target-coding-system)
                         (set-buffer-file-coding-system target-coding-system)))
    (if buffer-read-only
        (let ((buffer-read-only nil))
          (do-recode)
          (set-buffer-modified-p nil))
      (do-recode))))

(defun recode-buffer-safe (target-coding-system)
  "* Recode buffer as if it were encoded with `target-coding-system'.
If current buffer is write-protected (`buffer-read-only'), do nothing."
  (interactive "zEnter target coding system: ")
  (unless buffer-read-only
    (encode-coding-region (point-min)
                          (point-max)
                          buffer-file-coding-system)
    (decode-coding-region (point-min)
                          (point-max)
                          target-coding-system)
    (set-buffer-file-coding-system target-coding-system)))


;; 2-user-funcs.el ends here
