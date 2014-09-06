;;; 4-programming.el --- settings for programming modes


;;; Code:


;; Font for parens

(defface open-paren-face
  '((default :inherit default :height 0.9)
    (((class color) (min-colors 88) (background light))
     (:foreground "#DDD"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "#222")))
  "Face for opening parentheses.")

(defface close-paren-face
  '((default :inherit default :slant italic)
    (((class color) (min-colors 88) (background light))
     (:foreground "#888"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "#888")))
  "Face for close parentheses.")

(add-hook 'prog-mode-hook
          #'(lambda ()
              (font-lock-add-keywords nil '(("(" . 'open-paren-face)) 'append)
              (font-lock-add-keywords nil '((")" . 'close-paren-face)))) 'append)

;; ---------------------
;; Common

(defun run-current-file ()
  "Execute or compile the current file. For example,
 if the current buffer is the file x.pl, then it'll call “perl x.pl” in a shell.
 The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
 File suffix is used to determine what program to run."
  (interactive)
  (let (suffixMap fname suffix progName cmdStr) ;; a keyed list of file suffix to comand-line program path/name
    (setq suffixMap '( ("php" . "php")
                       ("pl" . "perl")
                       ("py" . "python")
                       ("rb" . "ruby")
                       ("js" . "js")
                       ("sh" . "bash")
                       ("ml" . "ocaml")
                       ("vbs" . "cscript")
                       ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640") ) )
    (setq fname (buffer-file-name))
    (setq suffix (file-name-extension fname))
    (setq progName (cdr (assoc suffix suffixMap)))
    (setq cmdStr (concat progName " \"" fname "\""))
    (if (string-equal suffix "el") ; special case for emacs lisp
        (load-file fname)
      (if progName
          (progn (message "Running...")
                 (shell-command cmdStr "*run-current-file output*" ) )
        (message "No recognized program file suffix for this file.") ) )))


(defun* get-closest-pathname (&optional (file "Makefile") (maxlevel 3))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/")) ; the win32 builds should translate this correctly
        (level 0))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       do (setq level (+ level 1))
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (> level maxlevel)
                       return nil
                       if (equal d root)
                       return nil))))

(require 'compile)
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (when buffer-file-name
                     (let ((file (file-name-nondirectory buffer-file-name))
                           (mkfile (get-closest-pathname)))
                       (if mkfile
                           (progn (format "cd %s; make" (file-name-directory mkfile)))
                         (format "%s -c -o %s.o %s %s %s"
                                 (or (getenv "CC") "gcc")
                                 (file-name-sans-extension file)
                                 (or (getenv "CPPFLAGS") "-DDEBUG=9")
                                 (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                                 file))))))))


;; make file executable if it's a script
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Autoindent code after pasting
(defadvice yank-pop (after indent-region activate)
  (if (or (member major-mode '(emacs-lisp-mode
                               scheme-mode lisp-mode
                               c-mode c++-mode objc-mode
                               latex-mode plain-tex-mode
                               php-mode nxml-mode
                               ruby-mode python-mode
                               lua-mode))
          (derived-mode-p 'prog-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; ----------------


;; gdb
(setq gdb-many-windows t
      gdb-show-main t)


;; C
(setq c-default-style "gnu")

(defun c-get-system-includes ()
  (with-temp-buffer
    (shell-command 
     "echo | cpp -x c++ -Wp,-v 2>&1 | grep '^ .*include' | sed 's/^ //g'" 
     (current-buffer))
    (split-string (buffer-string) "\n" t)))


;; lisp modes
(dolist (hook (list
               'emacs-lisp-mode-hook
               'lisp-mode-hook
               'lisp-interaction-mode-hook
               'ielm-mode-hook))
  (add-hook hook '(lambda ()
                    (eldoc-mode)
                    ;; (font-lock-add-keywords nil '(("\\((\\|)\\)" 1 font-lock-comment-face append)))
                    )))


;; go-mode
(with-eval-after-load "go-mode-autoloads"
  (setq-default gofmt-command "goimports")
  (with-eval-after-load 'company-autoloads
    (require 'company-go nil t))
  (with-eval-after-load "go-eldoc-autoloads"
    (add-hook 'go-mode-hook #'go-eldoc-setup)))


;; python
(with-eval-after-load "company-jedi-autoloads"
  (require 'company-jedi)
  ;;(setq company-jedi-command (format "python3 -m start_jedi -p %s" company-jedi-port))
  ;;(setq company-jedi-show-eldoc-as-single-line t)
  (add-hook 'python-mode-hook #'company-jedi-start)
  ;;(add-hook 'python-mode-hook #'company-jedi-eldoc-setup)
  (with-eval-after-load "company-autoloads"
    (add-to-list 'company-backends 'company-jedi)
    (setq company-backends (delete 'company-ropemacs company-backends)))
  (with-eval-after-load "python"
    (define-key python-mode-map (kbd ".") 'company-jedi-complete-on-dot)
    (define-key python-mode-map (kbd "<C-tab>") 'company-jedi)
    (define-key python-mode-map (kbd "M-?") 'company-jedi-show-doc)
    (define-key python-mode-map (kbd "M-r") 'company-jedi-find-references)
    (define-key python-mode-map (kbd "M-.") 'company-jedi-goto-definition)
    (define-key python-mode-map (kbd "M-,") 'pop-tag-mark)))
;; (with-eval-after-load "jedi-autoloads"
;;   (add-hook 'python-mode-hook #'jedi:setup))


;; ocaml
(with-eval-after-load 'caml-autoloads
  (load "ocaml"))


;; cperl
(with-eval-after-load "cperl-mode-autoloads"
  (defalias 'perl-mode 'cperl-mode)
  (setq cperl-hairy t)
  (add-hook 'cperl-mode-hook 'eldoc-mode))


;; php
(with-eval-after-load "php-mode-autoloads"
  (setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))
  (add-hook 'before-save-hook #'(lambda ()
                                  (if (string= "php-mode" major-mode)
                                      (save-excursion
                                        (goto-char (point-min))
                                        (delete-blank-lines)
                                        (goto-char (point-max))
                                        (delete-blank-lines))))))


;; haskell
(with-eval-after-load "haskell-mode-autoloads"
  (setq auto-mode-alist (append '(("\\.hs$" . haskell-mode)) auto-mode-alist))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))


;; C#
(with-eval-after-load "csharp-mode-autoloads"
  (setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
  (add-hook 'csharp-mode-hook #'(lambda ()
                                  (omnisharp-mode t)
                                  (add-to-list 'company-backends 'company-omnisharp))))


;; vala
(with-eval-after-load "vala-mode-autoloads"
  (setq auto-mode-alist (append '(("/*.\.vala$" . vala-mode)) auto-mode-alist))
  (setq auto-mode-alist (append '(("/*.\.vapi$" . vala-mode)) auto-mode-alist))
  (setq file-coding-system-alist (append '(("/*.\.vala$" . utf-8)) file-coding-system-alist))
  (setq file-coding-system-alist (append '(("/*.\.vapi$" . utf-8)) file-coding-system-alist)))

;; SQL
(with-eval-after-load "sqlup-mode-autoloads"
  (add-hook 'sql-mode-hook #'(lambda () (sqlup-mode t))))


;; 4-programming.el ends here
