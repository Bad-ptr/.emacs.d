;;; 4-programming.el --- settings for programming modes. -*- lexical-binding: t; -*-


;;; Code:


;; -----------------------
;; Common

;; (electric-indent-mode 1)

(require 'compile)

;; make file executable if it's a script
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(defvar project-license "GPL either version 2 or any later version"
  "Default license for new files.")

(with-eval-after-load "template"
  (push '("LICENSE" (insert project-license)) template-expansion-alist))

(defvar my/-prog-mode-hook nil
  "Hook to be run on programming mode activation.")
(defvar my/-prog-mode-hooks (if (< emacs-major-version 24)
								'(emacs-lisp-mode-hook cperl-mode-hook c-mode-common-hook
													   lisp-mode-hook lisp-interaction-mode-hook
													   ielm-mode-hook)
							  '(prog-mode-hook cperl-mode-hook ielm-mode-hook
											   eval-expression-minibuffer-setup-hook))
  "List of programming mode hooks.")

;; Common hook place for programming modes
(dolist (hook my/-prog-mode-hooks)
  (add-hook hook #'(lambda () (run-hooks 'my/-prog-mode-hook))))

(unless (version< emacs-version "24.4")
  (global-prettify-symbols-mode 1))

(add-hook
 'my/-prog-mode-hook
 #'(lambda ()
     (eldoc-mode)

     (setq show-trailing-whitespace t)

     (font-lock-add-keywords
      nil '(("("   . 'open-paren-face)   (")" . 'close-paren-face)
            ("{"   . 'open-paren-face)   ("}" . 'close-paren-face)
            ("\\[" . 'open-paren-face) ("\\]" . 'close-paren-face)

            ("->" . 'access-op-face) ("::" . 'access-op-face) ("\\." . 'access-op-face)
            ("," . 'comma-semicolon-face) (";" . 'comma-semicolon-face)

            ;; use the hl-todo package
            ;; ("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)

            ("--_--.*?--_--" 0 'underline prepend)
            ("--/--.*?--/--" 0 'overstriked-face prepend))
      'append)

     (unless (file-exists-p "Makefile")
       (setq-local compile-command
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@
                   (when buffer-file-name
                     (let ((file (file-name-nondirectory buffer-file-name))
                           (mkfile (get-closest-pathname)))
                       (if mkfile
                           (progn (format "cd %s; make" (file-name-directory mkfile)))
                         (format "%s %s %s -c %s -o %s.o"
                                 (or (getenv "CC") "gcc")
                                 (or (getenv "CPPFLAGS") "-DDEBUG=9")
                                 (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                                 file
                                 (file-name-sans-extension file)))))))))

(with-eval-after-load "clean-aindent-mode-autoloads"
  (add-hook 'my/-prog-mode-hook #'clean-aindent-mode))

(with-eval-after-load "dtrt-indent-autoloads"
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

(with-eval-after-load "ws-butler-autoloads"
  ;;(add-hook 'my/-prog-mode-hook #'ws-butler-mode)
  (add-hook 'markdown-mode-hook #'(lambda () (ws-butler-mode -1)))
  (ws-butler-global-mode))


;; Fonts for parens and ops

(defface open-paren-face
  '((default :inherit default :height 0.9)
    (((class color) (min-colors 88) (background light))
     (:foreground "#DDD"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "#444")))
  "Face for opening parentheses.")

(defface close-paren-face
  '((default :inherit default :slant italic)
    (((class color) (min-colors 88) (background light))
     (:foreground "#888"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "#888")))
  "Face for close parentheses.")

(defface access-op-face
  '((default :inherit open-paren-face)
    (((class color) (min-colors 88) (background light))
     (:foreground "#666"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "#AAA")))
  "Face for accessor operators.")

(defface comma-semicolon-face
  '((default :inherit close-paren-face :height 0.9 :slant normal))
  "Face for comma and semicolon")

(defface overstriked-face
  '((t :strike-through "#E11"))
  "Face with overstrike.")


(with-eval-after-load "dash"
  (defun my/-renormalize-faces ()
    (interactive)
    (let* ((c-bg (color-name-to-rgb (frame-parameter (selected-frame) 'background-color)))
           (c-op (color-name-to-rgb (face-foreground 'open-paren-face)))
           (c-cp (color-name-to-rgb (face-foreground 'close-paren-face)))
           (bg-mode (frame-parameter (selected-frame) 'background-mode))
           (dif-o-c-p (-zip-with #'- c-op c-cp)))
      ;;(message "%s - %s = %s" c-op c-cp dif-o-c-p)
      (setq c-op (-zip-with #'- c-bg dif-o-c-p)
            c-cp (-zip-with #'- c-op dif-o-c-p))
      ;;(message "%s - %s = %s" c-bg dif-o-c-p c-op)
      (set-face-foreground 'open-paren-face (apply #'color-rgb-to-hex c-op))
      (set-face-foreground 'close-paren-face (apply #'color-rgb-to-hex c-cp)))))


(cl-defun get-closest-pathname (&optional (file "Makefile") (maxlevel 3))
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

(defun run-current-file ()
  "Execute or compile the current file. For example,
 if the current buffer is the file x.pl, then it'll call “perl x.pl” in a shell.
 The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
 File suffix is used to determine what program to run."
  (interactive)
  (let (suffixMap fname suffix progName cmdStr) ;; a keyed list of file suffix to comand-line program path/name
    (setq suffixMap '(("php" . "php")
                      ("pl" . "perl")
                      ("py" . "python")
                      ("rb" . "ruby")
                      ("js" . "js")
                      ("sh" . "bash")
                      ("ml" . "ocaml")
                      ("vbs" . "cscript")
                      ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")))
    (setq fname (buffer-file-name)
          suffix (file-name-extension fname)
          progName (cdr (assoc suffix suffixMap))
          cmdStr (concat progName " \"" fname "\""))
    (if (string-equal suffix "el") ; special case for emacs lisp
        (load-file fname)
      (if progName
          (progn (message "Running...")
                 (shell-command cmdStr "*run-current-file output*"))
        (message "No recognized program file suffix for this file.")))))

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



;; (unless (version< emacs-version "24.1")
;;   (add-hook 'grep-mode-hook
;;             (lambda nil
;;               (setq display-buffer-alist
;;                     (list
;;                      (cons ".*"
;;                            (cons
;;                             (lambda (bufer alist)
;;                               (when (eq (selected-window) (previous-window))
;;                                 (split-window-horizontally))
;;                               (with-selected-window
;;                                   (previous-window)
;;                                 (switch-to-buffer bufer)
;;                                 (selected-window)))
;;                             nil)))))))


;; ----------------


;; cedet
;;(load "~/projects/cedet/cedet-devel-load.el")
;;(global-ede-mode t)
;; (require 'semantic/bovine/c)
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file
;;     "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h")
;; Example ede project:
;; (ede-cpp-root-project "Test"
;;                       :name "Test"
;;                       :file "~/projects/Test/Makefile"
;;                       :include-path '("/inc"))

;; gdb
(setq gdb-many-windows t
      gdb-show-main t)


;; C/Cpp/C++
(setq c-default-style "gnu")

(defvar my/-c-include-paths (cons "."
                                  (when (executable-find "cpp")
                                    (with-temp-buffer
                                      (shell-command
                                       "echo | cpp -x c++ -Wp,-v 2>&1 | grep '^ .*include' | sed 's/^ //g'"
                                       (current-buffer))
                                      (split-string (buffer-string) "\n" t))))
  "List of dirs with includes for c.")
(defun my/-c-get-includes ()
  (concatenate 'list
               (when (and global-ede-mode ede-object)
                 (append
                  (ede-system-include-path ede-object)
                  (when ede-object-project
                    (mapcar #'(lambda (s)
                                (when (string-prefix-p "/" s)
                                  ;;(ede-project-root-directory ede-object-project)
                                  (ede-expand-filename ede-object
                                                       (concat "." s)
                                                       ;;(substring s 1)
                                                       )))
                            (oref ede-object-project include-path)))))
               (when semantic-mode
                 semantic-dependency-system-include-path)
               my/-c-include-paths))

(with-eval-after-load "ffap"
  (defun ffap-c-mode (name)
    (ffap-locate-file name t (my/-c-get-includes)))
  (defalias 'ffap-c++-mode 'ffap-c-mode))

(with-eval-after-load "company-c-headers-autoloads"
  (setq company-c-headers-path-system #'my/-c-get-includes)
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (add-to-list 'company-backends 'company-c-headers)))))

(with-eval-after-load "c-eldoc-autoloads"
  (setq c-eldoc-buffer-regenerate-time 120)
  (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook #'c-turn-on-eldoc-mode))

;; rust
(with-eval-after-load "racer-autoloads"
  ;; (setq racer-cmd "~/.cargo/bin/racer"
  ;;       racer-rust-src-path "/usr/local/src/rust/src/")
  (add-hook 'rust-mode-hook #'racer-mode))

;; lisp
(let ((q-s-h-file (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p q-s-h-file)
    (load q-s-h-file)))
(setq inferior-lisp-program "sbcl")


;; go-mode
(with-eval-after-load "go-mode-autoloads"
  (when (executable-find "goimports")
    (setq-default gofmt-command "goimports"))
  (with-eval-after-load "company-autoloads"
    (require 'company-go nil t)
    (with-eval-after-load "company-go"
      (add-hook 'go-mode-hook
                #'(lambda ()
                    (add-to-list 'company-backends 'company-go)))))
  (with-eval-after-load "go-eldoc-autoloads"
    (add-hook 'go-mode-hook #'go-eldoc-setup)))


;; python
(with-eval-after-load "company-jedi-autoloads"
  (require 'company-jedi)
  ;;(setq company-jedi-command (format "python3 -m start_jedi -p %s" company-jedi-port))
  ;;(setq company-jedi-show-eldoc-as-single-line t)
  ;;(add-hook 'python-mode-hook #'company-jedi-start)
  ;;(add-hook 'python-mode-hook #'company-jedi-eldoc-setup)
  (with-eval-after-load "company-autoloads"
    ;;(add-to-list 'company-backends 'company-jedi)
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
(with-eval-after-load "caml-autoloads"
  (load "ocaml"))


;; perl
(defun perl-fname-to-package (fname &optional lib n-parts)
  (unless n-parts (setq n-parts 2))
  (or
   (when fname
     (let* ((fname-parts (split-string fname "[/\\]"))
            (lib-pos
             (and lib
                  (search (list lib) fname-parts :test #'equal :from-end t))))
       (if lib-pos
           (setq fname-parts (nthcdr (1+ lib-pos) fname-parts))
         (let* ((fn-l (length fname-parts))
                (d (- fn-l n-parts)))
           (when (> d 0)
             (setq fname-parts (nthcdr d fname-parts)))))
       (let* ((ret (mapconcat #'identity fname-parts "::"))
              (s-m (string-match "\\..*$" ret)))
         (if s-m
             (substring ret 0 s-m)
           ret))))
   ""))

(with-eval-after-load "template"
  (push '("PERL_PACKAGE_NAME"
          (insert (perl-fname-to-package (concat (nth 0 template-file) (nth 1 template-file)) "lib")))
        template-expansion-alist)
  (push '("PERL_VERSION"
          (insert (or (and (boundp 'perl-version) perl-version) "5.018")))
        template-expansion-alist))

(with-eval-after-load "cperl-mode"
  (setq cperl-hairy t
        cperl-lazy-help-time 1)

  (add-hook 'cperl-mode-hook 'cperl-lazy-install)

  (define-key cperl-mode-map (kbd "<tab>")
    #'(lambda ()
        (interactive)
        (if (region-active-p)
            (indent-region (region-beginning) (region-end))
          (cperl-indent-command))))

  (autoload 'perl-repl "inf-perl" "Run perl repl." t nil)

  (defun cperl-list-imenu-functions (&optional one-line)
    (interactive "P")
    (let ((bufname "*imenu-function-list*"))
      (let* ((functions
              (delete-if-not #'identity
                             (mapcar
                              #'(lambda (cc)
                                  (let ((fname (car cc)))
                                    (unless (string-prefix-p "package" fname)
                                      (when (string-match "^\\(?:.+::\\)?\\(\\(?:\\\w\\|\\s_\\)+\\)" fname)
                                        (match-string-no-properties 1 fname)))))
                              (cdr (second (imenu--make-index-alist))))))
             (text (mapconcat #'identity functions (if one-line " " "\n"))))
        (with-current-buffer (get-buffer-create bufname)
          (erase-buffer)
          (insert text))
        (pop-to-buffer bufname))))
  (defalias 'perl-mode 'cperl-mode))


;; haskell
(with-eval-after-load "haskell-mode-autoloads"
  (push '("\\.hs$" . haskell-mode) auto-mode-alist)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  (with-eval-after-load "shm"
    (set-face-background 'shm-current-face "#eee8d5")
    (set-face-background 'shm-quarantine-face "lemonchiffon")))

;; erlang/elexir
(with-eval-after-load "alchemist-autoloads"
  (defvar erlang-include-path
    (when (executable-find "erl")
      (with-temp-buffer
        (shell-command
         "erl -eval 'io:format(\"~s\", [lists:concat([code:root_dir(), \"/erts-\", erlang:system_info(version), \"/include\"])])' -s init stop -noshell"
         (current-buffer))
        (buffer-string))))
  (push erlang-include-path my/-c-include-paths)
  (with-eval-after-load "company-clang"
    (push (concat "-I" erlang-include-path) company-clang-arguments))
  (with-eval-after-load "c-eldoc"
    (setq c-eldoc-includes (concat "-I" erlang-include-path " " c-eldoc-includes)))
  (with-eval-after-load "ffap"
    (push erlang-include-path ffap-c-path)
    (push erlang-include-path ffap-c++-path))
  (setq alchemist-goto-erlang-source-dir (expand-file-name "~/projects/erlang-otp")
        alchemist-goto-elixir-source-dir (expand-file-name "~/projects/elixir_stuff/elixir"))
  (add-hook 'elixir-mode-hook #'alchemist-mode)

  (with-eval-after-load "smartparens-autoloads"
    (with-eval-after-load "smartparens"
      (defun my-elixir-do-end-close-action (id action context)
        (when (and (eq action 'insert)
                   (not (looking-back "fn\s+?")))
          (cancel-timer my/-double-key-timer)
          (newline-and-indent)
          (newline-and-indent)
          (forward-line -1)
          (indent-according-to-mode)))

      (sp-with-modes '(elixir-mode)
					 (sp-local-pair "fn" "end"
									:when '(("SPC" "RET"))
									:post-handlers '(:add my-elixir-do-end-close-action)
									:actions '(insert))
					 (sp-local-pair "do" "end"
									:when '(("SPC" "RET"))
									:post-handlers '(:add my-elixir-do-end-close-action)
									:actions '(insert))))
    (add-hook 'elixir-mode-hook #'turn-on-smartparens-mode)))


;; C#
(with-eval-after-load "csharp-mode-autoloads"
  (push '("\\.cs$" . csharp-mode) auto-mode-alist)
  ;; (add-hook 'csharp-mode-hook #'(lambda ()
  ;;                                 (omnisharp-mode t)
  ;;                                 (add-to-list 'company-backends 'company-omnisharp)))
  )


;; vala
(with-eval-after-load "vala-mode-autoloads"
  (push '("/*.\.vala$" . vala-mode) auto-mode-alist)
  (push '("/*.\.vapi$" . utf-8) file-coding-system-alist))


;; SQL
(with-eval-after-load "sqlup-mode-autoloads"
  (add-hook 'sql-mode-hook #'(lambda () (sqlup-mode t))))

;; php
(with-eval-after-load "php-mode-autoloads"
  (push '("/*.\.php[0-9]?$" . php-mode) auto-mode-alist)
  (add-hook 'before-save-hook #'(lambda ()
                                  (when (eq 'php-mode major-mode)
                                    (cleanup-buffer)))))

;; JS
(with-eval-after-load "tern-autoloads"
  (add-hook 'js-mode-hook #'(lambda () (tern-mode t)))
  (with-eval-after-load "company-tern-autoloads"
    (add-to-list 'company-backends 'company-tern)))
(with-eval-after-load "js2-mode-autoloads"
  (defalias 'js-mode 'js2-mode))

;; HTML
(with-eval-after-load "simplezen-autoloads"
  (require 'simplezen)
  (add-hook-that-fire-once 'html-mode-hook ()
    (define-key html-mode-map (kbd "TAB") 'simplezen-expand-or-indent-for-tab)))


;; 4-programming.el ends here
