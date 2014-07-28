;;; 3-packages.el --- setting up packages


;;; Code:


(require 'generic-x)
(require 'newcomment)


(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun my/-install-favourite-packages ()
  (dolist (pkg my/-favourite-packages-list)
    (require-package pkg)))

;; Install my packages at first run
(unless (file-exists-p package-user-dir)
  (my/-install-favourite-packages))


(defun fbread-mode()
  (interactive)
  (sgml-mode)
  (sgml-tags-invisible 0)
  (longlines-mode)
  (view-mode))
(add-to-list 'auto-mode-alist '("\\.fb2$" . fbread-mode))


(add-hook 'tabulated-list-mode-hook #'(lambda () (hl-line-mode 1)))

;; templates
(with-eval-after-load "template"
  (add-to-list 'template-default-directories
               (expand-file-name "site-lisp/templates/" user-emacs-directory))
  (setq template-auto-insert t
        template-auto-update nil)
  (add-to-list 'template-expansion-alist
               '("USER_MAIL" (insert user-mail-address)))
  (add-to-list 'template-expansion-alist
               '("USER_NICKNAME" (insert user-nickname)))
  (template-initialize))
(require 'template)


;; ido
(with-eval-after-load "ido"
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-default-file-method 'selected-window
        ido-max-prospects 10)
  (ido-mode t)
  (ido-everywhere t))
;;(require 'ido)

(with-eval-after-load "ido-ubiquitous-autoloads"
  (ido-ubiquitous-mode t))
(with-eval-after-load "ido-at-point-autoloads"
  (ido-at-point-mode 1))



;; Tabbar
(with-eval-after-load "tabbar-autoloads"
  (tabbar-mode 1))

;; smex
(with-eval-after-load "smex-autoloads"
  (smex-initialize)
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "M-X") #'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") #'execute-extended-command)
  (smex-auto-update 60)
  (setq smex-save-file (expand-file-name "~/.emacs.d/.smex-items")))

;; drag-stuff
(with-eval-after-load "drag-stuff-autoloads"
  (drag-stuff-global-mode 1))

;; popwin
(with-eval-after-load "popwin-autoload"
  (setq display-buffer-function 'popwin:display-buffer))

;; golden-ratio
(with-eval-after-load "golden-ratio-autoloads"
  (golden-ratio-mode 1))

;;Jabber
(with-eval-after-load "jabber"
  (defun jabber-muc-join-group ()
    (interactive)
    (let ((account (jabber-read-account)))
      (jabber-get-bookmarks account
                            #'(lambda (jc bms)
                                (let (groups group nickname)
                                  (mapc #'(lambda (b)
                                            (when (eq 'conference (jabber-xml-node-name b))
                                              (push (make-symbol (jabber-xml-get-attribute b 'jid)) groups)))
                                        bms)
                                  (setq group (jabber-read-jid-completing "group: " groups)
                                        nickname (jabber-muc-read-my-nickname jc group))
                                  (jabber-muc-join jc group nickname)))))))

;; yasnippet
(with-eval-after-load "yasnippet-autoloads"
  (yas-global-mode)
  (add-hook 'term-mode-hook #'(lambda () (yas-minor-mode -1))))

;; uniquify
(with-eval-after-load "uniquify"
  (setq uniquify-buffer-name-style 'forward
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;;mark-multiple
(with-eval-after-load "multiple-cursors-autoloads"
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-.") #'mc/unmark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-,") #'mc/unmark-previous-like-this)
  (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
  (global-set-key (kbd "C-?") #'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-\"") #'mc/skip-to-previous-like-this)

  (global-set-key (kbd "C-;") #'mc/mark-all-like-this-dwim)
  (global-set-key (kbd "C-'") #'mc/mark-all-symbols-like-this-in-defun))

;; expand-region
(with-eval-after-load "expand-region-autoloads"
  (global-set-key (kbd "M-o") 'er/expand-region))

;; redo+
(with-eval-after-load "redo+-autoloads"
  (setq undo-no-redo t)
  (require 'redo+))

;; key-chord
(with-eval-after-load "key-chord-autoloads"
  (key-chord-mode 1)
  (key-chord-define-global "hj" #'undo)
  (key-chord-define-global "cv" #'reindent-then-newline-and-indent)
  (key-chord-define-global "4r" "$")

  (key-chord-define-global ";;" #'(lambda ()
                                    (interactive);
                                    (move-end-of-line 1)
                                    (if (looking-back ";")
                                        (reindent-then-newline-and-indent)
                                      (insert ";"))))
  (key-chord-define-global ",," #'(lambda () (interactive)(move-after-closing-bracket)(insert ", ")))

  (key-chord-define-global "qx" #'eval-region)

  (key-chord-define-global "tn" #'tabbar-forward-tab)
  (key-chord-define-global "tb" #'tabbar-backward-tab)
  (key-chord-define-global "gn" #'tabbar-forward-group)
  (key-chord-define-global "gb" #'tabbar-backward-group))

;; idle-highlight-mode
(with-eval-after-load "idle-highlight-mode-autoloads"
  (setq idle-highlight-delay 1.0)
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

;; highlight-tail-mode
(with-eval-after-load "highlight-tail-autoloads"
  (highlight-tail-mode 1))

;; rainbow-mode
(with-eval-after-load "rainbow-mode-autoloads"
  (add-hook 'prog-mode-hook
            ;; 'rainbow-turn-on
            #'(lambda () (rainbow-mode 1))))


;; highlight-parentheses
(with-eval-after-load "highlight-parentheses-autoloads"
  (setq hl-paren-background-colors '())
  (setq hl-paren-colors (list "#FF0000" "#BB7700" "#664400" nil))
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))

  (global-highlight-parentheses-mode t))

;; highlight-blocks-mode
;; do not enable it as it's too slow
;; (with-eval-after-load "highlight-blocks-autoloads"
;;   (add-hook 'prog-mode-hook #'(lambda () (highlight-blocks-mode 1)))
;;   (add-hook 'activate-mark-hook #'(lambda () (highlight-blocks-mode -1)))
;;   (add-hook 'deactivate-mark-hook #'(lambda () (highlight-blocks-mode 1))))

;; smart-modeline
(with-eval-after-load "smart-mode-line-autoloads"
  (sml/setup))

;; wrap-with
(with-eval-after-load "wrap-with"
  (wrap-with-mode t))
(require 'wrap-with)

;; company-mode https://gist.github.com/Bad-ptr/7787596
(with-eval-after-load "company-autoloads"
  (global-company-mode 1)

  (setq company-tooltip-limit 20
        company-minimum-prefix-length 1
        company-echo-delay 0
        company-begin-commands '(self-insert-command
                                 c-electric-lt-gt c-electric-colon
                                 completion-separator-self-insert-command)
        company-idle-delay 0.2
        company-show-numbers t)

  (defun company-elisp-minibuffer (command &optional arg &rest ignored)
    "`company-mode' completion back-end for Emacs Lisp in the minibuffer."
    (interactive (list 'interactive))
    (case command
      ('prefix (and (minibufferp)
                    (case company-minibuffer-mode
                      ('execute-extended-command (company-grab-symbol))
                      (t (company-capf `prefix)))))
      ('candidates
       (when (< (count-lines (point-min) (point-max)) 3)
         (save-excursion
           (goto-char (point-max))
           (insert "\n\n")))
       (case company-minibuffer-mode
         ('execute-extended-command (all-completions arg obarray 'commandp))
         (t nil)))
      ('post-completion (when (eq company-minibuffer-mode 'execute-extended-command)
                          (save-excursion
                            (goto-char (point-max))
                            (while (looking-back "\n")
                              (delete-char -1)))))))

  (defun minibuffer-company ()
    (unless company-mode
      (when (and global-company-mode (or (eq this-command #'execute-extended-command)
                                         (eq this-command #'eval-expression)))

        (setq-local company-minibuffer-mode this-command)

        (setq-local completion-at-point-functions '(lisp-completion-at-point t))

        (setq-local company-backends '((company-elisp-minibuffer company-capf)))
        (setq-local company-tooltip-limit 8)
        (setq-local company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                        company-preview-if-just-one-frontend))

        (company-mode 1)
        (when (eq this-command #'execute-extended-command)
          (company-complete)))))

  (add-hook 'minibuffer-setup-hook #'minibuffer-company)
  ;;(add-hook 'eval-expression-minibuffer-setup-hook #'minibuffer-company)
  )


;; flx
(with-eval-after-load "flx-ido-autoloads"
  (setq ido-use-faces nil)
  (flx-ido-mode 1))

;; ido-vertical
(with-eval-after-load "ido-vertical-mode-autoloads"
  (ido-vertical-mode 1))

;; speedbar
(with-eval-after-load "speedbar"
  (add-to-list 'speedbar-frame-parameters (cons 'persp-ignore-wconf t)))

;; persp-mode
(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil)
  (setq windmove-window-distance-delta 2)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

;; 3-packages.el ends here
