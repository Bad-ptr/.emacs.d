;;; 3-packages.el --- setting up packages


;;; Code:


(require 'generic-x)
(require 'newcomment)


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
               (locate-user-emacs-file "site-lisp/templates/"))
  (setq template-auto-insert t
        template-auto-update nil)
  (add-to-list 'template-expansion-alist
               '("USER_MAIL" (insert user-mail-address)))
  (add-to-list 'template-expansion-alist
               '("USER_NICKNAME" (insert user-nickname)))
  (template-initialize))
(require 'template)

;; skeletor
(with-eval-after-load "skeletor-autoloads"
  (skeletor-define-template "Cpp"))

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
(require 'ido)

(with-eval-after-load "ido-ubiquitous-autoloads"
  (ido-ubiquitous-mode t))
(with-eval-after-load "ido-at-point-autoloads"
  (ido-at-point-mode 1))


;; Tabbar
(with-eval-after-load "tabbar-autoloads"
  (tabbar-mode 1)
  (set-face-attribute
   'tabbar-separator nil
   :background "gray20"
   :height 0.6)
  (defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    (let ((label (if tabbar--buffer-show-groups
                     (format "[%s] " (tabbar-tab-tabset tab))
                   (format "%s " (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
          label
        (tabbar-shorten
         label (max 1 (/ (window-width)
                         (length (tabbar-view
                                  (tabbar-current-tabset))))))))))

;; smex
(if (>= emacs-major-version 24)
    (with-eval-after-load "smex-autoloads"
      (smex-initialize)
      (global-set-key (kbd "M-x") #'smex)
      (global-set-key (kbd "M-X") #'smex-major-mode-commands)
      ;; This is your old M-x.
      (global-set-key (kbd "C-c C-c M-x") #'execute-extended-command)
      (smex-auto-update 60)
      (setq smex-save-file (expand-file-name "~/.emacs.d/.smex-items")))
  (with-eval-after-load "ido"
    (global-set-key
     "\M-x" (lambda ()
              (interactive)
              (call-interactively
               (intern
                (ido-completing-read "M-x " (all-completions "" obarray 'commandp))))))))

;; drag-stuff
(with-eval-after-load "drag-stuff-autoloads"
  (drag-stuff-global-mode 1))

;; popwin
(with-eval-after-load "popwin-autoloads"
  (setq display-buffer-function 'popwin:display-buffer))

;; golden-ratio
(with-eval-after-load "golden-ratio-autoloads"
  (when (>= emacs-major-version 24)
    (golden-ratio-mode 1)))

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
  (global-set-key (kbd "C-:") #'mc/mark-all-symbols-like-this-in-defun)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  (with-eval-after-load "multiple-cursors"
    (define-key mc/keymap (kbd "C-'") 'mc-hide-unmatched-lines-mode)))

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
  ;;(key-chord-define-global ",," #'(lambda () (interactive)(move-after-closing-bracket)(insert ", ")))

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
(require 'wrap-with nil t)

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

  (defvar-local company-col-offset 0 "Horisontal tooltip offset.")
  (defvar-local company-row-offset 0 "Vertical tooltip offset.")

  (defun company--posn-col-row (posn)
    (let ((col (car (posn-col-row posn)))
          ;; `posn-col-row' doesn't work well with lines of different height.
          ;; `posn-actual-col-row' doesn't handle multiple-width characters.
          (row (cdr (posn-actual-col-row posn))))
      (when (and header-line-format (version< emacs-version "24.3.93.3"))
        ;; http://debbugs.gnu.org/18384
        (cl-decf row))
      (cons (+ col (window-hscroll) company-col-offset) (+ row company-row-offset))))

  (defun company-elisp-minibuffer (command &optional arg &rest ignored)
    "`company-mode' completion back-end for Emacs Lisp in the minibuffer."
    (interactive (list 'interactive))
    (case command
      ('prefix (and (minibufferp)
                    (case company-minibuffer-mode
                      ('execute-extended-command (company-grab-symbol))
                      (t (company-capf `prefix)))))
      ('candidates
       (case company-minibuffer-mode
         ('execute-extended-command (all-completions arg obarray 'commandp))
         (t nil)))))

  (defun minibuffer-company ()
    (unless company-mode
      (when (and global-company-mode (or (eq this-command #'execute-extended-command)
                                         (eq this-command #'eval-expression)))

        (setq-local company-minibuffer-mode this-command)

        (setq-local completion-at-point-functions '(lisp-completion-at-point t))

        (setq-local company-show-numbers nil)
        (setq-local company-backends '((company-elisp-minibuffer company-capf)))
        (setq-local company-tooltip-limit 8)
        (setq-local company-col-offset 1)
        (setq-local company-row-offset 1)
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
(if (>= emacs-major-version 24)
    (with-eval-after-load "ido-vertical-mode-autoloads"
      (ido-vertical-mode 1))
  (with-eval-after-load "ido"
    ;; Display ido results vertically, rather than horizontally
    (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
    (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
    (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)))

;; speedbar
(with-eval-after-load "speedbar"
  (add-to-list 'speedbar-frame-parameters (cons 'persp-ignore-wconf t)))

;; persp-mode
(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil)
  ;;(setq windmove-window-distance-delta 2)
  (unless (>= emacs-major-version 24)
    (setq persp-when-kill-switch-to-buffer-in-perspective nil))
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))


;; 3-packages.el ends here
