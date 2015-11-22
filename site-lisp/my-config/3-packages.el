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
  (setq template-expansion-alist
        (append '(("USER_MAIL"     (insert user-mail-address))
                  ("USER_NICKNAME" (insert user-nickname)))
                template-expansion-alist))
  (template-initialize)
  (defun template-expand-to-string-for-virtual-file (file &optional template)
    (flet ((template-make-directory (dir) dir))
      (unless template
        (setq template (cdr (template-derivation file t)))))
    (with-temp-buffer
      (let (find-file-hooks)
        (flet ((buffer-file-name (&optional buffer) file))
          (template-new-file nil template)))
      (buffer-substring (point-min) (point-max)))))
(require 'template)

;; skeletor
(with-eval-after-load "skeletor-autoloads"

  (defun skeletor-project-license-type ()
    (substring skeletor-project-license
               (1+ (search "/" skeletor-project-license :from-end t))))

  (defun skeletor--ctor-runtime-spec (spec)
    "Concatenate the given macro SPEC with values evaluated at runtime."
    (let* ((project-name (skeletor--read-project-name))
           (dest (f-join skeletor-project-directory project-name)))
      (let-alist spec
        (setq skeletor--current-project-root dest
              skeletor-project-root dest
              skeletor-project-name project-name
              skeletor-project-license (when .create-license?
                                         (skeletor--read-license "License: " .license-file-name))
              skeletor-project-spec (-concat
                                     (list
                                      (cons 'project-name project-name)
                                      (cons 'project-dir skeletor-project-directory)
                                      (cons 'dest dest)
                                      (cons 'skeleton (skeletor--get-named-skeleton .name))
                                      (cons 'license-file skeletor-project-license))
                                     spec))
        (setq skeletor-project-spec
              (-concat
               (list
                (cons 'repls (-map 'skeletor--eval-substitution
                                   (-concat
                                    skeletor-global-substitutions
                                    (list (cons "__PROJECT-NAME__" project-name)
                                          (cons "__LICENSE-FILE-NAME__" .license-file-name))
                                    .substitutions))))
               skeletor-project-spec)))))

  (skeletor-define-template "Cpp"
    :substitutions
    (list
     (cons "__MAINCPP__"
           #'(lambda ()
               (if (fboundp 'template-expand-to-string-for-virtual-file)
                   (let ((project-license (skeletor-project-license-type)))
                     (template-expand-to-string-for-virtual-file
                      (concat skeletor--current-project-root  "/main.cpp")))
                 "#include <iostream>
int main (int argc, char **argv) {
  std::cout << \"Hello World\" << std::endl;
  exit(0);
}")))))

  (add-to-list 'skeletor-global-substitutions
               (cons "__DATE-TIME__" #'(lambda () (format-time-string "%d/%m/%Y %H:%M"))))
  (add-to-list 'skeletor-global-substitutions
               (cons "__USER-NICKNAME__" #'(lambda () user-nickname)))
  (add-to-list 'skeletor-global-substitutions
               (cons "__LICENSE__" #'skeletor-project-license-type)))


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
    (golden-ratio-mode 1))
  ;; (unless (fboundp 'window--resizable-p)
  ;;   (defun window--resizable-p (window &rest args)
  ;;     (with-selected-window window
  ;;       (not (minibuffer-selected-window)))))
  )

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
  (defun my/-er/mark-outside-inside-pairs ()
    (if (er--looking-at-pair)
        (er/mark-outside-pairs)
      (er/mark-inside-pairs)))
  (setq er/try-expand-list
        '(er/mark-word
          er/mark-symbol
          er/mark-symbol-with-prefix
          er/mark-next-accessor
          er/mark-method-call
          er/mark-inside-quotes
          er/mark-outside-quotes
          my/-er/mark-outside-inside-pairs
          er/mark-comment
          er/mark-url
          er/mark-email
          er/mark-defun))
  (global-set-key (kbd "M-o") 'er/expand-region))

;; redo+
(with-eval-after-load "redo+-autoloads"
  (setq undo-no-redo t)
  (require 'redo+))

;; key-chord
(with-eval-after-load "key-chord-autoloads"
  (key-chord-mode 1)
  (key-chord-define-global "4r" "$")

  (key-chord-define-global ";;" #'(lambda ()
                                    (interactive)
                                    (move-end-of-line 1)
                                    (if (looking-back ";")
                                        (reindent-then-newline-and-indent)
                                      (insert ";"))))

  (key-chord-define-global "qx" #'eval-region)

  (key-chord-define-global "t4" #'tabbar-backward-tab)
  (key-chord-define-global "t5" #'tabbar-forward-tab)
  ;; (key-chord-define-global "gn" #'tabbar-forward-group)
  ;; (key-chord-define-global "gb" #'tabbar-backward-group)

  (dolist (chords '(("((" . "))") ("[[" . "]]") ("{{" . "}}")
                    (nil . ",,")))
    (let ((chord-left (car chords))
          (chord-right (cdr chords)))
      (lexical-let ((chord-l-s (and chord-left (substring chord-left 0 1)))
                    (chord-r-s (and chord-right (substring chord-right 0 1))))
        (when chord-left
          (key-chord-define-global chord-left #'(lambda ()
                                                  (interactive)
                                                  (search-backward chord-l-s))))
        (when chord-right
          (key-chord-define-global chord-right #'(lambda ()
                                                   (interactive)
                                                   (search-forward chord-r-s))))))))

;; idle-highlight-mode
(with-eval-after-load "idle-highlight-mode-autoloads"
  (setq idle-highlight-delay 1.0)
  (add-hook 'my/-prog-mode-hook 'idle-highlight-mode))

;; highlight-tail-mode
(with-eval-after-load "highlight-tail-autoloads"
  (highlight-tail-mode 1)
  (add-hook 'my/-find-large-file-hook #'(lambda () (highlight-tail-mode -1))))

;; beacon-mode
(with-eval-after-load "beacon-autoloads"
  (beacon-mode 1))

;; rainbow-mode
(with-eval-after-load "rainbow-mode-autoloads"
  (add-hook 'my/-prog-mode-hook #'(lambda () (rainbow-mode 1)))
  (add-hook 'my/-find-large-file-hook #'(lambda () (rainbow-mode -1))))


;; highlight-parentheses
(with-eval-after-load "highlight-parentheses-autoloads"
  (setq hl-paren-background-colors '("#FFF" "#DDCCDD" "#CCDDDD"))
  (setq hl-paren-background-colors-dark '("#333" "#444" "#555"))
  (setq hl-paren-colors (list "#FF0000" "#FF00FF" "#00FFFF"))
  (setq hl-paren-colors-dark (list "#FF0000" "#FF00FF" "#00FFFF"))
  (setq hl-paren-sizes (list 1.1))
  (global-highlight-parentheses-mode t)
  (defun hl-paren-create-overlays ()
    (let ((fg (if (eq (frame-parameter (selected-frame) 'background-mode)
                      'light)
                  hl-paren-colors
                hl-paren-colors-dark))
          (bg (if (eq (frame-parameter (selected-frame) 'background-mode)
                      'light)
                  hl-paren-background-colors
                hl-paren-background-colors-dark))
          (size hl-paren-sizes)
          attributes)
      (while (or fg bg)
        (setq attributes (face-attr-construct 'hl-paren-face))
        (when (car fg)
          (setq attributes (plist-put attributes :foreground (car fg))))
        (pop fg)
        (when (car bg)
          (setq attributes (plist-put attributes :background (car bg))))
        (pop bg)
        (when (car size)
          (setq attributes (plist-put attributes :height (car size)))
          (setq attributes (plist-put attributes :weight 'bold)))
        (pop size)
        (dotimes (i 2) ;; front and back
          (push (make-overlay 0 0) hl-paren-overlays)
          (overlay-put (car hl-paren-overlays) 'face attributes)))
      (setq hl-paren-overlays (nreverse hl-paren-overlays))))
  (add-hook 'my/-find-large-file-hook #'(lambda () (highlight-parentheses-mode -1))))

;; highlight-blocks-mode
;; do not enable it as it's too slow
;; (with-eval-after-load "highlight-blocks-autoloads"
;;   (add-hook 'my/-prog-mode-hook #'(lambda () (highlight-blocks-mode 1)))
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


;; auto-complete-mode
(when (< emacs-major-version 24)
  (with-eval-after-load "auto-complete-autoloads"
    (require 'auto-complete)
    (require 'auto-complete-config)
    ;;(add-to-list 'ac-dictionary-directories (concat my/-conf-path "auto-complete/dict"))
    ;;(require 'auto-complete-clang)
    ;;(require 'go-autocomplete)
    (ac-config-default)
    (setq clang-completion-suppress-error t
          ac-clang-flags (mapcar #'(lambda (item)(concat "-I" item))
                                 (c-get-system-includes)))

    (global-auto-complete-mode t)           ;enable global-mode
    (setq ac-auto-start 2                ;automatically start (disabled)
          ac-dwim t                        ;Do what i mean
          ac-override-local-map nil        ;don't override local map
          ac-use-quick-help nil ac-quick-help-delay 1.5
          ac-use-menu-map t ac-auto-show-menu 0.5
          ac-ignore-case t ac-delay 0.5 ac-use-fuzzy t ac-use-comphist t)
    (custom-set-variables
     '(ac-sources
       '(;;ac-source-filename
         ac-source-files-in-current-dir ;;ac-source-words-in-buffer
         ac-source-words-in-same-mode-buffers
         ;;ac-source-yasnippet ac-source-words-in-all-buffer ac-source-gtags
         ;;ac-source-imenu ac-source-abbrev ac-source-semantic
         ;;ac-source-semantic-raw ac-source-ropemacs ac-source-symbols
         )))

    (dolist (hook '(emacs-lisp-mode-hook inferior-emacs-lisp-mode
                                         lisp-mode-hook lisp-interaction-mode-hook))
      (add-hook hook #'(lambda () (add-to-list 'ac-sources 'ac-source-symbols))))
    (add-hook 'haskell-mode-hook #'(lambda () (add-to-list 'ac-sources 'ac-source-haskell)))
    (add-hook 'c-mode-common-hook #'(lambda ()
                                      ;;(setq ac-sources '(ac-source-clang ac-source-yasnippet))
                                      (add-to-list 'ac-sources 'ac-source-clang)
                                      ;;(setq ac-sources '(ac-source-semantic))
                                      ))
    (ac-flyspell-workaround)))


;; flx
(with-eval-after-load "flx-ido-autoloads"
  (setq ido-use-faces nil)
  (flx-ido-mode 1))

;; ido-vertical or ido-grid-mode
(if (>= emacs-major-version 24)
    (progn
      ;; (with-eval-after-load "ido-vertical-mode-autoloads"
      ;;   (ido-vertical-mode 1))
      (with-eval-after-load "ido-grid-mode-autoloads"
        (ido-grid-mode 1)))
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
