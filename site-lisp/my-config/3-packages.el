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


;; ido
(defface ido-first-match  '((default :weight bold)
                            (((class color) (min-colors 88) (background light))
                             (:background "#DFD"))
                            (((class color) (min-colors 88) (background dark))
                             (:background "#474")))
  "Face used by Ido for highlighting first match."
  :group 'ido)
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

;; drag-stuff
(with-eval-after-load "drag-stuff-autoloads"
  (drag-stuff-global-mode 1))

;; popwin
(with-eval-after-load "popwin-autoloads"
  (setq display-buffer-function 'popwin:display-buffer))

;; shackle
;; (with-eval-after-load "shackle-autoloads"
;;   (shackle-mode)
;;   (setq shackle-lighter "s"
;;         shackle-default-rule '(:other t))
;;   (push '(grep-mode :align t) shackle-rules))

;; golden-ratio
(with-eval-after-load "golden-ratio-autoloads"
  (when (>= emacs-major-version 24)
    (golden-ratio-mode 1))
  ;; (unless (fboundp 'window--resizable-p)
  ;;   (defun window--resizable-p (window &rest args)
  ;;     (with-selected-window window
  ;;       (not (minibuffer-selected-window)))))
  )

;; golden-ratio-scroll-screen
(with-eval-after-load "golden-ratio-scroll-screen-autoloads"
  (global-set-key [remap scroll-down-command] #'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command]   #'golden-ratio-scroll-screen-up))

;; magit
(with-eval-after-load "magit-autoloads"
  (add-hook 'magit-mode-hook '(lambda () (font-lock-mode 0))))

;; templates
(with-eval-after-load "template"
  (push (locate-user-emacs-file "site-lisp/templates/") template-default-directories)
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
}"))))))
(with-eval-after-load "skeletor"
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
  (setq skeletor-global-substitutions
        (nconc
         (list (cons "__DATE-TIME__" #'(lambda () (format-time-string "%d/%m/%Y %H:%M")))
               (cons "__USER-NICKNAME__" #'(lambda () user-nickname))
               (cons "__LICENSE__" #'skeletor-project-license-type))
         skeletor-global-substitutions)))

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
  (global-set-key (kbd "C->") #'mc/mark-next-symbol-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-symbol-like-this)
  (global-set-key (kbd "C-M-.") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-M-,") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C->") #'mc/mark-all-like-this-dwim)
  (global-set-key (kbd "C-;") #'mc/mark-all-symbols-like-this)
  (global-set-key (kbd "C-:") #'mc/mark-all-symbols-like-this-in-defun)
  (global-set-key (kbd "C-c SPC") #'set-rectangular-region-anchor)

  (global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click))
(with-eval-after-load "multiple-cursors-core"
  (define-key mc/keymap (kbd "C-.")  #'mc/unmark-next-like-this)
  (define-key mc/keymap (kbd "C-,")  #'mc/unmark-previous-like-this)
  (define-key mc/keymap (kbd "C-?")  #'mc/skip-to-next-like-this)
  (define-key mc/keymap (kbd "C-\"") #'mc/skip-to-previous-like-this)

  (define-key mc/keymap (kbd "C-S-s") #'mc/toggle-pause)

  (define-key mc/keymap (kbd "C-'") #'mc-hide-unmatched-lines-mode)
  (require 'mc-cycle-cursors))

;; expand-region
(with-eval-after-load "expand-region-autoloads"
  (global-set-key (kbd "M-o") 'er/expand-region))

;; redo+
(with-eval-after-load "redo+-autoloads"
  (setq undo-no-redo t)
  (require 'redo+))

;; undo-tree
(with-eval-after-load "undo-tree-autoloads"
  (autoload 'undo-tree-visualize "undo-tree" "" t))

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
  (add-hook 'my/-prog-mode-hook #'idle-highlight-mode))

;; highlight-symbol
(with-eval-after-load "highlight-symbol-autoloads"
  (setq highlight-symbol-disable '())
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (null (memql major-mode highlight-symbol-disable))
                (highlight-symbol-mode)
                (highlight-symbol-nav-mode)))))

;; highlight-stages
;; (with-eval-after-load "highlight-stages-autoloads"
;;   (highlight-stages-global-mode))

;; highlight-indentation
;; (with-eval-after-load "highlight-indentation-autoloads"
;;   (add-hook 'my/-prog-mode-hook #'highlight-indentation-mode))

;; highlight-leading-spaces
;; (with-eval-after-load "highlight-leading-spaces-autoloads"
;;   (add-hook 'my/-prog-mode-hook #'highlight-leading-spaces-mode))

;; highlight-escape
(with-eval-after-load "highlight-escape-sequences-autoloads"
  (hes-mode))

;; highlight-quoted
(with-eval-after-load "highlight-quoted-autoloads"
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

;; highlight-numbers
(with-eval-after-load "highlight-numbers-autoloads"
  (setq highlight-numbers-disable '())
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (null (memql major-mode highlight-numbers-disable))
                (highlight-numbers-mode))))
  (highlight-numbers-mode))

;; highlight-tail-mode
(with-eval-after-load "highlight-tail-autoloads"
  (highlight-tail-mode 1)
  (add-hook 'my/-find-large-file-hook #'(lambda () (highlight-tail-mode -1))))
(with-eval-after-load "highlight-tail"
  (defun highlight-tail-get-bgcolor-hex (point)
    "Get the background color of point.
Do not take highlight-tail's overlays into consideration.  This means
that if there is ht's overlay at at the top then return 'default"
    (let ((point-face (get-char-property point 'face))
          point-face-from-cache
          point-face-bgcolor
          point-face-bgcolor-hex)
      (when point-face
        (when (listp point-face) (setq point-face (car point-face)))
        ;; This is weird because for howm-reminder-today-face, the
        ;; (get-char-property) function returns a list:
        ;; (howm-reminder-today-face), so it's needed to get car of
        ;; it...
        (when (stringp point-face) (setq point-face (intern point-face)))
        ;; This is weird because for faces used by ediff, the
        ;; (get-char-property) function returns a string:
        ;; "xxx-face", so it's needed to intern it...
        (unless (facep point-face) (setq point-face 'default)))
      (if point-face
          (progn
            (setq point-face-from-cache
                  (assoc point-face highlight-tail-nonhtfaces-bgcolors))
            (if point-face-from-cache
                (setq point-face-bgcolor-hex (cdr point-face-from-cache))
              (setq point-face-bgcolor
                    (highlight-tail-get-face-background point-face))
              (when (or (eq point-face-bgcolor nil)
                        (eq point-face-bgcolor 'unspecified))
                (setq point-face-bgcolor 'default))))
        (setq point-face-bgcolor 'default))
      (when (not point-face-bgcolor-hex)  ; not read from cache
        (if (eq point-face-bgcolor 'default)
            (setq point-face-bgcolor-hex 'default)
          ;; else
          (setq point-face-bgcolor-hex
                (highlight-tail-hex-from-colorname point-face-bgcolor))
          (setq highlight-tail-nonhtfaces-bgcolors
                (cons (cons point-face point-face-bgcolor-hex)
                      highlight-tail-nonhtfaces-bgcolors))
          (highlight-tail-add-colors-fade-table point-face-bgcolor-hex)
          (highlight-tail-make-faces
           (highlight-tail-get-colors-fade-table-with-key
            point-face-bgcolor-hex))))
      ;; return value
      point-face-bgcolor-hex)))

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
  (add-hook 'my/-find-large-file-hook #'(lambda () (highlight-parentheses-mode -1))))
(with-eval-after-load "highlight-parentheses"
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
      (setq hl-paren-overlays (nreverse hl-paren-overlays)))))

;; highlight-blocks-mode
;; do not enable it as it's too slow
;; (with-eval-after-load "highlight-blocks-autoloads"
;;   (add-hook 'my/-prog-mode-hook #'(lambda () (highlight-blocks-mode 1)))
;;   (add-hook 'activate-mark-hook #'(lambda () (highlight-blocks-mode -1)))
;;   (add-hook 'deactivate-mark-hook #'(lambda () (highlight-blocks-mode 1))))

;; smart-modeline
(with-eval-after-load "smart-mode-line-autoloads"
  (sml/setup))

;; prompt-text-el
(with-eval-after-load "prompt-text-autoloads"
  (setq prompt-text-format
        `("[" (:eval (symbol-name this-command)) "] "))
  (prompt-text-mode))

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
        company-show-numbers t
        company-tooltip-align-annotations t)

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

        (setq-local completion-at-point-functions
                    (list (if (fboundp 'elisp-completion-at-point)
                              #'elisp-completion-at-point
                            #'lisp-completion-at-point) t))

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
  ;;(remove-hook 'minibuffer-setup-hook #'minibuffer-company)
  ;;(add-hook 'eval-expression-minibuffer-setup-hook #'minibuffer-company)
  ;; (with-eval-after-load "company-flx-autoloads"
  ;; (company-flx-mode)
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
                                 (my/-c-get-includes)))

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


;; speedbar
(with-eval-after-load "speedbar"
  (push (cons 'persp-ignore-wconf t) speedbar-frame-parameters))

;; helm
;; (with-eval-after-load "helm-autoloads"
;;   (add-hook 'my/-packages-initialized-hook (lambda () (require 'helm-config))))
(with-eval-after-load "helm-autoloads";"helm-config"
  (global-set-key (kbd "C-c h") 'helm-command-prefix)

  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t)

  (with-eval-after-load "golden-ratio-autoloads"
    (push #'helm-alive-p golden-ratio-inhibit-functions))

  (global-set-key (kbd "M-x") #'helm-M-x)
  (setq helm-M-x-fuzzy-match t)

  (global-set-key (kbd "M-y") #'helm-show-kill-ring)

  (global-set-key (kbd "C-x b") #'helm-mini)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)

  (global-set-key (kbd "C-x C-f") #'helm-find-files)

  (helm-mode 1)
  ;; (helm-autoresize-mode t)
  (setq helm-autoresize-max-height 30)

  (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

  (defun helm-toggle-header-line ()
    (if (> (length helm-sources) 1)
        (set-face-attribute 'helm-source-header
                            nil
                            :foreground helm-source-header-default-foreground
                            :background helm-source-header-default-background
                            :box helm-source-header-default-box
                            :height 1.0)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground (face-attribute 'helm-selection :background)
                          :background (face-attribute 'helm-selection :background)
                          :box nil
                          :height 0.1)))
  (add-hook 'helm-before-initialize-hook #'helm-toggle-header-line)
  ;;(helm-flx-mode)
  )

;; (with-eval-after-load "scroll-restore-autoloads"
;;   (setq scroll-restore-jump-back t
;;         scroll-restore-handle-cursor t
;;         scroll-restore-handle-region t)
;;   ;; (scroll-restore-mode)
;;   )

;; persp-mode
(with-eval-after-load "persp-mode-autoloads"
  (defvar after-switch-to-buffer-functions nil)
  (defvar after-display-buffer-functions nil)

  (when (fboundp 'advice-add)
    (defun after-switch-to-buffer-adv (&rest r)
      (apply #'run-hook-with-args 'after-switch-to-buffer-functions r))
    (defun after-display-buffer-adv (&rest r)
      (apply #'run-hook-with-args 'after-display-buffer-functions r))
    (advice-add #'switch-to-buffer :after #'after-switch-to-buffer-adv)
    (advice-add #'display-buffer   :after #'after-display-buffer-adv))

  (setq wg-morph-on nil)
  ;;(setq windmove-window-distance-delta 2)
  (unless (>= emacs-major-version 24)
    (setq persp-when-kill-switch-to-buffer-in-perspective nil))

  (setq command-switch-alist
        (cons
         (cons "persp-q"
               #'(lambda (p)
                   (setq persp-auto-resume-time -1
                         persp-auto-save-opt 0)))
         command-switch-alist))

  (with-eval-after-load "dired"
    (def-auto-persp "dired"
      :parameters '((dont-save-to-file . t))
      :mode 'dired-mode
      :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
                 (persp-add-buffer-on-find-file nil)
                 persp-add-buffer-on-after-change-major-mode)
      :hooks '(after-switch-to-buffer-functions)
      :after-match #'(lambda (pn p b h ha ps noa)
                       (persp-window-switch pn))))

  (push #'(lambda () (persp-mode 1)
            (global-set-key (kbd "C-x k") #'persp-kill-buffer)
            (with-eval-after-load "helm"
              (persp-update-completion-system 'completing-read)))
        after-init-hook))


(with-eval-after-load "persp-mode"
  (with-eval-after-load "helm-mode"
    (defun helm-buffers-toggle-persp-filter ()
      (interactive)
      (with-helm-alive-p
       (let ((filter-attrs (helm-attr 'candidate-transformer
				      helm-source-persp-buffers)))
	 (if (memq #'helm-persp-buffers-filter-transformer filter-attrs)
	     (progn
	       (helm-attrset 'candidate-transformer
			     (delq #'helm-persp-buffers-filter-transformer
				   filter-attrs)
			     helm-source-persp-buffers t)
	       (helm-attrset 'name
			     "Buffers"
			     helm-source-persp-buffers t)
	       (setq helm-persp-filtered-buffers-cache nil))
	   (helm-attrset 'candidate-transformer
			 (cons #'helm-persp-buffers-filter-transformer
			       filter-attrs)
			 helm-source-persp-buffers t)
	   (helm-attrset 'name
			 "Current perspective buffers"
			 helm-source-persp-buffers t))
	 (helm-force-update))))
    (put 'helm-buffers-toggle-persp-filter 'helm-only t)
    (define-key helm-buffer-map
      persp-toggle-read-persp-filter-keys #'helm-buffers-toggle-persp-filter)
    (defvar helm-persp-filtered-buffers-cache nil)
    (defun helm-persp-buffers-filter-transformer (candidates)
      (setq helm-persp-filtered-buffers-cache nil)
      (let* ((persp (get-current-persp))
	     (ret
	      (cl-remove-if-not #'(lambda (bn)
				    (let* ((ret (persp-contain-buffer-p (get-buffer bn) persp)))
				      (unless ret
					(push bn helm-persp-filtered-buffers-cache))
				      ret))
				candidates)))
	ret))
    (defclass helm-persp-buffers-source (helm-source-buffers)
      ((buffer-list
	:initarg :buffer-list
	:initform #'(lambda () (mapcar #'buffer-name (persp-buffer-list-restricted nil -1 nil)))
	:custom function
	:documentation
	" A function with no arguments to create buffer list.")
       (cleanup :initform #'(lambda () (setq helm-persp-filtered-buffers-cache nil
					     helm-buffers-list-cache nil)))
       (candidate-transformer :initform '(helm-persp-buffers-filter-transformer))))
    (defvar helm-source-persp-buffers
      (helm-make-source "Current perspective buffers"
			'helm-persp-buffers-source
			:fuzzy-match t))
    (defclass helm-persp-filtered-buffers-source (helm-source-buffers)
      ((init :initform #'(lambda ()
			   (when (and (not helm-buffers-list-cache)
				      (not helm-persp-filtered-buffers-cache))
			     (setq helm-persp-filtered-buffers-cache
				   (persp-buffer-list-restricted nil -1 nil)))))
       (candidates :initform helm-persp-filtered-buffers-cache)
       (cleanup :initform #'(lambda () (setq helm-persp-filtered-buffers-cache nil)))))
    (defvar helm-source-persp-filtered-buffers
      (helm-make-source "Other buffers"
			'helm-persp-filtered-buffers-source
			:fuzzy-match t))
    (defun helm-persp-buffer-list-bridge
	(prompt _collection &optional test _require-match init hist default _inherit-im name buffer)
      (let ((dflt (or default "")))
	(or
	 (helm :sources '(helm-source-persp-buffers helm-source-persp-filtered-buffers)
	       :fuzzy-match helm-mode-fuzzy-match
	       :prompt prompt
	       :buffer buffer
	       :input init
	       :history hist
	       :resume 'noresume
	       :keymap helm-buffer-map
	       :truncate-lines helm-buffers-truncate-lines
	       :default dflt
	       :preselect (substring dflt 0 (min (string-width dflt) helm-buffer-max-length)))
	 (helm-mode--keyboard-quit))))
    (setq helm-mini-default-sources
	  (cons helm-source-persp-buffers
		(cons helm-source-persp-filtered-buffers
		      (cdr helm-mini-default-sources))))
    (defun helm-persp-mini ()
      (interactive)
      (let* ((cbuf (current-buffer))
	     (cbn (buffer-name cbuf)))
	(or
	 (helm :sources helm-mini-default-sources
	       :ff-transformer-show-only-basename nil
	       :fuzzy-match helm-mode-fuzzy-match
	       :buffer "*helm persp mini*"
	       :keymap helm-buffer-map
	       :truncate-lines helm-buffers-truncate-lines
	       :default cbn
	       :preselect (substring cbn 0 (min (string-width cbn) helm-buffer-max-length)))
	 (helm-mode--keyboard-quit))))
    (global-set-key (kbd "C-x b") #'helm-persp-mini)
    (setq helm-completing-read-handlers-alist
	  (append '((switch-to-buffer . helm-persp-buffer-list-bridge)
		    (kill-buffer . helm-persp-buffer-list-bridge)
		    (persp-kill-buffer . helm-persp-buffer-list-bridge)
		    (persp-temporarily-display-buffer . helm-persp-buffer-list-bridge)
		    (persp-add-buffer . helm-persp-buffer-list-bridge)
		    (persp-remove-buffer . helm-persp-buffer-list-bridge))
		  helm-completing-read-handlers-alist))))
