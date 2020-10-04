;;; 3-packages.el --- setting up packages


;;; Code:


(require 'generic-x)
(require 'newcomment)


(defun fbread-mode ()
  (interactive)
  (sgml-mode)
  (sgml-tags-invisible 0)
  (longlines-mode)
  (view-mode))
(add-to-list 'auto-mode-alist '("\\.fb2$" . fbread-mode))

(let ((hl-line-mode-hooks '(tabulated-list-mode-hook
                            occur-mode-hook grep-mode-hook
                            ivy-occur-mode-hook)))
  (dolist (hook hl-line-mode-hooks)
    (add-hook hook #'(lambda () (hl-line-mode 1)))))

(with-eval-after-load "comint"
  (push #'comint-truncate-buffer comint-output-filter-functions))

(with-eval-after-load "whitespace"
  (setq whitespace-style '(face lines-tail trailing tab-mark)
        whitespace-line-column fill-column))

;;Jabber
(with-eval-after-load "jabber"
  (defun jabber-muc-join-group ()
    (interactive)
    (let ((account (jabber-read-account)))
      (jabber-get-bookmarks
       account
       #'(lambda (jc bms)
           (let (groups group nickname)
             (mapc #'(lambda (b)
                       (when (eq 'conference (jabber-xml-node-name b))
                         (push (make-symbol (jabber-xml-get-attribute b 'jid))
                               groups)))
                   bms)
             (setq group (jabber-read-jid-completing "group: " groups)
                   nickname (jabber-muc-read-my-nickname jc group))
             (jabber-muc-join jc group nickname)))))))

(with-eval-after-load "edit-list-autoloads"
  (autoload 'edit-list "edit-list" "edit list" t))

;; ido
;; (defface ido-first-match
;;   '((default :weight bold)
;;     (((class color) (min-colors 88) (background light))
;;      (:background "#DFD"))
;;     (((class color) (min-colors 88) (background dark))
;;      (:background "#474")))
;;   "Face used by Ido for highlighting first match."
;;   :group 'ido)
;; (with-eval-after-load "ido"
;;   (setq ido-enable-prefix nil
;;         ido-enable-flex-matching t
;;         ido-create-new-buffer 'always
;;         ido-use-filename-at-point 'guess
;;         ido-default-file-method 'selected-window
;;         ido-max-prospects 10)
;;   (ido-mode t)
;;   (ido-everywhere t))
;; (require 'ido)

;; (with-eval-after-load "ido-ubiquitous-autoloads"
;;   (ido-ubiquitous-mode t))
;; (with-eval-after-load "ido-at-point-autoloads"
;;   (ido-at-point-mode 1))

;; ;; flx
;; (with-eval-after-load "flx-ido-autoloads"
;;   (setq ido-use-faces nil)
;;   (flx-ido-mode 1))

;; ;; ido-vertical or ido-grid-mode
;; (if (>= emacs-major-version 24)
;;     (progn
;;       ;; (with-eval-after-load "ido-vertical-mode-autoloads"
;;       ;;   (ido-vertical-mode 1))
;;       (with-eval-after-load "ido-grid-mode-autoloads"
;;         (ido-grid-mode 1)))
;;   (with-eval-after-load "ido"
;;     ;; Display ido results vertically, rather than horizontally
;;     (setq ido-decorations
;;           (quote ("\n-> " "" "\n   " "\n   ..."
;;                   "[" "]" " [No match]" " [Matched]" " [Not readable]"
;;                   " [Too big]" " [Confirm]")))
;;     (defun ido-disable-line-truncation ()
;;       (set (make-local-variable 'truncate-lines) nil))
;;     (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)))

;; ;; smex
;; (if (>= emacs-major-version 24)
;;     (with-eval-after-load "smex-autoloads"
;;       (smex-initialize)
;;       (global-set-key (kbd "M-x") #'smex)
;;       (global-set-key (kbd "M-X") #'smex-major-mode-commands)
;;       ;; This is your old M-x.
;;       (global-set-key (kbd "C-c C-c M-x") #'execute-extended-command)
;;       (smex-auto-update 60)
;;       (setq smex-save-file (expand-file-name "~/.emacs.d/.smex-items")))
;;   (with-eval-after-load "ido"
;;     (global-set-key
;;      "\M-x"
;;      (lambda ()
;;        (interactive)
;;        (call-interactively
;;         (intern
;;          (ido-completing-read
;;           "M-x " (all-completions "" obarray 'commandp))))))))


;; Tabbar
(with-eval-after-load "tabbar-autoloads"
  (tabbar-mode 1)
  (set-face-attribute
   'tabbar-separator nil
   :background "gray20"
   :height 0.6))

;; Common header/mode-line

(with-eval-after-load "common-header-mode-line-autoloads"
  (add-hook
   'window-setup-hook
   #'(lambda ()
       (common-header-mode-line-mode 1)

       (dolist (frame (frame-list))
         (set-frame-parameter frame 'bottom-divider-width 1)
         (set-frame-parameter frame 'right-divider-width 1))

       (push (cons 'bottom-divider-width 1) default-frame-alist)
       (push (cons 'right-divider-width 1) default-frame-alist)

       (let ((def-height (face-attribute 'default :height)))
         (set-face-attribute
          'per-window-header-line-active-face nil
          :height (ceiling (max 1 (* 0.9 def-height))))

         (set-face-attribute
          'per-window-header-line-inactive-face nil
          :height (floor (max 1 (* 0.7 def-height)))))

       ;; (set-face-background
       ;;  'per-window-header-line-active-face
       ;;  (face-background 'mode-line))

       ;; (set-face-background
       ;;  'per-frame-header-line-inactive-face
       ;;  (face-background 'mode-line-inactive))

       ;; (setq common-header-mode-line-update-delay 0.1)

       (defvar per-window-header-line-active-format nil)
       (defvar per-window-header-line-inactive-format nil)

       (add-hook 'semantic-stickyfunc-mode-hook
                 #'(lambda ()
                     (if (and semantic-mode semantic-stickyfunc-mode)
                         (push semantic-stickyfunc-header-line-format
                               per-window-header-line-active-format)
                       (setq per-window-header-line-active-format
                             (delq semantic-stickyfunc-header-line-format
                                   per-window-header-line-active-format)))))

       (add-hook 'multiple-cursors-mode-hook
                 #'(lambda ()
                     (if multiple-cursors-mode
                         (add-to-list 'per-window-header-line-active-format
                                      mc/mode-line)
                       (setq per-window-header-line-active-format
                             (delq mc/mode-line
                                   per-window-header-line-active-format)))))

       (setq per-window-header-line-format-function
             #'(lambda (win)
                 (unless per-window-header-line-active-format
                   (setq per-window-header-line-active-format
                         `("" mode-line-front-space mode-line-mule-info
                           mode-line-modified
                           mode-line-remote " " mode-line-buffer-identification
                           " " mode-line-position (vc-mode vc-mode) " "
                           ;;" " ,(caddr mode-line-modes)
                           mode-line-misc-info mode-line-end-spaces)))
                 (unless per-window-header-line-inactive-format
                   (setq per-window-header-line-inactive-format
                         `("" mode-line-front-space mode-line-mule-info
                           mode-line-modified
                           mode-line-remote " " mode-line-buffer-identification
                           " " ,(caddr mode-line-position) (vc-mode vc-mode) " "
                           mode-line-misc-info mode-line-end-spaces)))
                 ;; (let* ((buf (window-buffer win))
                 ;;        (bfrmt (buffer-local-value 'header-line-format buf))
                 ;;        (cf-sym (if (eq win (selected-window))
                 ;;                    'per-window-header-line-active-format
                 ;;                  'per-window-header-line-inactive-format)))
                 ;;   (with-current-buffer buf
                 ;;     (when (and (not (eq bfrmt
                 ;;                         per-window-header-line-active-format))
                 ;;                (not (eq bfrmt
                 ;;                         per-window-header-line-inactive-format))
                 ;;                (not (eq bfrmt tabbar-header-line-format)))
                 ;;       (set (make-local-variable
                 ;;             'per-window-header-line-active-format)
                 ;;            (cons bfrmt (default-value
                 ;;                          'per-window-header-line-active-format)))
                 ;;       (set (make-local-variable
                 ;;             'per-window-header-line-inactive-format)
                 ;;            (cons bfrmt (default-value
                 ;;                          'per-window-header-line-inactive-format))))
                 ;;     (symbol-value cf-sym)))
                 (let* ((buf (window-buffer win))
                        (frmt (unless (with-current-buffer buf
                                        (derived-mode-p 'magit-mode))
                                (if (eq (selected-window) win)
                                    per-window-header-line-active-format
                                  per-window-header-line-inactive-format)))
                        ;; (bfrmt (buffer-local-value 'header-line-format
                        ;;                            (window-buffer win)))
                        )
                   ;; (if (eq frmt (cdr bfrmt))
                   ;;     (setq frmt bfrmt)
                   ;;   (when (and bfrmt (not (eq bfrmt frmt))
                   ;;              (not (eq bfrmt '(:eval (tabbar-line)))))
                   ;;     (setq frmt (cons bfrmt frmt))))
                   (or frmt (buffer-local-value 'header-line-format buf)))))

       (setq
        per-frame-mode-line-update-display-function
        #'(lambda (display)
            (let ((buf (cdr (assq 'buf display))))
              (with-current-buffer buf
                (setq-local buffer-read-only nil)
                (erase-buffer)
                (let*
                    ((mode-l-str
                      (format-mode-line
                       `("%e" mode-line-front-space
                         (eldoc-mode-line-string (" " eldoc-mode-line-string " "))
                         mode-line-modified mode-line-client
                         mode-line-frame-identification
                         mode-line-modes mode-line-misc-info mode-line-end-spaces)
                       'per-frame-mode-line-face
                       per-frame-header-mode-line--selected-window)))
                  (insert mode-l-str))
                (setq-local mode-line-format nil)
                (setq-local header-line-format nil)
                (goto-char (point-min))
                (setq-local buffer-read-only t))))))))

;; move where I mean
(with-eval-after-load "mwim-autoloads"
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line))

;; drag-stuff
(with-eval-after-load "drag-stuff-autoloads"
  (drag-stuff-global-mode 1)

  (defun drag-stuff-sexp-left (arg)
    "Drags word left ARG times."
    (drag-stuff-sexp-horizontally (- arg)))

  (defun drag-stuff-sexp-right (arg)
    "Drags word right ARG times."
    (drag-stuff-sexp-horizontally arg))

  (defun drag-stuff-sexp-horizontally (arg)
    "Drags word horizontally ARG times."
    (let ((old-point (point))
          (offset (- (my/-save-mark-and-excursion
                       (forward-sexp) (point))
                     (point))))
      (condition-case err
          (progn
            (transpose-sexps arg)
            (backward-char offset))
        (error
         (message
          (if (> arg 0)
              "Can not move word further to the right"
            "Can not move word further to the left"))
         (goto-char old-point)))))

  (defun drag-stuff-right (arg)
    "Drag stuff ARG lines to the right."
    (interactive "p")
    (if mark-active
        (drag-stuff-region-right arg)
      (drag-stuff-sexp-right arg)))

  (defun drag-stuff-left (arg)
    "Drag stuff ARG lines to the left."
    (interactive "p")
    (if mark-active
        (drag-stuff-region-left arg)
      (drag-stuff-sexp-left arg)))

  (drag-stuff-define-keys))

;; ;; popwin
;; (with-eval-after-load "popwin-autoloads"
;;   (setq display-buffer-function 'popwin:display-buffer))

;; shackle
;; (with-eval-after-load "shackle-autoloads"
;;   (shackle-mode)
;;   (setq shackle-lighter "s"
;;         shackle-default-rule '(:other t))
;;   (push '(grep-mode :align t) shackle-rules))

;; golden-ratio
;; (with-eval-after-load "golden-ratio-autoloads"
;;   (when (>= emacs-major-version 24)
;;     (golden-ratio-mode 1))
;;   ;; (unless (fboundp 'window--resizable-p)
;;   ;;   (defun window--resizable-p (window &rest args)
;;   ;;     (with-selected-window window
;;   ;;       (not (minibuffer-selected-window)))))
;;   )
;; zoom
(with-eval-after-load "zoom-autoloads"
  (defun my/--zoom-size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
          (t                            '(0.5 . 0.5))))
  (setq zoom-size 'my/--zoom-size-callback)
  (zoom-mode))

;; golden-ratio-scroll-screen
(with-eval-after-load "golden-ratio-scroll-screen-autoloads"
  (global-set-key [remap scroll-down-command] #'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command]   #'golden-ratio-scroll-screen-up))

;; magit
;; (with-eval-after-load "magit-autoloads"
;;   (add-hook 'magit-mode-hook '(lambda () (font-lock-mode 0))))

;; templates
(with-eval-after-load "template"
  (push (locate-user-emacs-file "site-lisp/templates/")
        template-default-directories)
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
              skeletor-project-license
              (when .create-license?
                (skeletor--read-license "License: " .license-file-name))
              skeletor-project-spec
              (-concat
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
                (cons
                 'repls
                 (-map 'skeletor--eval-substitution
                       (-concat
                        skeletor-global-substitutions
                        (list (cons "__PROJECT-NAME__" project-name)
                              (cons "__LICENSE-FILE-NAME__" .license-file-name))
                        .substitutions))))
               skeletor-project-spec)))))
  (setq skeletor-global-substitutions
        (nconc
         (list (cons "__DATE-TIME__"
                     #'(lambda () (format-time-string "%d/%m/%Y %H:%M")))
               (cons "__USER-NICKNAME__" #'(lambda () user-nickname))
               (cons "__LICENSE__" #'skeletor-project-license-type))
         skeletor-global-substitutions)))

;; ;; yasnippet
;; (with-eval-after-load "yasnippet-autoloads"
;;   (yas-global-mode)
;;   (add-hook 'term-mode-hook #'(lambda () (yas-minor-mode -1))))

;; uniquify
(with-eval-after-load "uniquify"
  (setq uniquify-buffer-name-style 'forward
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(with-eval-after-load "hl-todo-autoloads"
  ;;(setq hl-todo-activate-in-modes '(prog-mode)
  ;; (add-hook 'after-change-major-mode-hook
  ;;           #'(lambda () (turn-on-hl-todo-mode-if-desired)))
  (with-eval-after-load "hl-todo"
    (setq hl-todo-keyword-faces
          '(("HOLD" . "#d0bf8f")
            ("TODO" . "#ff4323")
            ("NEXT" . "#dca3a3")
            ("THEM" . "#dc8cc3")
            ("PROG" . "#7cb8bb")
            ("OKAY" . "#7cb8bb")
            ("DONT" . "#5f7f5f")
            ("FAIL" . "#8c5353")
            ("DONE" . "#afd8af")
            ("NOTE"   . "#d0bf8f")
            ("KLUDGE" . "#d0bf8f")
            ("HACK"   . "#ff4323")
            ("FIXME"  . "#ff4323")
            ("XXX"    . "#ff4323")
            ("XXXX"   . "#ff4323")
            ("???"    . "#ff4323"))))
  (add-hook 'my/-prog-mode-hook #'(lambda () (hl-todo-mode +1)))
  (add-hook 'text-mode-hook #'(lambda () (hl-todo-mode +1))))

(with-eval-after-load "line-reminder-autoloads"
  (setq line-reminder-ignore-buffer-names
        '("*Buffer List*"
          "*Checkdoc Status*"
          "*Echo Area 0*"
          "*helm "
          "magit"
          "*run*"
          "*shell*"
          "*undo-tree*"))
  (global-line-reminder-mode t))

(with-eval-after-load "hilit-chg"
  (setq highlight-changes-colors
        '("#970" "#907" "#329" "#938" "#743" "#784" "#967")))

(with-eval-after-load "hi-lock"
  (defun highlight-toggle-symbol-at-point ()
    "Toggle highlight of symbol at point."
    (interactive)
    (let* ((regexp (hi-lock-regexp-okay
                    (find-tag-default-as-symbol-regexp)))
           (hi-lock-auto-select-face t)
           (face (hi-lock-read-face-name)))
      (unless (string-prefix-p "\\_<" regexp)
        (setq regexp
              (concat "\\_<" regexp
                      (unless (string-suffix-p "\\_>" regexp)
                        "\\_>"))))
      (let ((old (assoc regexp hi-lock-interactive-patterns)))
        (if old
            (hi-lock-unface-buffer regexp)
          (or (facep face) (setq face 'hi-yellow))
          (unless hi-lock-mode (hi-lock-mode 1))
          (hi-lock-set-pattern regexp face))))))
(autoload 'highlight-toggle-symbol-at-point "hi-lock"
  "Toggle highlight of symbol at point" t)

;; mark-multiple
;; (with-eval-after-load "multiple-cursors-autoloads"
  ;; (global-set-key (kbd "C->") #'mc/mark-next-symbol-like-this)
  ;; (global-set-key (kbd "C-<") #'mc/mark-previous-symbol-like-this)
  ;; (global-set-key (kbd "C-M->") #'mc/mark-next-like-this)
  ;; (global-set-key (kbd "C-M-<") #'mc/mark-previous-like-this)
  ;; (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
  ;; (global-set-key (kbd "C-c C->") #'mc/mark-all-like-this-dwim)
  ;; (global-set-key (kbd "C-;") #'mc/mark-all-symbols-like-this)
  ;; (global-set-key (kbd "C-:") #'mc/mark-all-symbols-like-this-in-defun)
  ;; (global-set-key (kbd "C-c SPC") #'set-rectangular-region-anchor)

  ;; (global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click))
;; (with-eval-after-load "multiple-cursors-core"
;;   (define-key mc/keymap (kbd "C-.")  #'mc/unmark-next-like-this)
;;   (define-key mc/keymap (kbd "C-,")  #'mc/unmark-previous-like-this)
;;   (define-key mc/keymap (kbd "C-?")  #'mc/skip-to-next-like-this)
;;   (define-key mc/keymap (kbd "C-\"") #'mc/skip-to-previous-like-this)

;;   (define-key mc/keymap (kbd "C-S-s") #'mc/toggle-pause)

;;   (define-key mc/keymap (kbd "C-'") #'mc-hide-unmatched-lines-mode)
;;   (require 'mc-cycle-cursors))
(with-eval-after-load "multiple-cursors-autoloads"
  (autoload 'mc/cycle-backward "mc-cycle-cursors" nil t)
  (autoload 'mc/cycle-forward  "mc-cycle-cursors" nil t)

  (with-eval-after-load "hydra-autoloads"
    (defhydra multiple-cursors-hydra (:hint nil)

      "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_s_] Mark all symbols
[_P_]   Skip    [_N_]   Skip    [_S_] -||- in defun
[_M-p_] Unmark  [_M-n_] Unmark  [_a_] Mark all
[_M-v_] Go Prev [_C-v_] Go Next [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit"

      ("s" mc/mark-all-symbols-like-this)
      ("S" mc/mark-all-symbols-like-this-in-defun)
      ("a" mc/mark-all-like-this)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("r" mc/mark-all-in-region-regexp)
      ("M-v" mc/cycle-backward)
      ("C-v" mc/cycle-forward)
      ("q" nil))
    (global-set-key (kbd "C-c ;") #'multiple-cursors-hydra/body)))

;; expand-region
(with-eval-after-load "expand-region-autoloads"
  (global-set-key (kbd "M-o") #'(lambda (arg)
                                  (interactive "p")
                                  (let (shift-select-mode)
                                    (er/expand-region arg)))))

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
;; (with-eval-after-load "idle-highlight-mode-autoloads"
;;   (setq idle-highlight-delay 1.0)
;;   (add-hook 'my/-prog-mode-hook #'idle-highlight-mode))

;; highlight-symbol
;; (with-eval-after-load "highlight-symbol-autoloads"
;;   (setq highlight-symbol-disable '())
;;   (with-eval-after-load "highlight-symbol"
;;     (set-face-background 'highlight-symbol-face nil)
;;     (set-face-underline 'highlight-symbol-face "#0F0"))
;;   (add-hook 'my/-prog-mode-hook
;;             (lambda ()
;;               (when (null (memql major-mode highlight-symbol-disable))
;;                 (highlight-symbol-mode)
;;                 (highlight-symbol-nav-mode)))))

;; symbol-overlay
(with-eval-after-load "symbol-overlay-autoloads"
  (add-hook 'my/-prog-mode-hook
            (lambda () (symbol-overlay-mode 1)))
  (with-eval-after-load "symbol-overlay"
    (set-face-attribute 'symbol-overlay-default-face nil
                        :inherit 'default
                        :foreground nil :background nil
                        :underline "#0F0")
    (global-set-key (kbd "M-i") 'symbol-overlay-put)
    (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
    (global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
    (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
    (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)))

;; ;; highlight-stages
;; (with-eval-after-load "highlight-stages-autoloads"
;;   (highlight-stages-global-mode))

;; ;; highlight-indentation
;; (with-eval-after-load "highlight-indentation-autoloads"
;;   (add-hook 'my/-prog-mode-hook #'highlight-indentation-mode))

;; ;; highlight-leading-spaces
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
  (add-hook 'my/-prog-mode-hook #'highlight-numbers-mode)
  (add-hook 'text-mode-hook #'highlight-numbers-mode))

;; ;; highlight-tail-mode
;; (with-eval-after-load "highlight-tail-autoloads"
;;   (highlight-tail-mode 1)
;;   (add-hook 'my/-find-large-file-hook #'(lambda () (highlight-tail-mode -1))))
;; (with-eval-after-load "highlight-tail"
;;   (defun highlight-tail-get-bgcolor-hex (point)
;;     "Get the background color of point.
;; Do not take highlight-tail's overlays into consideration.  This means
;; that if there is ht's overlay at at the top then return 'default"
;;     (let ((point-face (get-char-property point 'face))
;;           point-face-from-cache
;;           point-face-bgcolor
;;           point-face-bgcolor-hex)
;;       (when point-face
;;         (when (listp point-face) (setq point-face (car point-face)))
;;         ;; This is weird because for howm-reminder-today-face, the
;;         ;; (get-char-property) function returns a list:
;;         ;; (howm-reminder-today-face), so it's needed to get car of
;;         ;; it...
;;         (when (stringp point-face) (setq point-face (intern point-face)))
;;         ;; This is weird because for faces used by ediff, the
;;         ;; (get-char-property) function returns a string:
;;         ;; "xxx-face", so it's needed to intern it...
;;         (unless (facep point-face) (setq point-face 'default)))
;;       (if point-face
;;           (progn
;;             (setq point-face-from-cache
;;                   (assoc point-face highlight-tail-nonhtfaces-bgcolors))
;;             (if point-face-from-cache
;;                 (setq point-face-bgcolor-hex (cdr point-face-from-cache))
;;               (setq point-face-bgcolor
;;                     (highlight-tail-get-face-background point-face))
;;               (when (or (eq point-face-bgcolor nil)
;;                         (eq point-face-bgcolor 'unspecified))
;;                 (setq point-face-bgcolor 'default))))
;;         (setq point-face-bgcolor 'default))
;;       (when (not point-face-bgcolor-hex)  ; not read from cache
;;         (if (eq point-face-bgcolor 'default)
;;             (setq point-face-bgcolor-hex 'default)
;;           ;; else
;;           (setq point-face-bgcolor-hex
;;                 (highlight-tail-hex-from-colorname point-face-bgcolor))
;;           (setq highlight-tail-nonhtfaces-bgcolors
;;                 (cons (cons point-face point-face-bgcolor-hex)
;;                       highlight-tail-nonhtfaces-bgcolors))
;;           (highlight-tail-add-colors-fade-table point-face-bgcolor-hex)
;;           (highlight-tail-make-faces
;;            (highlight-tail-get-colors-fade-table-with-key
;;             point-face-bgcolor-hex))))
;;       ;; return value
;;       point-face-bgcolor-hex)))

;; beacon-mode
(with-eval-after-load "beacon-autoloads"
  (beacon-mode 1))

;; rainbow-identifiers
;; https://www.reddit.com/r/emacs/comments/6xpzx6/subword_syntax_highlighting_with/
;; (with-eval-after-load "rainbow-identifiers-autoloads"
;;   (defun rainbow-identifiers--matcher (end)
;;     "The matcher function to be used by font lock mode."
;;     (catch 'rainbow-identifiers--matcher
;;       (while (re-search-forward (rx word-start (*? any) word-end) end t)
;;         (let ((beginning (match-beginning 0))
;;               (end (match-end 0)))
;;           (when (run-hook-with-args-until-failure 'rainbow-identifiers-filter-functions beginning end)
;;             (let* ((identifier (buffer-substring-no-properties beginning end))
;;                    (hash (rainbow-identifiers--hash-function identifier)))
;;               (setq rainbow-identifiers--face (funcall rainbow-identifiers-choose-face-function hash))
;;               (throw 'rainbow-identifiers--matcher t)))))
;;       nil)))

;; rainbow-mode
(require 'rainbow-mode)
(with-eval-after-load "rainbow-mode"
  (add-hook 'my/-prog-mode-hook #'(lambda () (rainbow-mode 1)))
  (add-hook 'text-mode-hook #'(lambda () (rainbow-mode 1)))
  (add-hook 'my/-find-large-file-hook #'(lambda () (rainbow-mode -1))))


;; highlight-parentheses
(with-eval-after-load "highlight-parentheses-autoloads"
  (setq hl-paren-background-colors
        #'(lambda ()
            (if (eq (frame-parameter (selected-frame) 'background-mode)
                    'light)
                '("#FFF" "#DDCCDD" "#CCDDDD")
              '("#333" "#444" "#555"))))
  (setq hl-paren-colors '("#FF0000" "#FF00FF" "#00FFFF"))
  (setq hl-paren-attributes '((:height 1.1 :weight bold)))
  (global-highlight-parentheses-mode t)
  (add-hook 'my/-find-large-file-hook
            #'(lambda () (highlight-parentheses-mode -1))))

;; ;; highlight-blocks-mode
;; do not enable it as it's too slow
;; (with-eval-after-load "highlight-blocks-autoloads"
;;   (add-hook 'my/-prog-mode-hook #'(lambda () (highlight-blocks-mode 1)))
;;   (add-hook 'activate-mark-hook #'(lambda () (highlight-blocks-mode -1)))
;;   (add-hook 'deactivate-mark-hook #'(lambda () (highlight-blocks-mode 1))))

;; ;; smart-modeline
;; (with-eval-after-load "smart-mode-line-autoloads"
;;   (sml/setup))

;; prompt-text-el
(with-eval-after-load "prompt-text-autoloads"
  (setq prompt-text-format
        `("[" (:eval (symbol-name this-command)) "] "))
  (prompt-text-mode))

;; wrap-with
(with-eval-after-load "wrap-with"
  (wrap-with-mode t))
(require 'wrap-with nil t)

;; smartparens
(with-eval-after-load "smartparens"
  (setq sp-show-pair-delay 0.2
        ;; fix paren highlighting in normal mode
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil))

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
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil)

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
      (cons (+ col (window-hscroll) company-col-offset)
            (+ row company-row-offset))))

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
      (when (and global-company-mode
                 (or (eq this-command #'execute-extended-command)
                     (eq this-command #'eval-expression)))

        (setq-local company-minibuffer-mode this-command)

        (setq-local completion-at-point-functions
                    (list (if (fboundp 'elisp-completion-at-point)
                              #'elisp-completion-at-point
                            #'lisp-completion-at-point)
                          t))

        (setq-local company-show-numbers nil)
        (setq-local company-backends '((company-elisp-minibuffer company-capf)))
        (setq-local company-tooltip-limit 8)
        (setq-local company-col-offset 1)
        (setq-local company-row-offset 1)
        (setq-local company-frontends
                    '(company-pseudo-tooltip-unless-just-one-frontend
                      company-preview-if-just-one-frontend))

        (company-mode 1)
        (when (eq this-command #'execute-extended-command)
          (company-complete)))))

  (add-hook 'minibuffer-setup-hook #'minibuffer-company)
  ;;(remove-hook 'minibuffer-setup-hook #'minibuffer-company)
  ;;(add-hook 'eval-expression-minibuffer-setup-hook #'minibuffer-company)
  ;; (with-eval-after-load "company-flx-autoloads"
  ;; (company-flx-mode)
  ;; (with-eval-after-load "readline-complete-autoloads"
  ;;   (require 'readline-complete)
  ;;   ;; bind "set completion-query-items 0"
  ;;   ;; bind "set page-completions off"
  ;;   (push '("^constantin@zxnotdead:.+?$ " ac-prefix-rlc-shell) ac-rlc-prompts)
  ;;   (push '("^\$[[:space:]]" ac-prefix-rlc-shell) ac-rlc-prompts)
  ;;   (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  ;;   (push 'company-readline company-backends))
  )


;; ;; auto-complete-mode
;; (when (< emacs-major-version 24)
;;   (with-eval-after-load "auto-complete-autoloads"
;;     (require 'auto-complete)
;;     (require 'auto-complete-config)
;;     (add-to-list 'ac-dictionary-directories
;;                  (concat my/-conf-path "auto-complete/dict"))
;;     ;;(require 'auto-complete-clang)
;;     ;;(require 'go-autocomplete)
;;     (ac-config-default)
;;     (setq clang-completion-suppress-error t
;;           ac-clang-flags (mapcar #'(lambda (item)(concat "-I" item))
;;                                  (my/-c-get-includes)))

;;     (global-auto-complete-mode t)          ;enable global-mode
;;     (setq ac-auto-start 2                  ;automatically start (disabled)
;;           ac-dwim t                        ;Do what i mean
;;           ac-override-local-map nil        ;don't override local map
;;           ac-use-quick-help nil ac-quick-help-delay 1.5
;;           ac-use-menu-map t ac-auto-show-menu 0.5
;;           ac-ignore-case t ac-delay 0.5 ac-use-fuzzy t ac-use-comphist t)
;;     (custom-set-variables
;;      '(ac-sources
;;        '(;;ac-source-filename
;;          ac-source-files-in-current-dir ;;ac-source-words-in-buffer
;;          ac-source-words-in-same-mode-buffers
;;          ;;ac-source-yasnippet ac-source-words-in-all-buffer ac-source-gtags
;;          ;;ac-source-imenu ac-source-abbrev ac-source-semantic
;;          ;;ac-source-semantic-raw ac-source-ropemacs ac-source-symbols
;;          )))

;;     (dolist (hook '(emacs-lisp-mode-hook
;;                     inferior-emacs-lisp-mode
;;                     lisp-mode-hook lisp-interaction-mode-hook))
;;       (add-hook hook #'(lambda () (add-to-list 'ac-sources 'ac-source-symbols))))
;;     (add-hook 'haskell-mode-hook
;;               #'(lambda ()
;;                   (add-to-list 'ac-sources 'ac-source-haskell)))
;;     (add-hook 'c-mode-common-hook
;;               #'(lambda ()
;;                   ;;(setq ac-sources '(ac-source-clang ac-source-yasnippet))
;;                   (add-to-list 'ac-sources 'ac-source-clang)
;;                   ;;(setq ac-sources '(ac-source-semantic))
;;                   ))
;;     (ac-flyspell-workaround)))

;; anzu
;; (with-eval-after-load "anzu-autoloads"
;;   (global-set-key [remap query-replace] 'anzu-query-replace)
;;   (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

;; visual-regexp
(with-eval-after-load "visual-regexp-autoloads"
  (global-set-key (kbd "C-c r") 'vr/replace)
  (global-set-key (kbd "C-c q") 'vr/query-replace)
  (global-set-key (kbd "C-c m") 'vr/mc-mark))

;; speedbar
(with-eval-after-load "speedbar"
  (push (cons 'persp-ignore-wconf t) speedbar-frame-parameters))

;; which-key
(with-eval-after-load "which-key-autoloads"
  (which-key-mode))

;; ivy
(with-eval-after-load "ivy-autoloads"
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate "full"
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (ivy-mode 1)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)

  ;;(require 'lv)
  ;; (defun ivy-display-function-lv (text)
  ;;   (let ((lv-force-update t))
  ;;     (lv-message
  ;;      (if (string-match "\\`\n" text)
  ;;          (substring text 1)
  ;;        text))))
  ;; (setq ivy-display-function 'ivy-display-function-lv)

  (with-eval-after-load "grep"
    (setq grep-read-files-original (symbol-function #'grep-read-files))
    (fset 'grep-read-files
          #'(lambda (regexp)
              (let ((ret
                     (funcall grep-read-files-original regexp)))
                (file-name-nondirectory ret)))))
  (with-eval-after-load "ivy-rich-autoloads"
    (ivy-rich-mode)))

;; counsel
(with-eval-after-load "counsel-autoloads"
  (setq counsel-find-file-at-point t)

  (autoload 'rgrep-default-command "grep")
  (defvar counsel-rgrep-last-cmd ""
    "Last command generated in counsel-rgrep")

  (defun my/counsel-rgrep-action (x)
    "Go to occurrence X in current Git repository."
    (when (string-match "\\`\\(.*?\\)\0\\([0-9]+\\):\\(.*\\)\\'" x)
      (let ((file-name (match-string-no-properties 1 x))
            (line-number (match-string-no-properties 2 x)))
        (find-file (expand-file-name
                    file-name
                    (ivy-state-directory ivy-last)))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line-number)))
        (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (swiper--ensure-visible)
        (run-hooks 'counsel-grep-post-action-hook)
        (unless (eq ivy-exit 'done)
          (swiper--cleanup)
          (swiper--add-overlays (ivy--regex ivy-text))))))

  (defun counsel-rgrep (&optional zgrep-p)
    (interactive "P")
    (require 'counsel)
    ;; (ivy-set-prompt 'counsel-rgrep #'counsel-prompt-function-default)
    (let ((file-name-pattern
           (read-string "File name pattern: " "*" nil "*"))
          (grep-program (if zgrep-p "zgrep" "grep"))
          (counsel--git-grep-dir (expand-file-name "./")))
      (ivy-read (if zgrep-p "zrgrep" "rgrep")
                (apply-partially
                 #'(lambda (dir file-name-pattern grep-progam string)
                     (if (< (length string) 3)
                         (ivy-more-chars)
                       (let ((regex ;; (counsel-unquote-regex-parens
                              ;;  (setq ivy--old-re
                              ;;        (ivy--regex string)))
                              string)
                             grep-find-template
                             grep-find-command
                             grep-host-defaults-alist)
                         (grep-compute-defaults)
                         (setq counsel-rgrep-last-cmd
                               (concat
                                (let (grep-highlight-matches)
                                 (rgrep-default-command regex file-name-pattern dir))
                                (when (string= "zgrep" grep-program)
                                  " || true")))
                         (counsel--async-command counsel-rgrep-last-cmd)
                         nil)))
                 "./" file-name-pattern grep-program)
                :dynamic-collection t
                :keymap counsel-ag-map
                :history 'counsel-git-grep-history
                :re-builder #'ivy--regex
                ;; :action #'counsel-git-grep-action
                :action #'my/counsel-rgrep-action
                :unwind (lambda ()
                          (counsel-delete-process)
                          (swiper--cleanup))
                :caller 'counsel-rgrep)))
  (ivy-set-display-transformer 'counsel-rgrep 'counsel-git-grep-transformer)

  (defun counsel-rgrep-occur ()
    "Generate a custom occur buffer for `counsel-rgrep'.
When REVERT is non-nil, regenerate the current *ivy-occur* buffer."
    (unless (eq major-mode 'ivy-occur-grep-mode)
      (ivy-occur-grep-mode))
    (setq default-directory counsel--git-grep-dir)
    (let* ((cmd-out (shell-command-to-string counsel-rgrep-last-cmd))
           (cands (split-string cmd-out "\n" t)))
      ;; Need precise number of header lines for `wgrep' to work.
      (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                      default-directory))
      (insert (format "%d candidates:\n" (length cands)))
      (ivy--occur-insert-lines cands)))

  (ivy-set-occur 'counsel-rgrep 'counsel-rgrep-occur)
  ;; (push '(counsel-git-grep . ivy--regex-plus) ivy-re-builders-alist)

  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-load-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-rgrep)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(with-eval-after-load "swiper-autoloads"
  (setq swiper-action-recenter t)
  (global-set-key "\C-s" 'swiper))


;; helm-dash
(with-eval-after-load "helm-dash-autoloads"
  (add-hook 'my/-prog-mode-hook #'helm-dash-set-apropriate-major-mode-docsets)
  (add-hook 'conf-mode-hook #'helm-dash-set-apropriate-major-mode-docsets))

;; ;; helm
;; ;; (with-eval-after-load "helm-autoloads"
;; ;;   (add-hook 'my/-packages-initialized-hook (lambda () (require 'helm-config))))
;; (with-eval-after-load "helm-autoloads";"helm-config"
;;   (global-set-key (kbd "C-c h") 'helm-command-prefix)

;;   (setq helm-split-window-in-side-p t
;;         helm-move-to-line-cycle-in-source t
;;         helm-echo-input-in-header-line t
;;         helm-ff-guess-ffap-filenames t
;;         helm-buffer-max-length nil)

;;   (with-eval-after-load "golden-ratio-autoloads"
;;     (push #'helm-alive-p golden-ratio-inhibit-functions))

;;   (global-set-key (kbd "M-x") #'helm-M-x)
;;   (setq helm-M-x-fuzzy-match t)

;;   (global-set-key (kbd "M-y") #'helm-show-kill-ring)

;;   (global-set-key (kbd "C-x b") #'helm-mini)
;;   (setq helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match t)

;;   (global-set-key (kbd "C-x C-f") #'helm-find-files)

;;   (helm-mode 1)
;;   ;; (helm-autoresize-mode t)
;;   (setq helm-autoresize-max-height 30)

;;   (defvar helm-source-header-default-background
;;     (face-attribute 'helm-source-header :background))
;;   (defvar helm-source-header-default-foreground
;;     (face-attribute 'helm-source-header :foreground))
;;   (defvar helm-source-header-default-box
;;     (face-attribute 'helm-source-header :box))

;;   (defun helm-toggle-header-line ()
;;     (if (> (length helm-sources) 1)
;;         (set-face-attribute 'helm-source-header
;;                             nil
;;                             :foreground helm-source-header-default-foreground
;;                             :background helm-source-header-default-background
;;                             :box helm-source-header-default-box
;;                             :height 1.0)
;;       (set-face-attribute
;;        'helm-source-header
;;        nil
;;        :foreground (face-attribute 'helm-selection :background)
;;        :background (face-attribute 'helm-selection :background)
;;        :box nil
;;        :height 0.1)))
;;   (add-hook 'helm-before-initialize-hook #'helm-toggle-header-line)
;;   ;;(helm-flx-mode)
;;   )

;; (with-eval-after-load "scroll-restore-autoloads"
;;   (setq scroll-restore-jump-back t
;;         scroll-restore-handle-cursor t
;;         scroll-restore-handle-region t)
;;   ;; (scroll-restore-mode)
;;   )


;; projectile
;; (with-eval-after-load "projectile-autoloads"
;;   (setq projectile-keymap-prefix (kbd "C-x p"))
;;   (projectile-global-mode)
;;   ;; (with-eval-after-load "helm-projectile-autoloads"
;;   ;;   (helm-projectile-on))
;;   )

;; persp-mode
(with-eval-after-load "persp-mode-autoloads"
  (defvar after-switch-to-buffer-functions nil)
  (defvar after-find-file-functions nil)

  (when (fboundp 'advice-add)
    (defvar after-switch-to-buffer-adv-suspend nil)
    (defun after-switch-to-buffer-adv (&rest args)
      (unless after-switch-to-buffer-adv-suspend
        (apply #'run-hook-with-args 'after-switch-to-buffer-functions args)))
    (advice-add #'switch-to-buffer :after #'after-switch-to-buffer-adv)
    (defun after-find-file-adv (&rest args)
      (apply #'run-hook-with-args 'after-find-file-functions args))
    (advice-add #'find-file :after #'after-find-file-adv))

  (setq wg-morph-on nil)
  ;;(setq windmove-window-distance-delta 2)
  (unless (>= emacs-major-version 24)
    (setq persp-when-kill-switch-to-buffer-in-perspective nil))
  (setq persp-autokill-buffer-on-remove 'kill-weak)

  (setq command-switch-alist
        (cons
         (cons "persp-q"
               #'(lambda (p)
                   (setq persp-auto-resume-time -1
                         persp-auto-save-opt 0)))
         command-switch-alist))

  ;; (with-eval-after-load "dired"
  ;;   (persp-def-auto-persp "dired"
  ;;     :parameters '((dont-save-to-file . t))
  ;;     :mode 'dired-mode
  ;;     :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
  ;;                persp-add-buffer-on-find-file
  ;;                persp-add-buffer-on-after-change-major-mode)
  ;;     :hooks '(after-switch-to-buffer-functions)
  ;;     :weak t
  ;;     :switch 'window))

  ;; (persp-def-buffer-save/load
  ;;  :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
  ;;  :save-vars '(major-mode default-directory))
  ;; (persp-def-buffer-save/load
  ;;  :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
  ;;  :save-vars '(major-mode default-directory compilation-directory
  ;;                          compilation-environment compilation-arguments))
  ;; (with-eval-after-load "magit-autoloads"
  ;;   (autoload 'magit-status-mode "magit")
  ;;   (autoload 'magit-refresh "magit")
  ;;   (persp-def-buffer-save/load
  ;;    :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
  ;;    :save-vars '(major-mode default-directory)
  ;;    :after-load-function #'(lambda (b &rest _)
  ;;                             (with-current-buffer b (magit-refresh)))))

  ;; (persp-def-buffer-save/load
  ;;  :mode 'shell-mode
  ;;  :mode-restore-function #'(lambda (_mode) (shell)) ; or #'identity if you do not want to start shell process
  ;;  :tag-symbol 'def-shell
  ;;  :save-vars '(major-mode default-directory))


  ;; (with-eval-after-load "persp-mode"
  ;;   (defvar persp-mode-projectile-bridge-before-switch-selected-window-buffer nil)

  ;;   ;; (setq persp-add-buffer-on-find-file 'if-not-autopersp)

  ;;   (persp-def-auto-persp "projectile"
  ;;     :parameters '((dont-save-to-file . t)
  ;;                   (persp-mode-projectile-bridge . t))
  ;;     :hooks '(projectile-before-switch-project-hook
  ;;              projectile-after-switch-project-hook
  ;;              projectile-find-file-hook
  ;;              find-file-hook)
  ;;     :dyn-env '((after-switch-to-buffer-adv-suspend t))
  ;;     :switch 'frame
  ;;     :predicate
  ;;     #'(lambda (buffer &optional state)
  ;;         (if (eq 'projectile-before-switch-project-hook
  ;;                 (alist-get 'hook state))
  ;;             state
  ;;           (and
  ;;            projectile-mode
  ;;            (buffer-live-p buffer)
  ;;            (buffer-file-name buffer)
  ;;            ;; (not git-commit-mode)
  ;;            (projectile-project-p)
  ;;            (or state t))))
  ;;     :get-name
  ;;     #'(lambda (state)
  ;;         (if (eq 'projectile-before-switch-project-hook
  ;;                 (alist-get 'hook state))
  ;;             state
  ;;           (push (cons 'persp-name
  ;;                       (concat "p) "
  ;;                               (with-current-buffer (alist-get 'buffer state)
  ;;                                 (projectile-project-name))))
  ;;                 state)
  ;;           state))
  ;;     :on-match
  ;;     #'(lambda (state)
  ;;         (let ((hook (alist-get 'hook state))
  ;;               (persp (alist-get 'persp state))
  ;;               (buffer (alist-get 'buffer state)))
  ;;           (case hook
  ;;             (projectile-before-switch-project-hook
  ;;              (let ((win (if (minibuffer-window-active-p (selected-window))
  ;;                             (minibuffer-selected-window)
  ;;                           (selected-window))))
  ;;                (when (window-live-p win)
  ;;                  (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer
  ;;                        (window-buffer win)))))

  ;;             (projectile-after-switch-project-hook
  ;;              (when (buffer-live-p
  ;;                     persp-mode-projectile-bridge-before-switch-selected-window-buffer)
  ;;                (let ((win (selected-window)))
  ;;                  (unless (eq (window-buffer win)
  ;;                              persp-mode-projectile-bridge-before-switch-selected-window-buffer)
  ;;                    (set-window-buffer
  ;;                     win persp-mode-projectile-bridge-before-switch-selected-window-buffer)))))

  ;;             (find-file-hook
  ;;              (setcdr (assq :switch state) nil)))
  ;;           (if (case hook
  ;;                 (projectile-before-switch-project-hook nil)
  ;;                 (t t))
  ;;               (persp--auto-persp-default-on-match state)
  ;;             (setcdr (assq :after-match state) nil)))
  ;;         state)
  ;;     :after-match
  ;;     #'(lambda (state)
  ;;         (when (eq 'find-file-hook (alist-get 'hook state))
  ;;           (run-at-time 0.5 nil
  ;;                        #'(lambda (buf persp)
  ;;                            (when (and (eq persp (get-current-persp))
  ;;                                       (not (eq buf (window-buffer
  ;;                                                     (selected-window)))))
  ;;                              ;; (switch-to-buffer buf)
  ;;                              (persp-add-buffer buf persp t nil)))
  ;;                        (alist-get 'buffer state)
  ;;                        (get-current-persp)))
  ;;         (persp--auto-persp-default-after-match state)))

  ;;   ;; (add-hook 'persp-after-load-state-functions
  ;;   ;;           #'(lambda (&rest args) (persp-auto-persps-pickup-buffers)) t)
  ;;   )

  ;; (with-eval-after-load "persp-mode-projectile-bridge-autoloads"
  ;;   (add-hook 'persp-mode-projectile-bridge-mode-hook
  ;;             #'(lambda ()
  ;;                 (if persp-mode-projectile-bridge-mode
  ;;                     (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
  ;;                   (persp-mode-projectile-bridge-kill-perspectives))))
  ;;   (add-hook 'window-setup-hook
  ;;             #'(lambda ()
  ;;                 (persp-mode-projectile-bridge-mode 1))
  ;;             t))

  (add-hook
   'window-setup-hook
   #'(lambda () (persp-mode 1)
            ;; (add-hook 'persp-after-load-state-functions
            ;;           #'(lambda (&rest args) (persp-auto-persps-pickup-buffers))
            ;;           t)
            (global-set-key (kbd "C-x k") #'persp-kill-buffer)
            ;; (with-eval-after-load "helm"
            ;;   (persp-update-completion-system 'completing-read))
            )))

;; (with-eval-after-load "persp-mode"
;;   (with-eval-after-load "helm-mode"
;;     (defun helm-buffers-toggle-persp-filter ()
;;       (interactive)
;;       (with-helm-alive-p
;;         (let ((filter-attrs (helm-attr 'candidate-transformer
;;                                        helm-source-persp-buffers)))
;;           (if (memq #'helm-persp-buffers-filter-transformer filter-attrs)
;;               (progn
;;                 (helm-attrset 'candidate-transformer
;;                               (delq #'helm-persp-buffers-filter-transformer
;;                                     filter-attrs)
;;                               helm-source-persp-buffers t)
;;                 (helm-attrset 'name
;;                               "Buffers"
;;                               helm-source-persp-buffers t)
;;                 (setq helm-persp-filtered-buffers-cache nil))
;;             (helm-attrset 'candidate-transformer
;;                           (cons #'helm-persp-buffers-filter-transformer
;;                                 filter-attrs)
;;                           helm-source-persp-buffers t)
;;             (helm-attrset 'name
;;                           "Current perspective buffers"
;;                           helm-source-persp-buffers t))
;;           (helm-force-update))))
;;     (put 'helm-buffers-toggle-persp-filter 'helm-only t)
;;     (define-key helm-buffer-map
;;       persp-toggle-read-persp-filter-keys #'helm-buffers-toggle-persp-filter)
;;     (defvar helm-persp-filtered-buffers-cache nil)
;;     (defun helm-persp-buffers-filter-transformer (candidates)
;;       (setq helm-persp-filtered-buffers-cache nil)
;;       (let* ((persp (get-current-persp))
;;              (ret
;;               (cl-remove-if-not
;;                #'(lambda (bn)
;;                    (let* ((ret (persp-contain-buffer-p (get-buffer bn) persp)))
;;                      (unless ret
;;                        (push bn helm-persp-filtered-buffers-cache))
;;                      ret))
;;                candidates)))
;;         ret))
;;     (defclass helm-persp-buffers-source (helm-source-buffers)
;;       ((buffer-list
;;         :initarg :buffer-list
;;         :initform #'(lambda () (mapcar #'buffer-name (buffer-list)))
;;         :custom function
;;         :documentation
;;         " A function with no arguments to create buffer list.")
;;        (cleanup :initform #'(lambda ()
;;                               (setq helm-persp-filtered-buffers-cache nil
;;                                     helm-buffers-list-cache nil)))
;;        (candidate-transformer :initform '(helm-persp-buffers-filter-transformer))))
;;     (defvar helm-source-persp-buffers
;;       (helm-make-source "Current perspective buffers"
;;           'helm-persp-buffers-source
;;         :fuzzy-match t))
;;     (defclass helm-persp-filtered-buffers-source (helm-source-buffers)
;;       ((candidates
;;         :initform
;;         #'(lambda ()
;;             (if helm-persp-filtered-buffers-cache
;;                 helm-persp-filtered-buffers-cache
;;               (setq helm-persp-filtered-buffers-cache
;;                     (mapcar
;;                      #'buffer-name
;;                      (persp-buffer-list-restricted nil 1))))))
;;        (cleanup :initform #'(lambda ()
;;                               (setq helm-persp-filtered-buffers-cache nil)))))
;;     (defvar helm-source-persp-filtered-buffers
;;       (helm-make-source "Other buffers"
;;           'helm-persp-filtered-buffers-source
;;         :fuzzy-match t))
;;     (defun helm-persp-buffer-list-bridge
;;         (prompt _collection &optional
;;                 test _require-match init hist default _inherit-im name buffer)
;;       (let ((dflt (or default ""))
;;             (cbuf (current-buffer))
;;             helm-candidate-number-limit)
;;         (or
;;          (helm :sources
;;                (cond
;;                 ((eq this-command 'persp-add-buffer)
;;                  '(helm-source-persp-filtered-buffers))
;;                 ((eq this-command 'persp-remove-buffer)
;;                  '(helm-source-persp-buffers))
;;                 (t
;;                  '(helm-source-persp-buffers helm-source-persp-filtered-buffers)))
;;                :fuzzy-match helm-mode-fuzzy-match
;;                :prompt prompt
;;                :buffer buffer
;;                :input init
;;                :history hist
;;                :resume 'noresume
;;                :keymap helm-buffer-map
;;                :truncate-lines helm-buffers-truncate-lines
;;                :default dflt
;;                :preselect
;;                #'(lambda ()
;;                    (if (and (eq this-command 'persp-temporarily-display-buffer)
;;                             (persp-contain-buffer-p cbuf))
;;                        (helm-next-source)
;;                      (helm-mark-current-line)
;;                      (let ((buffer-name-truncated-regexp
;;                             (helm-buffers--quote-truncated-buffer cbuf))
;;                            (start (point)) mp)
;;                        (helm-awhile (re-search-forward
;;                                      buffer-name-truncated-regexp nil t)
;;                          (when (helm-pos-header-line-p) (forward-line 1))
;;                          (helm-mark-current-line)
;;                          (when (eq cbuf (helm-get-selection))
;;                            (cl-return (setq mp it))))
;;                        (goto-char (or mp start))
;;                        (helm-mark-current-line)))))
;;          (helm-mode--keyboard-quit))))
;;     (defvar helm-persp-mini-default-sources
;;       (cons 'helm-source-persp-buffers
;;             (cons 'helm-source-persp-filtered-buffers
;;                   (remove 'helm-source-buffers-list helm-mini-default-sources))))
;;     (defun helm-persp-mini ()
;;       (interactive)
;;       (let* ((cbuf (current-buffer))
;;              (cbuf-name (buffer-name cbuf))
;;              helm-candidate-number-limit)
;;         (or
;;          (helm :sources helm-persp-mini-default-sources
;;                :ff-transformer-show-only-basename nil
;;                :fuzzy-match helm-mode-fuzzy-match
;;                :buffer "*helm persp mini*"
;;                :keymap helm-buffer-map
;;                :truncate-lines helm-buffers-truncate-lines
;;                :default cbuf-name
;;                :preselect #'(lambda ()
;;                               (let ((buffer-name-truncated-regexp
;;                                      (helm-buffers--quote-truncated-buffer cbuf))
;;                                     (start (point)) mp)
;;                                 (helm-awhile (re-search-forward
;;                                               buffer-name-truncated-regexp nil t)
;;                                   (when (helm-pos-header-line-p) (forward-line 1))
;;                                   (helm-mark-current-line)
;;                                   (when (eq cbuf (helm-get-selection))
;;                                     (cl-return (setq mp it))))
;;                                 (goto-char (or mp start))
;;                                 (helm-mark-current-line))))
;;          (helm-mode--keyboard-quit))))
;;     (global-set-key (kbd "C-x b") #'helm-persp-mini)
;;     (setq helm-completing-read-handlers-alist
;;           (append
;;            '((switch-to-buffer                 . helm-persp-buffer-list-bridge)
;;              (persp-switch-to-buffer           . helm-persp-buffer-list-bridge)
;;              (kill-buffer                      . helm-persp-buffer-list-bridge)
;;              (persp-kill-buffer                . helm-persp-buffer-list-bridge)
;;              (persp-temporarily-display-buffer . helm-persp-buffer-list-bridge)
;;              (persp-add-buffer                 . helm-persp-buffer-list-bridge)
;;              (persp-remove-buffer              . helm-persp-buffer-list-bridge))
;;            helm-completing-read-handlers-alist))))

(with-eval-after-load "persp-mode"
  (with-eval-after-load "persp-fr-autoloads"
    (setq persp-fr-title-prefix "")
    (persp-fr-start)))

(with-eval-after-load "persp-mode"
  (add-hook 'persp-before-switch-functions
            #'(lambda (new-persp-name w-or-f)
                (let ((cur-persp-name (safe-persp-name (get-current-persp))))
                  (when (member cur-persp-name persp-names-cache)
                    (setq persp-names-cache
                          (cons cur-persp-name
                                (delete cur-persp-name persp-names-cache)))))))

  (add-hook 'persp-renamed-functions
            #'(lambda (persp old-name new-name)
                (setq persp-names-cache
                      (cons new-name (delete old-name persp-names-cache)))))

  (add-hook 'persp-before-kill-functions
            #'(lambda (persp)
                (setq persp-names-cache
                      (delete (safe-persp-name persp) persp-names-cache))))

  (add-hook 'persp-created-functions
            #'(lambda (persp phash)
                (when (and (eq phash *persp-hash*)
                           (not (member (safe-persp-name persp)
                                        persp-names-cache)))
                  (setq persp-names-cache
                        (cons (safe-persp-name persp) persp-names-cache))))))


(with-eval-after-load "persp-mode"
  (with-eval-after-load "ivy"
    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil))))))

(with-eval-after-load "persp-mode"
  (require 'erc)
  (with-eval-after-load "erc"
    (persp-def-buffer-save/load
     :mode 'erc-mode :tag-symbol 'def-erc-server
     :save-vars '("^erc-session-.+" "^erc-server-.+")
     :save-function #'(lambda (buffer tag lvars)
                        (if (get-buffer-process buffer)
                            (progn
                              (push (cons 'persp-erc-chans
                                          (mapcar #'buffer-name
                                                  (erc-channel-list
                                                   (get-buffer-process buffer))))
                                    lvars)
                              (push (cons 'persp-erc-persp-name
                                          (car (buffer-local-value
                                                'persp-buffer-in-persps
                                                buffer)))
                                    lvars)
                              (list tag (buffer-name buffer) lvars))
                          'skip))
     :after-load-function
     #'(lambda (erc-buf &rest _other)
         (lexical-let (chans
                       erc-persp-name erc-persp (erc-buf erc-buf) initial-persp
                       erc-window
                       persp-erc-after-connect-lambda persp-erc-join-lambda)
           (setq persp-erc-after-connect-lambda
                 #'(lambda (ntwrk nck)
                     (if (and (window-live-p erc-window)
                              (eq erc-buf (window-buffer erc-window)))
                         (select-window erc-window)
                       (setq erc-window (selected-window))
                       (set-window-buffer erc-window erc-buf))
                     (add-hook 'erc-server-JOIN-functions persp-erc-join-lambda
                               t)
                     (mapc #'(lambda (chan)
                               (with-current-buffer erc-buf
                                 (persp-add-buffer (erc-join-channel chan nil)
                                                   erc-persp)))
                           chans)
                     (remove-hook 'erc-after-connect
                                  persp-erc-after-connect-lambda)
                     nil)
                 persp-erc-join-lambda
                 #'(lambda (proc parsed)
                     (if chans
                         (when (eq proc (get-buffer-process erc-buf))
                           (let ((chan (erc-response.contents parsed)))
                             (when (member chan chans)
                               (setq chans (delete chan chans))
                               (when erc-persp (persp-add-buffer chan erc-persp))
                               (unless chans
                                 (remove-hook 'erc-server-JOIN-functions
                                              persp-erc-join-lambda)
                                 ;; (persp-frame-switch
                                 ;;  (safe-persp-name initial-persp))
                                 ))))
                       (remove-hook 'erc-server-JOIN-functions
                                    persp-erc-join-lambda))
                     nil))
           (with-current-buffer erc-buf
             (setq chans persp-erc-chans
                   erc-persp-name persp-erc-persp-name))
           (when erc-persp-name
             (setq erc-persp (persp-get-by-name erc-persp-name))
             (setq initial-persp (get-current-persp))
             (persp-frame-switch erc-persp-name))
           (setq erc-window (get-buffer-window erc-buf (selected-frame)))
           (if (window-live-p erc-window)
               (select-window erc-window)
             (setq erc-window (selected-window))
             (set-window-buffer erc-window erc-buf))
           (add-hook 'erc-after-connect persp-erc-after-connect-lambda t)
           (with-current-buffer erc-buf
             (erc-server-reconnect)
             (persp-special-last-buffer-make-current)))))))

;; (with-eval-after-load "persp-mode"

;;   (defun persp-remove-killed-buffers ()
;;     (interactive)
;;     (mapc #'(lambda (p)
;;               (when p
;;                 (setf (persp-buffers p)
;;                       (delete-if-not #'buffer-live-p
;;                                      (persp-buffers p)))))
;;           (persp-persps)))


;;   (defvar persp-trace-buffers-hash (make-hash-table :test #'equal))

;;   (add-hook 'persp-renamed-functions
;;             #'(lambda (persp old-name new-name)
;;                 (let ((bufs (gethash old-name persp-trace-buffers-hash)))
;;                   (remhash old-name persp-trace-buffers-hash)
;;                   (puthash new-name bufs persp-trace-buffers-hash))))

;;   (add-hook 'persp-before-kill-functions
;;             #'(lambda (persp)
;;                 (remhash (safe-persp-name persp) persp-trace-buffers-hash)))

;;   (add-hook 'persp-created-functions
;;             #'(lambda (persp phash)
;;                 (when (eq phash *persp-hash*)
;;                   (puthash (safe-persp-name persp) nil persp-trace-buffers-hash))))

;;   (defun persp--check-for-killed-buffers (&optional persp)
;;     (when persp-mode
;;       (if persp
;;           (let ((known-buffers (append (gethash (persp-name persp)
;;                                                 persp-trace-buffers-hash)
;;                                        nil)))
;;             (dolist (buf (persp-buffers persp))
;;               (if (not (buffer-live-p buf))
;;                   (message
;;                    "[persp-mode] Warning: Found killed buffer in the %s perspective."
;;                    (persp-name persp))
;;                 (setq known-buffers (delete (buffer-name buf) known-buffers))))
;;             (when known-buffers
;;               (message
;;                "[persp-mode] Warning: These %s buffers was killed, but not \
;; removed from the %s perspective"
;;                known-buffers (persp-name persp))))
;;         (mapc #'persp--check-for-killed-buffers (delq nil (persp-persps))))))

;;   (defun* persp-add-buffer-after-adv (buff-or-name &optional persp &rest _rargs)
;;     (when persp
;;       (let* ((buf (get-buffer buff-or-name))
;;              (buf-name (buffer-name buf))
;;              (known-buffers (gethash (persp-name persp)
;;                                      persp-trace-buffers-hash)))
;;         (when (and (memq buf (persp-buffers persp))
;;                    (not (member buf-name known-buffers)))
;;           (puthash (persp-name persp) (cons buf-name known-buffers)
;;                    persp-trace-buffers-hash)))
;;       (persp--check-for-killed-buffers)))
;;   (defun* persp-remove-buffer-after-adv
;;       (buff-or-name &optional persp &rest _rargs)
;;     (when persp
;;       (puthash (persp-name persp)
;;                (delete (buffer-name (get-buffer buff-or-name))
;;                        (gethash (persp-name persp) persp-trace-buffers-hash))
;;                persp-trace-buffers-hash)
;;       (persp--check-for-killed-buffers)))

;;   (advice-add #'persp-add-buffer    :after #'persp-add-buffer-after-adv)
;;   (advice-add #'persp-remove-buffer :after #'persp-remove-buffer-after-adv)

;;   (run-at-time t 30 #'persp--check-for-killed-buffers))
