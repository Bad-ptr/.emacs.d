;;; 0-rc.el --- basic emacs settings


;;; Code:


;;==================================
;; Encoding

(set-language-environment "UTF-8")
(setq read-quoted-char-radix 10)

(define-coding-system-alias 'windows-1251 'cp1251)
;; Setting up encoding autodetection
;; prefer-coding-system place it's argument to beginning of list
;; So the first encoding here will be utf-8-unix
(prefer-coding-system 'utf-8)
(prefer-coding-system 'koi8-r-unix)
(prefer-coding-system 'windows-1251-dos)
(prefer-coding-system 'cp866-dos)
(prefer-coding-system 'utf-8-unix)

(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(setq locale-coding-system   'utf-8)
(set-selection-coding-system 'utf-8)

(setq buffer-file-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;==================================


;; Large files

(defvar my/-find-large-file-hook nil
  "Hooks to be run when you open large file(see `large-file-warning-threshold').")

(add-hook 'find-file-hook
          #'(lambda ()
              (when (>= (buffer-size) large-file-warning-threshold)
                (run-hooks 'my/-find-large-file-hook))))

(add-hook 'my/-find-large-file-hook
          #'(lambda ()
              (fundamental-mode)
              (font-lock-mode nil)
              (jit-lock-mode nil)
              (setq-local font-lock-support-mode nil)
              (setq buffer-read-only t
                    bidi-display-reordering nil)
              (buffer-disable-undo)))


;; Terminal
(standard-display-8bit 128 255)

;; switch off gui
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; turn off startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Make emacs use the clipboard
(setq x-select-enable-clipboard t
      interprogram-paste-function #'x-cut-buffer-or-selection-value)

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; dont wrap lines
(setq-default truncate-lines t)

;; default major mode
(set-default major-mode 'text-mode)

;; Make searches case insensitive
(setq case-fold-search t
      ;; highlight incremental search
      search-highlight t)

;; Cursor
(setq-default cursor-type 'hbar
              cursor-in-non-selected-windows 'hollow)
(set-face-background 'cursor "#000")
(blink-cursor-mode 1)

;; Show marked text
(transient-mark-mode 1)
(delete-selection-mode t)

;; syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; enable cua keybindings and functions
;; (when (> emacs-major-version 23)
;;   (cua-mode 1))
(global-set-key (kbd "C-z") #'undo)

;; Highlight parentheses
(show-paren-mode -1)
(setq show-paren-style 'expression
      blink-matching-paren nil
      blink-matching-paren-distance nil)
(defvar show-paren-deactivated-until-active-mark nil)
(add-hook 'activate-mark-hook   #'(lambda () (when show-paren-mode
                                               (setq show-paren-deactivated-until-active-mark t)
                                               (show-paren-mode -1))))
(add-hook 'deactivate-mark-hook #'(lambda () (when show-paren-deactivated-until-active-mark
                                               (setq show-paren-deactivated-until-active-mark nil)
                                               (show-paren-mode 1))))

(defvar-local show-paren-advice-enabled t)
(add-hook 'my/-find-large-file-hook
          #'(lambda () (setq-local show-paren-advice-enabled nil)))
(defadvice show-paren-function (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
echo area. Has no effect if the character before point is not of
the syntax class ')'."
  (interactive)
  (when (and show-paren-advice-enabled
             (let ((ctx (syntax-ppss-context (syntax-ppss))))
               (null ctx)))
    (unless (minibufferp)
      (let ((cb (char-before (point)))
            (ca (char-after (point)))
            t-beg t-end line column mesg)
        (when (and cb (char-equal (char-syntax cb) ?\)))
          (save-excursion (backward-list)
                          (unless (pos-visible-in-window-p)
                            (setq line (line-number-at-pos)
                                  column (current-column)
                                  t-beg (progn (beginning-of-line) (point))
                                  t-end (progn (end-of-line) (point))
                                  mesg (format "[%s:%s] %s" line column (buffer-substring t-beg t-end))))))
        (when (and ca (char-equal (char-syntax ca) ?\())
          (save-excursion (forward-list)
                          (unless (pos-visible-in-window-p)
                            (setq line (line-number-at-pos)
                                  column (current-column)
                                  t-beg (progn (beginning-of-line) (point))
                                  t-end (progn (end-of-line) (point))
                                  mesg (concat (and mesg (concat mesg "\n"))
                                               (format "[%s:%s] %s" line column (buffer-substring t-beg t-end)))))))
        (when mesg (let ((message-log-max nil))
                     (message "%s" mesg)))))))

(ad-enable-advice #'show-paren-function 'after 'show-matching-paren-offscreen)
(ad-activate #'show-paren-function)


;; Scrolling settings
;; seems like it overriden by smooth-scrolling package
(setq
 scroll-margin 5
 scroll-conservatively 100000
 scroll-step 1
 scroll-up-aggressively nil
 scroll-down-aggressively nil
 scroll-preserve-screen-position 1)

;; Mouse
(setq mouse-wheel-follow-mouse 't
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
(xterm-mouse-mode t)
;;(mouse-avoidance-mode 'jump)

;; Use fringes
(setq indicate-empty-lines t
      indicate-buffer-boundaries 'left)
(setq fringe-mode '(0.1 . 0))

;;Minibuffer
(setq
 enable-recursive-minibuffers t
 max-mini-window-height .25
 minibuffer-scroll-window nil
 resize-mini-windows t)

;; Shadow some files (like starting with dot, etc.)
(file-name-shadow-mode t)

;; show column & line numbers in status bar
(column-number-mode 1)
(line-number-mode 1)
;; show buffer size
(size-indication-mode 1)

;; Tab settings
(setq-default tab-width 4
              backward-delete-char-untabify-method 'hungry)
; use spaces (not tabs) for indenting
(setq-default indent-tabs-mode nil)

;; make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

; no bells please
(setq ring-bell-function (lambda nil nil))

;; disable startup echo area message
(fset 'display-startup-echo-area-message 'ignore)

;; more comfortable window navigation
(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)

(setq x-alt-keysym 'meta)

;; isearch
(defun isearch-delete-something ()
  "Delete non-matching text or the last character."
  ;; https://gist.github.com/johnmastro/508fb22a2b4e1ce754e0
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(define-key isearch-mode-map (kbd "<backspace>")
  #'isearch-delete-something)


;; indent automatically
;;(global-set-key (kbd "RET") #'newline-and-indent)
(defvar my/-double-key-timeout 0.25)
(defvar my/-double-key-timer nil)
(defun my/-ret ()
  (interactive)
  (let ((last-called (get this-command 'my/-last-call-time)))
    (if (and (eq last-command this-command)
             last-called
             (<= (time-to-seconds (time-since last-called)) my/-double-key-timeout))
        (progn
          (cancel-timer my/-double-key-timer)
          (put this-command 'my/-last-call-time nil)
          (reindent-then-newline-and-indent))
      (put this-command 'my/-last-call-time (current-time))
      (setq my/-double-key-timer
            (run-at-time my/-double-key-timeout nil #'newline-and-indent)))))
(global-set-key (kbd "RET") #'my/-ret)

(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)



;; backups
(let ((bu-dir (locate-user-emacs-file "cache/backups")))
  (unless (file-directory-p bu-dir) (make-directory bu-dir t))
  (setq make-backup-files t ;; do make backups
        backup-by-copying t ;; and copy them here
        backup-directory-alist `(("." . ,bu-dir))
        version-control   t
        kept-new-versions 2
        kept-old-versions 5
        delete-old-versions t))

(setq auto-save-list-file-prefix
      (concat user-emacs-directory "cache/auto-save-list/.saves-"))

;; tramp autosave
(with-eval-after-load "tramp"
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

;; abbrevs (abbreviations)
(let ((abbrev-dir (locate-user-emacs-file "data")))
  (unless (file-exists-p abbrev-dir) (make-directory abbrev-dir t))

  (setq abbrev-file-name  ;; tell emacs where to read abbrev
        (expand-file-name "abbrev_defs" abbrev-dir))  ;; definitions from...

  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))  ;;  don't tell

  (setq abbrev-mode t            ;; turn it on
        save-abbrevs 'silently)  ;; don't ask

  (add-hook 'kill-emacs-hook     ;; write when ...
            #'write-abbrev-file)  ;; ...when exiting emacs
  )
(eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))

;; filecache: http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
(eval-after-load "filecache"
  '(progn (message "Loading file cache...")
          (file-cache-add-directory "~/")
          (file-cache-add-directory-list '("~/Desktop" "~/Documents"))))

;; overrride the default function....
(defun emacs-session-filename (SESSION-ID)
  (locate-user-emacs-file (concat "cache/session." SESSION-ID)))

;; bookmarks
(setq bookmark-default-file (locate-user-emacs-file "data/bookmarks")
      bookmark-save-flag 1) ;; bookmarks

;; saveplace: save location in file when saving files
(setq save-place-file (locate-user-emacs-file "cache/saveplace"))
(setq-default save-place t)            ;; activate it for all buffers
(require 'saveplace)                   ;; get the package

;; savehist: save some history
(setq savehist-additional-variables     ;; also save...
      '(search ring regexp-search-ring) ;; ... my search entries
      savehist-autosave-interval 60     ;; save every minute (default: 5 min)
      savehist-file (locate-user-emacs-file "cache/savehist")) ;; keep my home clean
(savehist-mode t)

;; recentf
(require 'recentf)    ;; save recently used files
(setq
 recentf-save-file (locate-user-emacs-file "cache/recentf")
 recentf-max-saved-items 100     ;; max save 100
 recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)                 ;; turn it on


(winner-mode)

;;(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; 0-rc.el ends here
