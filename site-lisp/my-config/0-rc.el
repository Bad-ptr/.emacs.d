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

(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

(setq locale-coding-system      'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq coding-system-for-read    'utf-8)
(setq coding-system-for-write   'utf-8)

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
(tool-bar-mode   0)
(menu-bar-mode   0)
(scroll-bar-mode 0)

;; turn off startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Make emacs use the clipboard
(setq x-select-enable-clipboard t
      interprogram-paste-function #'x-selection-value)

;; Create frames maximized
(push '(fullscreen . maximized) initial-frame-alist)

;; Don't mind of frame size
(setq frame-inhibit-implied-resize t)

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; dont wrap lines
(setq-default truncate-lines t)

;; default major mode
(setq-default major-mode 'text-mode)

;; Make searches case insensitive
(setq case-fold-search t
      ;; highlight incremental search
      search-highlight t)

;; Cursor
(setq-default cursor-type 'box
              cursor-in-non-selected-windows 'hollow)
(set-face-background 'cursor "#080")
(blink-cursor-mode 1)

;; Show marked text
(transient-mark-mode   1)
(delete-selection-mode t)

;; syntax highlighting
(setq font-lock-maximum-decoration t
      ;;font-lock-maximum-size nil
      )
;;(global-font-lock-mode 1)

;; enable cua keybindings and functions
;; (when (> emacs-major-version 23)
;;   (cua-mode 1))
(global-set-key (kbd "C-z") #'undo)

;; Highlight parentheses
(show-paren-mode -1)
(setq show-paren-style 'expression
      show-paren-when-point-in-periphery t
      show-paren-when-point-inside-paren t
      blink-matching-paren nil
      blink-matching-paren-distance nil)
(defface show-paren-match-face
  '((default :inherit nil)
    (((class color) (min-colors 88) (background light))
     (:background "#eafff5"))
    (((class color) (min-colors 88) (background dark))
     (:background "#002005")))
  "Face for show-paren mode, when parens match.")
(defvar show-paren-deactivated-until-active-mark nil)
(add-hook 'activate-mark-hook
          #'(lambda ()
              (when show-paren-mode
                (setq show-paren-deactivated-until-active-mark t)
                (show-paren-mode -1))))
(add-hook 'deactivate-mark-hook
          #'(lambda ()
              (when show-paren-deactivated-until-active-mark
                (setq show-paren-deactivated-until-active-mark nil)
                (show-paren-mode 1))))

(defvar-local my/-show-paren-advice-enabled t)
(add-hook 'my/-find-large-file-hook
          #'(lambda () (setq-local my/-show-paren-advice-enabled nil)))
(defvar my/-show-paren-advice-timeout 0.5)
(defvar my/-show-paren-advice-timer   nil)
(defvar my/-show-paren-advice-last-msg "")
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
echo area. Has no effect if the character before point is not of
the syntax class ')'."
  (interactive)
  (unless my/-show-paren-advice-timer
    (setq my/-show-paren-advice-timer
          (run-at-time
           my/-show-paren-advice-timeout nil
           #'(lambda ()
               (with-demoted-errors "show-paren error: %S"
                 (unwind-protect
                     (when (and (not (minibufferp))
                                my/-show-paren-advice-enabled
                                (let ((ctx (syntax-ppss-context (syntax-ppss))))
                                  (null ctx)))
                       (let ((cb (char-before (point)))
                             (ca (char-after (point)))
                             t-beg t-end line column mesg)
                         (my/-save-mark-and-excursion
                           (when (cond
                                  ((and cb (char-equal (char-syntax cb) ?\)))
                                   (backward-list) t)
                                  ((and ca (char-equal (char-syntax ca) ?\())
                                   (forward-list) t)
                                  (t nil))
                             (unless (pos-visible-in-window-p)
                               (setq
                                line (line-number-at-pos)
                                column (current-column)
                                t-beg (progn (beginning-of-line) (point))
                                t-end (progn (end-of-line) (point))
                                mesg (let ((cmsg (current-message)))
                                       (when (string=
                                              cmsg
                                              my/-show-paren-advice-last-msg)
                                         (setq cmsg nil))
                                       (setq my/-show-paren-advice-last-msg
                                             (concat
                                              (format "[%s:%s] %s" line column
                                                      (buffer-substring
                                                       t-beg t-end))
                                              (and cmsg "\n") cmsg)))))))
                         (when mesg
                           (let ((message-log-max nil))
                             (message "%s" mesg)))))
                   (setq my/-show-paren-advice-timer nil))))))))

(ad-enable-advice #'show-paren-function 'after 'show-matching-paren-offscreen)
(ad-activate #'show-paren-function)


;; Scrolling settings
;; seems like it overriden by smooth-scrolling package
(setq
 scroll-margin 5
 scroll-step   1
 scroll-conservatively 100000
 scroll-up-aggressively   nil
 scroll-down-aggressively nil
 scroll-preserve-screen-position 1
 fast-but-imprecise-scrolling    t)
;; (pixel-scroll-mode)
;; (pixel-scroll-precision-mode)

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
(line-number-mode   1)
;; show buffer size
(size-indication-mode 1)

;; Fill column
(setq-default fill-column 80)

;; Tab settings
(setq-default tab-width 4
              backward-delete-char-untabify-method 'hungry)
;; use spaces (not tabs) for indenting
;; (setq-default indent-tabs-mode nil)
;; use tabs for indenting
(setq-default indent-tabs-mode t)

;; make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; no bells please
(setq ring-bell-function (lambda nil nil))

;; disable startup echo area message
(fset 'display-startup-echo-area-message 'ignore)

;; more comfortable window navigation
(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)

(setq x-alt-keysym 'meta)


;; indent automatically
;;(global-set-key (kbd "RET") #'newline-and-indent)
(defvar my/-double-key-timeout 0.25)
(defvar my/-double-key-timer nil)
(defun my/-ret ()
  (interactive)
  (let ((last-called (get this-command 'my/-last-call-time)))
    (if (and (eq last-command this-command)
             last-called
             (<= (time-to-seconds (time-since last-called))
                 my/-double-key-timeout))
        (progn
          (cancel-timer my/-double-key-timer)
          (put this-command 'my/-last-call-time nil)
          (reindent-then-newline-and-indent))
      (put this-command 'my/-last-call-time (current-time))
      (setq my/-double-key-timer
            (run-at-time my/-double-key-timeout nil #'newline-and-indent)))))
(global-set-key (kbd "RET") #'my/-ret)

;; open-line and indent
(defun my/-open-line (&optional N)
  (interactive "*p")
  (open-line N)
  (my/-save-mark-and-excursion
    (forward-line)
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") #'my/-open-line)

;; delete region ignoring read-only text properties
(defun my/-delete-region-even-if-readonly (&optional start end)
  (interactive "r")
  (when (and transient-mark-mode mark-active)
    (unless start (setq start (point)))
    (unless end (setq end (mark)))
    (when (> start end) (rotatef start end)))
  (when (and start end)
    (let ((inhibit-read-only t))
      (delete-region start end))))
(global-set-key (kbd "s-<delete>") #'my/-delete-region-even-if-readonly)

(defun my/-clear-buffer-to-current-line ()
  (interactive)
  (save-mark-and-excursion
    (let ((end (progn (previous-line)
                      (point-at-eol)))
          (start (point-min)))
      (my/-delete-region-even-if-readonly start end))))

(define-key minibuffer-local-map (kbd "C-<up>") 'previous-line)
(define-key minibuffer-local-map (kbd "C-<down>") 'next-line)
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)

(defun my/-delete-overlays-in-region (&optional start end)
  (interactive "r")
  (when (and transient-mark-mode mark-active)
    (unless start (setq start (point)))
    (unless end (setq end (mark)))
    (when (> start end) (rotatef start end)))
  (when (and start end)
    (mapc #'delete-overlay (overlays-in start end))))

;; backups
(let ((bu-dir (locate-user-emacs-file "cache/backups")))
  (unless (file-directory-p bu-dir)
    (make-directory bu-dir t))
  (setq make-backup-files t ;; do make backups
        backup-by-copying t ;; and copy them here
        backup-directory-alist `(("." . ,bu-dir))
        version-control   t
        kept-new-versions 2
        kept-old-versions 5
        delete-old-versions t))

(setq auto-save-list-file-prefix
      (expand-file-name "cache/auto-save-list/.saves-" user-emacs-directory))

;; tramp autosave
(with-eval-after-load "tramp"
  (setq tramp-auto-save-directory
        (locate-user-emacs-file "tramp-auto-save")))

;; abbrevs (abbreviations)
(let ((abbrev-dir (locate-user-emacs-file "data")))
  (unless (file-exists-p abbrev-dir)
    (make-directory abbrev-dir t))

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
(setq save-place-file
      (locate-user-emacs-file "cache/saveplace"))
;;(setq-default save-place t)            ;; activate it for all buffers
(require 'saveplace)                   ;; get the package
(save-place-mode t)

;; savehist: save some history
(setq savehist-additional-variables     ;; also save...
      '(search ring regexp-search-ring) ;; ... my search entries
      savehist-autosave-interval 60     ;; save every minute (default: 5 min)
      savehist-file                     ;; keep my home clean
      (locate-user-emacs-file "cache/savehist"))
(savehist-mode t)

;; recentf
(require 'recentf)    ;; save recently used files
(setq
 recentf-save-file
 (locate-user-emacs-file "cache/recentf")
 recentf-max-saved-items 1000    ;; max save 1000
 recentf-max-menu-items  15)     ;; max 15 in menu
(recentf-mode t)                 ;; turn it on


(winner-mode)

;;(disable-command 'overwrite-mode)
(put 'overwrite-mode 'disabled t)

;;(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(when (fboundp 'eww)
  (setq browse-url-browser-function 'eww-browse-url))

;; 0-rc.el ends here
