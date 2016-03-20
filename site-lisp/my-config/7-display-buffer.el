;;; -*- lexical-binding: t -*-

(when (fboundp 'balance-windows-area)
  (defvar original-balance-windows (symbol-function 'balance-windows))
  (defalias 'balance-windows #'balance-windows-area))

(setq even-window-sizes nil)

(defun my/-display-buffer-at-bottom (buffer alist)
  (unless (or (window-minibuffer-p)
              (window-dedicated-p))
    ;;(set-window-parameter nil 'golden-ratio-h-factor 3)
    (condition-case err
        (lexical-let ((window (split-window-below)))
          (when window
            (select-window window)
            (window--display-buffer buffer window 'window alist)
            (with-current-buffer buffer
              (setq window-area-factor 10)
              (add-hook 'kill-buffer-hook #'(lambda ()
                                              ;; (run-at-time 0.5 nil #'(lambda (w)
                                              ;;                          (when (window-live-p w)
                                              ;;                            (delete-window w)))
                                              ;;              window)
                                              (with-current-buffer (current-buffer)
                                                (when (and (window-live-p window)
                                                           (window-deletable-p window))
                                                  (delete-window window)))
                                              ) t t))
            window))
      (error nil))))

(defun my/-display-buffer-reuse-window-by-regexp (buffer alist)
  (let ((regexp (cdr (assoc 'reuse-window-buffer-regexp alist))))
    (if regexp
        (progn
          (let ((win (cl-find-if #'(lambda (w)
                                     (string-match-p regexp (buffer-name (window-buffer w))))
                                 (window-list))))
            win))
      (display-buffer-reuse-window buffer alist))))

(unless (version< emacs-version "24.1")
  (add-to-list 'display-buffer-alist
               '(".*"
                 (display-buffer-reuse-window
                  ;;display-buffer-in-side-window
                  my/-display-buffer-at-bottom
                  display-buffer-use-some-window
                  )
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.4)))

  ;; (with-eval-after-load "golden-ratio"
  ;;   ;; (push #'(lambda () (window--side-window-p nil)) golden-ratio-inhibit-functions)
  ;;   (push #'(lambda () (window-parameter nil 'golden-ratio-ignore)) golden-ratio-inhibit-functions)
  ;;   )
  )
