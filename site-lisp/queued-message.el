;;; queued-message.el --- messages with queue. -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Constantin Kulikov
;;
;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 0.1
;; Package-Requires: ()
;; Date: 2014/11/21 10:35:56
;; License: GPL either version 2 or any later version
;; Keywords: message, queue
;; X-URL: 

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Replacement for emacs `message' function with queue and urgency.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'queued-message)

;;; Code:

(require 'cl-lib)

(unless (fboundp 'cl-flet)
  (require 'cl)
  (defalias 'cl-flet 'flet))


(cl-defstruct (q-m/Msg (:conc-name q-m/Msg-)
                       (:constructor q-m/make-Msg))
  (msg-s "")
  (urgent nil)
  (on-show nil)
  (life-time 5)
  (kill-timer nil)
  (replace-tag nil)
  (list-in nil)
  (dead nil))


(defvar q-m/-display-list nil
  "List of the messages that displayed now.")

(defvar *q-m/-display-list-max* (cl-typecase max-mini-window-height
                                  (integer max-mini-window-height)
                                  (t 3))
  "Maximum number of messages to display at once.")

(defvar q-m/-do-not-delete-on-timer t
  "If t -- do not really delete a message from the display-list,
only set it's dead flag.")

(defvar q-m/-urgent-queue nil
  "Queue of the urgent messages.")

(defvar q-m/-nonurgent-queue nil
  "Queue of the nonurgent messages.")

(defvar q-m/-replace-tag-hash (make-hash-table :test #'equal :size 10)
  "Hash table that points to messages with specified replace-tag.")

(defvar q-m/-classify-msg-rxs
  '(("^\\(Failing\\( \\(over\\)?wrapped\\)? \\|Overwrapped \\)?I-search.*" . "isearch")
    ("^Indent.*"                  . "indent")
    ("^History item.*"            . "history")
    ("^\\(Wr[oi]te\\|Read\\|\\((No .+ need .+\\)?[Ss]av[ie]\\|Load\\).*"
     . "write/read")
    ("^\\(Importing package-keyring\\|\\(Install\\|Delete\\) th\\|Generating autoloads\\|Package \\).*"
     . "packages/elisp")
    ("^Compil.*"                  . "compile")
    ("^Tramp:.*"                  . "tramp")
    ("^Password.*"                . "password")
    ("^Parsing .+ file.*"         . "parsing/decompress"))
  "If message match car then set replace-tag as cdr.")

(defvar q-m/-preprocess-msg-functions
  (list
   #'(lambda (msg)
       (let ((msg-s (q-m/Msg-msg-s msg)))
         (block 'rx-loop
           (dolist (item q-m/-classify-msg-rxs)
             (when (string-match-p (car item) msg-s)
               (setf (q-m/Msg-replace-tag msg) (cdr item))
               (return-from 'rx-loop)))))))
  "List of functions that get message structure as argument and must return
replace-tag or nil.")

(defvar q-m/-prep-msg-for-display-func
  #'(lambda (msg i)
      (concat (propertize (number-to-string i)
                          'face '(:background "gray"))
              "|" (q-m/Msg-msg-s msg)))
  "This function is called with the message string and position
in the display-list as arguments. Must return string.")

(defvar *q-m/-default-life-time* 5
  "Default life-time of a message.")

(defvar *q-m/-default-urgent* nil
  "Default urgency of a message.")

(defvar *q-m/-default-on-show* nil
  "Default function that called when a message
placed in the `q-m/-display-list'.")

(defvar *q-m/-messages-separator* " "
  "Separator between messages.")

(defvar *q-m/-string-cache* nil
  "Messages output cache.")

(defconst q-m/-emacs-message-func (symbol-function 'message)
  "Emacs' original message function.")

(defvar *q-m/-output-function*
  #'(lambda (str)
      (funcall q-m/echo "%s" str))
  "Function that do output to user.")
(defvar *q-m/-log-function*
  #'(lambda (str)
      (funcall q-m/log "%s" str))
  "Function that logs messages somewhere or nil.")


(defun q-m/Msg-cancel-kill-timer (msg)
  (when (q-m/Msg-kill-timer msg)
    (cancel-timer (q-m/Msg-kill-timer msg)))
  (setf (q-m/Msg-kill-timer msg) nil))
(defun q-m/Msg-reset-kill-timer (msg)
  (q-m/Msg-cancel-kill-timer msg)
  (setf (q-m/Msg-kill-timer msg)
        (run-at-time (q-m/Msg-life-time msg) nil
                     #'q-m/-msg-kill-timer-func msg)))

(defun q-m/Msg-run-on-show (msg)
  (when (q-m/Msg-on-show msg)
    (funcall (q-m/Msg-on-show msg) msg)))


(defun q-m/minibuffer-clear ()
  (with-current-buffer (window-buffer (minibuffer-window))
    (erase-buffer)))
(defun q-m/minibuffer-echo (fmt &rest objs)
  (q-m/minibuffer-clear)
  (let ((msg-s (apply #'format fmt objs)))
    (with-current-buffer (window-buffer (minibuffer-window))
      (goto-char (point-max))
      (insert msg-s))))

(defun q-m/echo (fmt &rest objs)
  (let ((message-log-max nil))
    (apply q-m/-emacs-message-func fmt objs)))

(defun q-m/log (fmt &rest objs)
  (run-at-time nil nil
               #'(lambda ()
                   (with-current-buffer (messages-buffer)
                     (let ((str (apply #'format fmt objs))
                           (inhibit-read-only t))
                       (goto-char (point-max))
                       (unless (bolp) (insert "\n"))
                       (insert str "\n"))))))

(cl-defmacro with-q-m/-options ((&key urgent life-time (on-show nil on-show-p)) &rest body)
  (let ((*q-m/-default-life-time* (or life-time *q-m/-default-life-time*))
        (*q-m/-default-urgent* (or urgent *q-m/-default-urgent**))
        (*q-m/-default-on-show* *q-m/-default-on-show*))
    (when on-show-p (setq *q-m/-default-on-show* on-show))
    ,@body))

(defun q-m/message (fmt &rest objs)
  (when fmt
    (let ((msg-s (apply #'format fmt objs)))
      (when message-log-max
        (q-m/log "%s" msg-s))
      (q-m/-message :msg-s msg-s)
      (or msg-s ""))))

(cl-defun q-m/-message (&key (msg-s "") (urgent *q-m/-default-urgent*)
                             (life-time *q-m/-default-life-time*)
                             (on-show *q-m/-default-on-show*)
                             (replace-tag nil))
  (let ((new-msg (q-m/make-Msg
                  :msg-s msg-s :urgent urgent :life-time life-time
                  :on-show on-show :replace-tag replace-tag)))
    (dolist (fun q-m/-preprocess-msg-functions)
      (funcall fun new-msg))
    (cl-flet
        ((enq-msg
          (&optional puthash)
          (when (and (q-m/Msg-msg-s new-msg) (> (string-width (q-m/Msg-msg-s new-msg)) 0))
            (when puthash (puthash (q-m/Msg-replace-tag new-msg) new-msg q-m/-replace-tag-hash))
            (q-m/-enqueue-message new-msg))))
      (if (q-m/Msg-replace-tag new-msg)
          (let ((old-msg (gethash (q-m/Msg-replace-tag new-msg) q-m/-replace-tag-hash nil)))
            (if old-msg
                (let ((lst (q-m/Msg-list-in old-msg)))
                  (q-m/Msg-cancel-kill-timer old-msg)
                  (setf (q-m/Msg-msg-s old-msg)     (q-m/Msg-msg-s new-msg)
                        (q-m/Msg-urgent old-msg)    (q-m/Msg-urgent new-msg)
                        (q-m/Msg-life-time old-msg) (q-m/Msg-life-time new-msg)
                        (q-m/Msg-on-show old-msg)   (q-m/Msg-on-show new-msg))
                  (q-m/-lift-up-msg-in-list lst old-msg)
                  (if (eq lst 'q-m/-display-list)
                      (progn
                        (q-m/Msg-run-on-show old-msg)
                        (q-m/Msg-reset-kill-timer old-msg)
                        (q-m/-invalidate-cache)
                        (q-m/-redisplay))
                    (q-m/-update-display-list)))
              (enq-msg t)))
        (enq-msg)))))

(defun q-m/-enqueue-message (msg)
  (when msg
    (macrolet ((lift-msg (lst-name)
                         (let ((old-msg (gensym)))
                           `(let (,old-msg)
                              (setf (symbol-value ,lst-name)
                                    (cl-delete msg (symbol-value ,lst-name)
                                               :count 1
                                               :test #'(lambda (c-m o-m)
                                                         (and (string= (q-m/Msg-msg-s c-m)
                                                                       (q-m/Msg-msg-s o-m))
                                                              (setq ,old-msg o-m)))))
                              (when ,old-msg
                                (setf (symbol-value ,lst-name) (cons ,old-msg (symbol-value ,lst-name)))
                                (q-m/Msg-cancel-kill-timer ,old-msg)
                                (setf (q-m/Msg-msg-s ,old-msg)     (q-m/Msg-msg-s msg)
                                      (q-m/Msg-urgent ,old-msg)    (q-m/Msg-urgent msg)
                                      (q-m/Msg-life-time ,old-msg) (q-m/Msg-life-time msg)
                                      (q-m/Msg-on-show ,old-msg)   (q-m/Msg-on-show msg))
                                (when (eq ,lst-name 'q-m/-display-list)
                                  (q-m/Msg-run-on-show ,old-msg)
                                  (q-m/Msg-reset-kill-timer ,old-msg)
                                  (q-m/-invalidate-cache)
                                  (q-m/-redisplay))
                                t)))))
      (let ((msg-s (q-m/Msg-msg-s msg))
            (re-tag (q-m/Msg-replace-tag msg)))
        (cl-case (q-m/Msg-urgent msg)
          (nil
           (unless (and (not re-tag) (or (lift-msg 'q-m/-nonurgent-queue)
                                         (lift-msg 'q-m/-display-list)))
             (setq q-m/-nonurgent-queue
                   (append q-m/-nonurgent-queue `(,msg)))
             (setf (q-m/Msg-list-in msg) 'q-m/-nonurgent-queue)))
          (t
           (unless (and (not re-tag) (or (lift-msg 'q-m/-urgent-queue)
                                         (lift-msg 'q-m/-display-list)))
             (setq q-m/-urgent-queue
                   (append q-m/-urgent-queue `(,msg)))
             (setf (q-m/Msg-list-in msg) 'q-m/-urgent-queue))))))
    (q-m/-update-display-list)))

(defmacro q-m/-lift-up-msg-in-list (list-name msg)
  `(setf (symbol-value ,list-name) (cl-delete ,msg (symbol-value ,list-name) :count 1)
         (symbol-value ,list-name) (cons ,msg (symbol-value ,list-name))))

(defun q-m/-update-display-list ()
  (if (and (< (length q-m/-display-list) *q-m/-display-list-max*))
      (while (and (< (length q-m/-display-list) *q-m/-display-list-max*)
                  (or q-m/-urgent-queue q-m/-nonurgent-queue))
        (if q-m/-urgent-queue
            (q-m/-add-to-display (pop q-m/-urgent-queue))
          (when q-m/-nonurgent-queue
            (q-m/-add-to-display (pop q-m/-nonurgent-queue)))))
    (let ((lastm (last q-m/-display-list)))
      (if (and lastm (or q-m/-urgent-queue q-m/-nonurgent-queue)
               q-m/-do-not-delete-on-timer (q-m/Msg-dead (car lastm)))
          (progn
            (nbutlast q-m/-display-list)
            (when (q-m/Msg-replace-tag (car lastm))
              (remhash (q-m/Msg-replace-tag (car lastm)) q-m/-replace-tag-hash))
            (q-m/-update-display-list))
        (when q-m/-urgent-queue
          (let ((cnt (length q-m/-urgent-queue))
                removed)
            (setq q-m/-display-list
                  (cl-delete nil q-m/-display-list
                             :test #'(lambda (_ msg)
                                       (unless (q-m/Msg-urgent msg)
                                         (q-m/-suspend msg)
                                         (setf (q-m/Msg-list-in msg) 'q-m/-nonurgent-queue)
                                         (push msg removed)
                                         t))
                             :count cnt))
            (mapc #'(lambda (msg) (push msg q-m/-nonurgent-queue)) (nreverse removed))
            (when removed (q-m/-update-display-list))))))))

(defun q-m/-add-to-display (msg)
  (when msg
    (setf (q-m/Msg-list-in msg) 'q-m/-display-list)
    (push msg q-m/-display-list)
    (q-m/Msg-run-on-show msg)
    (q-m/Msg-reset-kill-timer msg)
    (q-m/-invalidate-cache)
    (q-m/-redisplay)))

(defun q-m/-msg-kill-timer-func (msg)
  (when msg
    (setf (q-m/Msg-dead msg) t)
    (unless q-m/-do-not-delete-on-timer
      (setq q-m/-display-list (delq msg q-m/-display-list))
      (when (q-m/Msg-replace-tag msg)
        (remhash (q-m/Msg-replace-tag msg) q-m/-replace-tag-hash))
      (q-m/-update-display-list))))

(defun q-m/-suspend (&optional msg)
  (if msg
      (q-m/Msg-cancel-kill-timer msg)
    (dolist (msg1 q-m/-display-list)
      (when msg1
        (q-m/-suspend msg1)))))

(defun q-m/-resume (&optional msg)
  (if msg
      (q-m/Msg-reset-timer msg)
    (dolist (msg1 q-m/-display-list)
      (when msg1
        (q-m/-resume msg1)))
    (q-m/-redisplay)))

(defun q-m/-invalidate-cache ()
  (setq *q-m/-string-cache* nil) t)

(defun q-m/-redisplay ()
  (let* ((i 0)
         (output (or *q-m/-string-cache*
                     (setq *q-m/-string-cache*
                           (mapconcat #'(lambda (msg)
                                          (funcall q-m/-prep-msg-for-display-func msg (incf i)))
                                      q-m/-display-list *q-m/-messages-separator*)))))
    (when (> (string-width output) 0)
      (q-m/echo "%s" output))))


(defun q-m/replace-message ()
  (interactive)
  (fset 'message (symbol-function 'q-m/message)))
(defun q-m/restore-message ()
  (interactive)
  (fset 'message (symbol-value 'q-m/-emacs-message-func)))


(provide 'queued-message)

;;; queued-message.el ends here
