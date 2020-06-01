;;; rsync.el --- summary -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
;; Version: 0.1 alpha
;; Package-Requires:
;; Keywords:
;;
;;; MIT License
;;
;; Copyright (c) 2020 esac
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;;  A little wanna be rsync library interface. (working in progress!).
;;
;;; Code:

(require 'auth-source)

(defgroup rsync nil
  "Rsync command interface."
  :group 'extensions
  :group 'convenience)

(defcustom rsync-program "rsync"
  "The rsync executable file (program).
Should be located by `file-executable-p'."
  :type 'string
  :group 'rsync)

(defcustom rsync-opts "-a -v -h --progress -r"
  "The default options for rsync command."
  :type 'string
  :group 'rsync)

(defcustom rsync-buffer-name "*rsync*"
  "The rysnc process buffer name."
  :type 'string
  :group 'rsync)

(defcustom rsync-create-buffer-p t
  "If non-nil create rsync process buffer."
  :type 'bool
  :group 'rsync)

(defcustom rsync-kill-buffer-p nil
  "If non-nil kill buffer after exit."
  :type 'bool
  :group 'rsync)

(defcustom rsync-debug-p t
  "If non-nil print debug messages on *Message* buffer."
  :type 'bool
  :group 'rsync)

;; TODO: research hooks
;; (defvar rsync-before-command-hook nil
;;   "Hooks to be run after rsync command.")

;; (defvar rsync-after-command-hook nil
;;   "Hooks to be run after rsync command finishes.")

;; TODO: research filter
;; (defun rsync--filter ())

(defun rsync--default-sentinel (process event)
  "The rsync PROCESS default EVENT handler function.
This is also a template for another callbacks."
  (let ((status (process-status process))
         (buffer (process-buffer process)))
    ;; TODO condition-case to handle process status (signals, exit, etc...)
    ;; debug message
    (when rsync-debug-p
      (message "Process: %s had the event: %s" process event))
    (cond
      ;; handle exit status
      ((eq status 'exit)
        (when rsync-kill-buffer-p (kill-buffer buffer)))
      ;; TODO: research how and if its necessary
      ;; to verify this process status
      ((or
         (eq status 'stop)
         (eq status 'signal)
         (eq status 'closed)
         (eq status 'failed))
        nil))))

(defun rsync--set-sentinel (process callback)
  "Set rsync PROCESS sentinel and call CALLBACK function to handle events."
  (set-process-sentinel process callback))

(defun rsync--start-process (program-args callback)
  "Start rsync process with PROGRAM-ARGS.
Set a CALLBACK function to handle rsync process signals and returns."
  (let ((buffer (if rsync-create-buffer-p
                  (get-buffer-create rsync-buffer-name))))
    (if (executable-find rsync-program)
      (rsync--set-sentinel
        (apply 'start-process rsync-program buffer rsync-program program-args)
        callback)
      (message "Command %s not found" rsync-program))))

;; TODO: auth-source integration
(defun rsync-auth-source-search (host user)
  "Lookup (format HOST USER PORT) password on auth-source default file."
  (let* ((cred (auth-source-search :host host :user user))
          (secretf (if cred (plist-get (car cred) :secret))))
    (when secretf (funcall secretf))))

(defun rsync-transfer-files (program-args &optional sentinel)
  "The rsync transfer files operation."
    (rsync--start-process program-args
      (or sentinel 'rsync--default-sentinel)))

;; TODO: make a function to reuse arguments formation
;; (defun rsync-parse-args ())

;; Push: rsync [OPTION...] SRC... [USER@]HOST::DEST
(defun rsync-push (host src dest &optional user sentinel)
  "The rsync push operation."
  ;; transfer files from local host to remote host
  (let* ((host (if user (concat user "@" host) host))
          (dest (concat host ":" dest))
          (opts (split-string rsync-opts))
          (args (append opts (list src dest))))
    (rsync-transfer-files args sentinel)))

(defun rsync-pull (host src dest &optional user sentinel)
  "The rsync pull operation."
  (let* ((host (if user (concat user "@" host) host))
         (src (concat host ":" src))
         (opts (split-string rsync-opts))
         (args (append opts (list src dest))))
    (rsync-transfer-files args sentinel)))

(provide 'rsync)
;;; rsync.el ends here
