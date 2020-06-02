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
  "The rsync program name.
Should be located by `executable-find'."
  :type 'string
  :group 'rsync)

(defcustom rsync-options "-a -v -h --progress -r"
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

(defvar rsync-executable (executable-find rsync-program)
  "The rsync executable file (full path).")

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
    ;; debug message
    (when rsync-debug-p
      (message "Process: %s had the event: %s" process event))
    (cond
      ;; handle exit status
      ((eq status 'exit)
        (when rsync-kill-buffer-p (kill-buffer buffer)))
      ;; TODO condition-case to handle process status (signals, exit, etc...)
      ;; TODO: research how and if its necessary
      ;; to verify this process status
      ((or
         (eq status 'stop)
         (eq status 'signal)
         (eq status 'closed)
         (eq status 'failed))
        nil))))

(defun rsync--set-sentinel (process sentinel)
  "Set rsync PROCESS SENTINEL (callback) function to handle events."
  (set-process-sentinel process sentinel))

(defun rsync--start-process (program-args sentinel)
  "Start rsync process defined by PROGRAM with PROGRAM-ARGS.

Set a SENTINEL (callback) function to handle rsync
process signals and returns."
  ;; create a buffer, if create buffer predicate is true
  (let ((buffer (if rsync-create-buffer-p
                  (get-buffer-create rsync-buffer-name))))
      (rsync--set-sentinel
        (apply 'start-process
          rsync-program buffer rsync-executable program-args)
        sentinel)))

(defun rsync--parse-args (opcode host src dest &optional user)
  "Parse HOST SRC DEST USER arguments based on the rysnc OPCODE.

Pull: rsync [OPTION...] [USER@]HOST:SRC... [DEST]
Push: rsync [OPTION...] SRC... [USER@]HOST:DEST

This function return rsync string arguments list."

  ;; parse rsync mandatory arguments and options
  (let ((host (if user (concat user "@" host) host))
         (opts (split-string rsync-options)))
    (if (eq opcode 'pull)
      (setq src (concat host ":" src))
      (setq dest (concat host ":" dest)))
    (append opts (list src dest))))

;; TODO: auth-source integration
(defun rsync-auth-source-search (host user)
  "Lookup (format HOST USER PORT) password on auth-source default file."
  (let* ((cred (auth-source-search :host host :user user))
          (secretf (if cred (plist-get (car cred) :secret))))
    (when secretf (funcall secretf))))

(defun rsync-transfer-files (program-args &optional sentinel)
  "The rsync transfer files operation.

PROGRAM-ARGS rsync parsed arguments.
SENTINEL (callback) function to handle process signals/status."
  ;; start transfer procedure (invoke rsync spirit)
  (rsync--start-process program-args
    (or sentinel 'rsync--default-sentinel)))

(defun rsync-push (host src dest &optional user callback)
  "The rsync push operation.

HOST Remote host identifier.
The host name can be a alises defined in ~/.ssh/config or /etc/hosts,
auth-sources interface will be provided soon.

SRC  Source directory.
DEST Destination directory.

&OPTIONAL:

USER     User identifier.
CALLBACK Function to handle process events (sentinel)."

  (if rsync-executable
    (rsync-transfer-files
      (rsync--parse-args 'push host src dest user) callback)
    (error "Command %s not found" rsync-program)))

(defun rsync-pull (host src dest &optional user callback)
  "The rsync pull operation.

HOST Remote host identifier.
The host name can be a alises defined in ~/.ssh/config or /etc/hosts,
auth-sources interface will be provided soon.

SRC  Source directory.
DEST Destination directory.

&OPTIONAL:

USER     User identifier.
CALLBACK Function to handle process events (sentinel)."
  ;; if rsync executable was found:
  ;; parse arguments and transfer files
  (if rsync-executable
    (rsync-transfer-files
      (rsync--parse-args 'pull host src dest user) callback)
    (error "Command %s not found" rsync-program)))

(provide 'rsync)
;;; rsync.el ends here
