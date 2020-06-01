;;; rsync.el --- summary -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Maintainer: esac
;; Version:
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

;; hooks?
;; (defvar rsync-before-command-hook nil
;;   "Hooks to be run after rsync command.")

;; (defvar rsync-after-command-hook nil
;;   "Hooks to be run after rsync command finishes.")

;; filter?
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
      ;; todo: research
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

;; Push: rsync [OPTION...] SRC... [USER@]HOST::DEST
;;       rsync [OPTION...] SRC... rsync://[USER@]HOST[:PORT]/DEST
(defun rsync-push (host src dst &optional user sentinel)
  "The rsync push action.

HOST Remote machine identifier.
The host can be a alises defined in ~/.ssh/config or /etc/hosts,
auth-sources interface will be provided soon.

SRC  Source directory.
DST  Destination directory.

&OPTIONAL:

USER Account username.
SENTINEL function to handle process events."

  ;; parse rsync options and arguments
  (let*
    ((host (if user (concat user "@" host) host))
      (dst  (concat host ":" dst))
      (opts (split-string rsync-opts))
      (args (append opts (list src dst)))
      (sentinel (or sentinel 'rsync--default-sentinel)))
    ;; call start process and set sentinel callback
    (rsync--start-process args sentinel)))

;; Pull: rsync [OPTION...] [USER@]HOST::SRC... [DEST]
;;       rsync [OPTION...] rsync://[USER@]HOST[:PORT]/SRC... [DEST]
(defun rsync-pull (host user src dst)
  ""
  ((host (if user (concat user "@" host) host))
      (dst  (concat host ":" dst))
      (opts (split-string rsync-opts))
      (args (append opts (list src dst)))
      (sentinel (or sentinel 'rsync--default-sentinel)))


  (rsync--start-process args 'rsync--default-sentinel)))


;;   )
;; (append (split-string rsync-opts) '("host"))
;; (rsync-push "solaris" "/home/esac/core/dev/rsync-el/" "/home/esac/")


(provide 'rsync)
;;; rsync.el ends here
