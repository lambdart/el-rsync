;;; rsync.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: Alpha 0.0.1
;; Homepage: https://github.com/lambdart/rsync.el
;; Keywords: rsync elisp interface
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
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
;; Rsync is a fast and extraordinarily versatile file copying tool.
;; It can copy locally, to/from another host over any remote shell, or
;; to/from a remote rsync daemon.
;;
;; It offers a large number of options that control every aspect of its
;; behavior and permit very flexible specification of the set of files
;; to be copied.  It is famous for its delta-transfer algorithm,
;; which reduces the amount of data sent over the network by sending
;; only the differences between the source files and the existing files
;; in the destination.  Rsync is widely used for backups and mirroring
;; and as an improved copy command for everyday use.
;;
;; This library is just a =Elisp= interface to explore =rsync=
;; capabilities direct from our beloved =Emacs=.
;;
;;; Code:

(require 'env)
(require 'ansi-color)
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

(defcustom rsync-switches "-a -v --progress -r"
  "Rsync default options."
  :type 'string
  :group 'rsync)

(defcustom rsync-buffer-name "*rsync*"
  "Rysnc process buffer name."
  :type 'string
  :group 'rsync)

(defcustom rsync-create-buffer-flag t
  "Non-nil means create rsync process buffer."
  :type 'bool
  :group 'rsync)

(defcustom rsync-kill-buffer-flag nil
  "Non-nil means kill buffer after exit."
  :type 'bool
  :group 'rsync)

(defcustom rsync-debug-flag t
  "Non-nil means print debug messages on *Message* buffer."
  :type 'bool
  :group 'rsync)

(defcustom rsync-ignore-file-list '()
  "List of files (and directories) to be ignored in rsync operations.
Can be set interactive with the function `rsync--ignore-files'.
This list will be passe to the --exclude= switch, see rsync(1)
for more information."
  :type 'list
  :group 'rsync)

(defvar rsync-executable (executable-find rsync-program)
  "Rsync executable.")

(defvar rsync-host nil
  "Rsync target host string.")

(defvar rsync-user user-login-name
  "Rsync target host string.")

(defvar rsync-src-dir nil
  "Rsync source directory string.")

(defvar rsync-dest-dir nil
  "Rsync destination directory string.")

(defvar rsync-buffer-name "*rsync*"
  "Rysnc process buffer name.")

(defvar rsync-host-history-list '()
  "Rsync host history list.")

(defvar rsync-dest-history-list '()
  "Rsync destination directory history list.")

(defun rsync--process-filter (proc string)
  "Filter rsync PROC ouput STRING."
  (let ((buffer (process-buffer proc)))
    (when buffer
      (with-current-buffer buffer
        (ansi-color-apply-on-region (point-min) (point-max))
        (insert string)))))

(defun rsync--sentinel (process event)
  "Rsync sentinel: PROCESS default EVENT handler function.
This is also a template for another callbacks."
  (let ((status (process-status process))
        (buffer (process-buffer process)))
    ;; debug message
    (when rsync-debug-flag
      (message "Process: %s had the event: %s" process event))
    (cond
     ;; handle exit status
     ((eq status 'exit)
      (when rsync-debug-flag (message "Process: %s finishes" process))
      (when rsync-kill-buffer-flag (kill-buffer buffer)))
     ;; TODO condition-case to handle process status (signals, exit, etc...)
     ;; TODO: research if its necessary and how to do:
     ;; verify the process status
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
  (let* ((buffer (if rsync-create-buffer-flag
                     (get-buffer-create rsync-buffer-name)))
         (proc (apply 'start-process
                      rsync-program buffer rsync-executable program-args)))
    ;; verify if process was correctly created
    (unless proc
      (error "Was not possible to create rsync process"))
    ;; set (default or callback) sentinel
    (rsync--set-sentinel proc sentinel)
    ;; set default filter
    (set-process-filter proc 'rsync--process-filter)))

(defun rsync--read-args (prefix)
  "Read rsync arguments, if PREFIX is non-nil asks for the user."
  (let ((host (read-string "Host: "))
        (user (when prefix
                (read-string "User: " nil nil user-login-name)))
        (src  (read-directory-name "Source: " nil nil 'confirm))
        (dest (read-directory-name "Destination: ")))
    (list host
          (expand-file-name src)
          (expand-file-name dest)
          (or user (getenv "USER")))))

(defun rsync--parse-args (operation host src dest &optional user)
  "Parse HOST SRC DEST USER arguments based on the rysnc OPERATION.

Pull: rsync [OPTION...] [USER@]HOST:SRC... [DEST]
Push: rsync [OPTION...] SRC... [USER@]HOST:DEST

This function return rsync string arguments list."

  ;; parse rsync mandatory arguments and options
  (let* ((host (if user (concat user "@" host) host))
         (password (rsync-lookup-password host user))
         (options (split-string rsync-switches)))
    ;; if password, use/set rsync password environment variable
    (when password (setenv "RSYNC_PASSWORD" password))
    ;; the operation (pull, push) determine the other
    ;; of the arguments
    (if (eq operation 'pull)
        (setq src (concat host ":" src))
      (setq dest (concat host ":" dest)))
    (append options (list src dest) nil)))

(defun rsync-lookup-password (host user)
  "Lookup using (HOST USER) password on auth-source default file."
  (let* ((auth (auth-source-search :host host :user user))
         (secretf (when auth (plist-get (car auth) :secret))))
    (cond
     ((not auth) nil)
     ((not secretf) nil)
     (t (funcall secretf)))))

(defun rsync-transfer-files (program-args &optional sentinel)
  "The rsync transfer files operation.

PROGRAM-ARGS rsync parsed arguments.
SENTINEL (callback) function to handle process signals/status."
  ;; start transfer procedure (invoke rsync spirit)
  (rsync--start-process program-args (or sentinel 'rsync--sentinel)))

;;;###autoload
(defun rsync-push (host src dest &optional user sentinel)
  "The rsync push operation.

HOST Remote host identifier.
The host name can be a alises defined in ~/.ssh/config or /etc/hosts,
auth-sources interface will be provided soon.

SRC  Source directory.
DEST Destination directory.

&OPTIONAL:

USER     User identifier.
SENTINEL Function to handle process events (callback)."

  (interactive
   (rsync--read-args current-prefix-arg))

  (if (not rsync-executable)
      (message "Rsync executable not found")
    ;; parse arguments and transfer files
    (rsync-transfer-files
     (rsync--parse-args 'push host src dest user) sentinel)))

;;;###autoload
(defun rsync-pull (host src dest &optional user sentinel)
  "The rsync pull operation.

HOST Remote host identifier.
The host name can be a alises defined in ~/.ssh/config or /etc/hosts,
auth-sources interface will be provided soon.

SRC  Source directory.
DEST Destination directory.

&OPTIONAL:

USER     User identifier.
SENTINEL Function to handle process events."

  (interactive (rsync--read-args current-prefix-arg))
  ;; verify rsync executable was found
  (if (not rsync-executable)
      (message "Rsync executable not found")
    ;; parse arguments and transfer files
    (rsync-transfer-files
     (rsync--parse-args 'pull host src dest user) sentinel)))

(provide 'rsync)
;;; rsync.el ends here
