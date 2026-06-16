;;; claude-code-ide-sessions.el --- Session overview for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Keywords: ai, claude, sessions, overview

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a `tabulated-list-mode' overview of all active
;; Claude Code sessions, analogous to a fleet view.  Each running
;; session is shown on its own row with its project, liveness, MCP
;; connection state, process id and port.  From the overview the user
;; can press RET to jump into a session's buffer, or stop a session.
;;
;; Open it with `M-x claude-code-ide-session-list'.
;;
;; Key bindings inside the overview:
;;   RET / o   visit the session at point in its side window
;;   g         refresh the list (pruning dead sessions)
;;   k / d     stop the session at point
;;   q         quit the overview window

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
;; Loading this overview pulls in the core package: the autoload entry
;; point for `claude-code-ide-session-list' must bring the session
;; registry and helpers (`claude-code-ide--cleanup-dead-processes' etc.)
;; with it.  The core package does not require this file, so there is no
;; load cycle.  The MCP session accessors arrive transitively via
;; claude-code-ide -> claude-code-ide-mcp.
(require 'claude-code-ide)

(defcustom claude-code-ide-session-list-auto-refresh-interval nil
  "Seconds between automatic refreshes of the session overview buffer.
When nil, the overview only refreshes manually via \\<claude-code-ide-session-list-mode-map>\\[claude-code-ide-session-list-refresh]."
  :type '(choice (const :tag "Manual only" nil)
                 (number :tag "Seconds"))
  :group 'claude-code-ide)

(defconst claude-code-ide-session-list-buffer-name "*Claude Code Sessions*"
  "Name of the buffer used for the Claude Code session overview.")

(defvar-local claude-code-ide--session-list-timer nil
  "Auto-refresh timer local to a session overview buffer.")

(defun claude-code-ide--collect-sessions ()
  "Return a list of plists describing the tracked Claude Code sessions.
Each plist contains :directory, :buffer, :process, :live, :connected,
:pid and :port.  Returns the empty list when no sessions are tracked."
  (let ((sessions '()))
    (when (hash-table-p claude-code-ide--processes)
      (maphash
       (lambda (directory process)
         (let* ((buffer-name (funcall claude-code-ide-buffer-name-function directory))
                (buffer (get-buffer buffer-name))
                (mcp-session (and (hash-table-p claude-code-ide-mcp--sessions)
                                  (gethash directory claude-code-ide-mcp--sessions))))
           (push (list :directory directory
                       :buffer buffer
                       :process process
                       :live (and (process-live-p process) t)
                       :connected (and mcp-session
                                       (claude-code-ide-mcp-session-client mcp-session)
                                       t)
                       :pid (and mcp-session
                                 (claude-code-ide-mcp-session-cli-pid mcp-session))
                       :port (and mcp-session
                                  (claude-code-ide-mcp-session-port mcp-session)))
                 sessions)))
       claude-code-ide--processes))
    (nreverse sessions)))

(defun claude-code-ide--session-list-entries ()
  "Build `tabulated-list-entries' rows from the tracked sessions."
  (mapcar
   (lambda (session)
     (let ((directory (plist-get session :directory))
           (pid (plist-get session :pid))
           (port (plist-get session :port)))
       (list directory
             (vector
              (file-name-nondirectory (directory-file-name directory))
              (if (plist-get session :live)
                  (propertize "live" 'face 'success)
                (propertize "dead" 'face 'error))
              (if (plist-get session :connected)
                  (propertize "yes" 'face 'success)
                (propertize "no" 'face 'shadow))
              (if pid (number-to-string pid) "-")
              (if port (number-to-string port) "-")
              (abbreviate-file-name directory)))))
   (claude-code-ide--collect-sessions)))

(defun claude-code-ide-session-list-refresh ()
  "Prune dead sessions and redraw the session overview."
  (interactive)
  (claude-code-ide--cleanup-dead-processes)
  (tabulated-list-print t))

(defun claude-code-ide--session-list-directory-at-point ()
  "Return the session directory on the current overview row.
Signal a `user-error' when point is not on a session row."
  (or (tabulated-list-get-id)
      (user-error "No Claude Code session on this line")))

(defun claude-code-ide-session-list-visit ()
  "Visit the Claude Code session on the current line in its side window."
  (interactive)
  (let* ((directory (claude-code-ide--session-list-directory-at-point))
         (buffer-name (funcall claude-code-ide-buffer-name-function directory))
         (buffer (get-buffer buffer-name)))
    (if (buffer-live-p buffer)
        (progn
          (claude-code-ide--display-buffer-in-side-window buffer)
          (when-let* ((window (get-buffer-window buffer t)))
            (select-window window)))
      (user-error "Buffer for session %s no longer exists"
                  (abbreviate-file-name directory)))))

(defun claude-code-ide-session-list-stop ()
  "Stop the Claude Code session on the current line.
Killing the session buffer triggers the package's normal cleanup
hooks; the overview is then refreshed."
  (interactive)
  (let ((directory (claude-code-ide--session-list-directory-at-point)))
    (when (yes-or-no-p (format "Stop Claude Code session in %s? "
                               (abbreviate-file-name directory)))
      (let* ((buffer-name (funcall claude-code-ide-buffer-name-function directory))
             (buffer (get-buffer buffer-name)))
        (if (buffer-live-p buffer)
            (kill-buffer buffer)
          ;; No buffer left; drop the stale registry entries directly.
          (when (hash-table-p claude-code-ide--processes)
            (remhash directory claude-code-ide--processes))
          (when (hash-table-p claude-code-ide--session-ids)
            (remhash directory claude-code-ide--session-ids)))
        (claude-code-ide-session-list-refresh)))))

(defun claude-code-ide--session-list-cancel-timer ()
  "Cancel the auto-refresh timer for the current overview buffer."
  (when (timerp claude-code-ide--session-list-timer)
    (cancel-timer claude-code-ide--session-list-timer))
  (setq claude-code-ide--session-list-timer nil))

(defun claude-code-ide--session-list-tick (buffer)
  "Refresh the overview in BUFFER, or cancel the timer if it is gone."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'claude-code-ide-session-list-mode)
          (claude-code-ide-session-list-refresh)))
    (cancel-timer claude-code-ide--session-list-timer)))

(defun claude-code-ide--session-list-maybe-start-timer ()
  "Start the auto-refresh timer when an interval is configured."
  (claude-code-ide--session-list-cancel-timer)
  (when (and claude-code-ide-session-list-auto-refresh-interval
             (> claude-code-ide-session-list-auto-refresh-interval 0))
    (setq claude-code-ide--session-list-timer
          (run-at-time claude-code-ide-session-list-auto-refresh-interval
                       claude-code-ide-session-list-auto-refresh-interval
                       #'claude-code-ide--session-list-tick
                       (current-buffer)))
    (add-hook 'kill-buffer-hook
              #'claude-code-ide--session-list-cancel-timer nil t)))

(defvar-keymap claude-code-ide-session-list-mode-map
  :doc "Keymap for `claude-code-ide-session-list-mode'."
  "RET" #'claude-code-ide-session-list-visit
  "o"   #'claude-code-ide-session-list-visit
  "g"   #'claude-code-ide-session-list-refresh
  "k"   #'claude-code-ide-session-list-stop
  "d"   #'claude-code-ide-session-list-stop)

(define-derived-mode claude-code-ide-session-list-mode tabulated-list-mode
  "CC-Sessions"
  "Major mode for an overview of active Claude Code sessions."
  (setq tabulated-list-format
        [("Project" 22 t)
         ("Status" 7 t)
         ("Conn" 5 t)
         ("PID" 8 t)
         ("Port" 6 t)
         ("Directory" 0 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Project" nil)
        tabulated-list-entries #'claude-code-ide--session-list-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun claude-code-ide-session-list ()
  "Display a live overview of all active Claude Code sessions.
Sessions are listed in a `tabulated-list-mode' buffer.  Press RET on a
row to jump into that session's buffer."
  (interactive)
  (claude-code-ide--cleanup-dead-processes)
  (let ((buffer (get-buffer-create claude-code-ide-session-list-buffer-name)))
    (with-current-buffer buffer
      (claude-code-ide-session-list-mode)
      (tabulated-list-print t)
      (claude-code-ide--session-list-maybe-start-timer))
    (pop-to-buffer buffer)))

(provide 'claude-code-ide-sessions)

;;; claude-code-ide-sessions.el ends here
