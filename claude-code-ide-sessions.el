;;; claude-code-ide-sessions.el --- Agent session overview for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Keywords: ai, claude, sessions, agents, overview

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

;; This file provides a `tabulated-list-mode' overview of all Claude
;; Code sessions on the machine -- including ones started outside Emacs
;; -- by reading the native agent view via `claude agents --json'.
;;
;; Each session (interactive or background) is shown on its own row with
;; its name, kind, state, age, working directory and short session id.
;; Pressing RET on a row resumes that session by id in a terminal buffer
;; using the configured terminal backend, so an externally started
;; session can be entered from Emacs.
;;
;; Open it with `M-x claude-code-ide-session-list'.
;;
;; Key bindings inside the overview:
;;   RET / o   enter the session at point (resume interactive sessions;
;;             open the native agent view to attach background agents)
;;   f         fork a copy of the session at point
;;   g         refresh the list
;;   q         quit the overview window

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
;; Loading this overview pulls in the core package so the terminal
;; backend helpers and customizations are available.  The core package
;; does not require this file, so there is no load cycle.
(require 'claude-code-ide)

;; Optional terminal backends are provided by third-party packages and are
;; only touched at runtime once `claude-code-ide--terminal-ensure-backend'
;; has confirmed the configured one is available.
(defvar vterm-buffer-name)
(defvar vterm-shell)
(defvar eat-term-name)
(declare-function vterm "vterm" (&optional buffer-name))
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile &rest switches))
(declare-function ghostel-exec "ghostel" (buffer program &optional args))

(defcustom claude-code-ide-session-list-auto-refresh-interval nil
  "Seconds between automatic refreshes of the session overview buffer.
When nil, the overview only refreshes manually via \\<claude-code-ide-session-list-mode-map>\\[claude-code-ide-session-list-refresh]."
  :type '(choice (const :tag "Manual only" nil)
                 (number :tag "Seconds"))
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-list-include-completed nil
  "When non-nil, include completed sessions in the overview.
This passes the --all flag to `claude agents --json'."
  :type 'boolean
  :group 'claude-code-ide)

(defconst claude-code-ide-session-list-buffer-name "*Claude Agents*"
  "Name of the buffer used for the Claude Code agent overview.")

(defvar-local claude-code-ide--session-list-timer nil
  "Auto-refresh timer local to a session overview buffer.")

(defun claude-code-ide--agents-parse (output)
  "Parse OUTPUT of `claude agents --json' into a list of plists.
Return nil when OUTPUT is empty or not valid JSON."
  (when (and (stringp output) (> (length (string-trim output)) 0))
    (condition-case nil
        (let ((parsed (json-parse-string output
                                         :object-type 'plist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil)))
          (and (listp parsed) parsed))
      (error nil))))

(defun claude-code-ide--agents-fetch ()
  "Return the list of Claude sessions from `claude agents --json'.
Each element is a plist describing one session.  Signals a
`user-error' when the CLI is missing or the command fails."
  (let ((cli (or (and (boundp 'claude-code-ide-cli-path) claude-code-ide-cli-path)
                 "claude")))
    (unless (executable-find cli)
      (user-error "Claude Code CLI not found: %s" cli))
    (let ((args (append (list "agents" "--json")
                        (when claude-code-ide-session-list-include-completed
                          (list "--all"))))
          (output nil)
          (status nil))
      (with-temp-buffer
        (setq status (apply #'call-process cli nil t nil args))
        (setq output (buffer-string)))
      (unless (eq status 0)
        (user-error "`%s agents --json' failed (exit %s): %s"
                    cli status (string-trim output)))
      (claude-code-ide--agents-parse output))))

(defun claude-code-ide--format-age (started-ms now-seconds)
  "Return a short human age for START-MS relative to NOW-SECONDS.
STARTED-MS is an epoch time in milliseconds; NOW-SECONDS is in seconds."
  (if (not (numberp started-ms))
      "-"
    (let ((secs (max 0 (- now-seconds (/ started-ms 1000.0)))))
      (cond
       ((< secs 60) (format "%ds" (floor secs)))
       ((< secs 3600) (format "%dm" (floor (/ secs 60))))
       ((< secs 86400) (format "%dh" (floor (/ secs 3600))))
       (t (format "%dd" (floor (/ secs 86400))))))))

(defun claude-code-ide--agent-state-cell (session)
  "Return a propertized state string for SESSION.
Combines the :state and :status fields and colors by liveness."
  (let* ((state (plist-get session :state))
         (status (plist-get session :status))
         (label (or state status "")))
    (pcase label
      ((or "working" "busy") (propertize label 'face 'warning))
      ("blocked" (propertize label 'face 'error))
      ((or "done" "idle") (propertize label 'face 'shadow))
      (_ label))))

(defun claude-code-ide--agent-short-id (session-id)
  "Return the leading 8 characters of SESSION-ID, or \"-\"."
  (if (and (stringp session-id) (> (length session-id) 0))
      (substring session-id 0 (min 8 (length session-id)))
    "-"))

(defun claude-code-ide--session-list-entries-from (sessions now-seconds)
  "Build `tabulated-list-entries' rows from SESSIONS at NOW-SECONDS.
SESSIONS is a list of plists as returned by `claude-code-ide--agents-fetch'.
The entry id is the session plist itself so commands can read its fields."
  (mapcar
   (lambda (session)
     (let* ((cwd (or (plist-get session :cwd) ""))
            (name (plist-get session :name))
            (kind (or (plist-get session :kind) ""))
            (display-name (if (and (stringp name) (> (length name) 0))
                              name
                            (concat "("
                                    (if (> (length cwd) 0)
                                        (file-name-nondirectory
                                         (directory-file-name cwd))
                                      "session")
                                    ")"))))
       (list session
             (vector
              display-name
              kind
              (claude-code-ide--agent-state-cell session)
              (claude-code-ide--format-age (plist-get session :startedAt) now-seconds)
              (if (> (length cwd) 0) (abbreviate-file-name cwd) "-")
              (claude-code-ide--agent-short-id (plist-get session :sessionId))))))
   sessions))

(defun claude-code-ide--session-list-entries ()
  "Fetch sessions and build rows for the overview."
  (claude-code-ide--session-list-entries-from
   (claude-code-ide--agents-fetch)
   (float-time)))

(defun claude-code-ide-session-list-refresh ()
  "Redraw the session overview from the current agent state."
  (interactive)
  (tabulated-list-print t))

(defun claude-code-ide--session-at-point ()
  "Return the session plist on the current overview row.
Signal a `user-error' when point is not on a session row."
  (or (tabulated-list-get-id)
      (user-error "No Claude session on this line")))

(defun claude-code-ide--agent-launch (buffer-name args dir)
  "Run the Claude CLI with ARGS in terminal BUFFER-NAME rooted at DIR.
Uses the configured `claude-code-ide-terminal-backend'."
  (claude-code-ide--terminal-ensure-backend)
  (let* ((cli (or claude-code-ide-cli-path "claude"))
         (dir (file-name-as-directory
               (or (and (stringp dir) (> (length dir) 0) dir)
                   default-directory)))
         (default-directory dir))
    (pcase claude-code-ide-terminal-backend
      ('vterm
       (let ((vterm-buffer-name buffer-name)
             (vterm-shell (mapconcat #'identity (cons cli args) " ")))
         (vterm buffer-name)))
      ('eat
       (let ((buffer (get-buffer-create buffer-name))
             (eat-term-name "xterm-256color"))
         (with-current-buffer buffer
           (setq default-directory dir)
           (unless (eq major-mode 'eat-mode)
             (eat-mode)))
         ;; Display before exec so the terminal is created at the window's
         ;; size; otherwise it starts mis-sized and a startup resize is
         ;; sent once shown, which Claude Code's fullscreen TUI relayouts
         ;; badly (jumpy output).
         (pop-to-buffer buffer)
         (with-current-buffer buffer
           (eat-exec buffer buffer-name cli nil args))
         buffer))
      ('ghostel
       (let ((buffer (get-buffer-create buffer-name)))
         (with-current-buffer buffer
           (setq default-directory dir))
         ;; Display first: `ghostel-exec' sizes the terminal to the window
         ;; showing BUFFER (or 80x24 when undisplayed).  Sizing it up front
         ;; avoids the startup SIGWINCH that Claude Code's fullscreen TUI
         ;; handles imperfectly (ghostel issue #192) -- the cause of the
         ;; terminal staying mis-sized and the TUI bouncing.
         (pop-to-buffer buffer)
         (with-current-buffer buffer
           (ghostel-exec buffer cli args))
         buffer))
      (other
       (error "Unknown terminal backend: %s" other)))))

(defun claude-code-ide--agent-enter-args (session)
  "Return the Claude CLI args to enter SESSION.
Interactive sessions resume by id (\"--resume ID\").  Background
agents cannot be resumed directly, so this opens the native agent view
\(\"agents\"), filtered to the session's directory, where the agent can
be attached."
  (let ((sid (plist-get session :sessionId))
        (cwd (plist-get session :cwd)))
    (if (equal (plist-get session :kind) "background")
        (append (list "agents")
                (when (and (stringp cwd) (> (length cwd) 0))
                  (list "--cwd" cwd)))
      (list "--resume" sid))))

(defun claude-code-ide-session-list-visit ()
  "Enter the Claude session on the current line.
Interactive sessions are resumed by id in a terminal.  Background
agents open the native agent view (`claude agents'), filtered to the
session's directory, where you can attach to the running agent."
  (interactive)
  (let* ((session (claude-code-ide--session-at-point))
         (sid (plist-get session :sessionId))
         (background (equal (plist-get session :kind) "background"))
         (buffer-name (if background
                          "*claude-agents*"
                        (format "*claude-agent[%s]*"
                                (claude-code-ide--agent-short-id sid)))))
    (when (and (not background)
               (not (and (stringp sid) (> (length sid) 0))))
      (user-error "Session on this line has no session id"))
    (claude-code-ide--agent-launch buffer-name
                                   (claude-code-ide--agent-enter-args session)
                                   (plist-get session :cwd))))

(defun claude-code-ide-session-list-fork ()
  "Fork a copy of the Claude session on the current line into a terminal.
Works for interactive and background sessions: the original keeps
running while you drive an independent branch (\"--resume ID
--fork-session\")."
  (interactive)
  (let* ((session (claude-code-ide--session-at-point))
         (sid (plist-get session :sessionId)))
    (unless (and (stringp sid) (> (length sid) 0))
      (user-error "Session on this line has no session id"))
    (claude-code-ide--agent-launch
     (format "*claude-agent[%s-fork]*" (claude-code-ide--agent-short-id sid))
     (list "--resume" sid "--fork-session")
     (plist-get session :cwd))))

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
          (ignore-errors (claude-code-ide-session-list-refresh))))
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
  "f"   #'claude-code-ide-session-list-fork
  "g"   #'claude-code-ide-session-list-refresh)

(define-derived-mode claude-code-ide-session-list-mode tabulated-list-mode
  "CC-Agents"
  "Major mode for an overview of all Claude Code agent sessions."
  (setq tabulated-list-format
        [("Name" 28 t)
         ("Kind" 6 t)
         ("State" 9 t)
         ("Age" 6 t)
         ("Directory" 32 t)
         ("Session" 8 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Name" nil)
        tabulated-list-entries #'claude-code-ide--session-list-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun claude-code-ide-session-list ()
  "Display a live overview of all Claude Code sessions on this machine.
Sessions are read from the native agent view (`claude agents --json'),
including ones started outside Emacs.  Press RET on a row to resume
that session in a terminal."
  (interactive)
  (let ((buffer (get-buffer-create claude-code-ide-session-list-buffer-name)))
    (with-current-buffer buffer
      (claude-code-ide-session-list-mode)
      (tabulated-list-print t)
      (claude-code-ide--session-list-maybe-start-timer))
    (pop-to-buffer buffer)))

(provide 'claude-code-ide-sessions)

;;; claude-code-ide-sessions.el ends here
