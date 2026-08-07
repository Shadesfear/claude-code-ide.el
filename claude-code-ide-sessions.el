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
;;             attach to background agents with `claude attach')
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

(defcustom claude-code-ide-session-list-kinds '("background")
  "Session kinds the overview shows, or nil for every kind.
`claude agents' manages background agents, and its TUI lists only
those.  `claude agents --json' also reports interactive sessions, which
are the Claude processes already attached to a terminal elsewhere.  The
overview filters to background agents so it agrees with the CLI."
  :type '(choice (const :tag "Background agents only" ("background"))
                 (const :tag "Every kind" nil)
                 (repeat :tag "Named kinds" string))
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-cache-max-age 5
  "Seconds a cached session list stays fresh enough to reuse.
`claude-code-ide-session-find' reuses the cache within this window
instead of paying for another CLI call, which costs about half a
second.  An open sidebar refreshes the cache on its own timer, so the
finder usually opens instantly."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-list-sidebar-side 'left
  "Side of the frame used by the agent overview sidebar."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-list-sidebar-width 40
  "Width in columns of the agent overview sidebar."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-session-list-sidebar-refresh-interval 5
  "Seconds between refreshes while the overview sidebar is visible.
The timer only runs while the sidebar is displayed, and each refresh
runs `claude agents --json' asynchronously so Emacs never blocks on it.
Set to nil to refresh the sidebar manually."
  :type '(choice (const :tag "Manual only" nil)
                 (number :tag "Seconds"))
  :group 'claude-code-ide)

(defconst claude-code-ide-session-list-buffer-name "*Claude Agents*"
  "Name of the buffer used for the Claude Code agent overview.")

(defconst claude-code-ide-session-list-sidebar-buffer-name "*Claude Agents Sidebar*"
  "Name of the buffer used for the agent overview sidebar.")

(defvar-local claude-code-ide--session-list-timer nil
  "Auto-refresh timer local to a session overview buffer.")

(defvar-local claude-code-ide--session-list-compact nil
  "When non-nil, render the overview with the narrow sidebar columns.")

(defvar claude-code-ide--session-cache nil
  "Sessions from the last completed fetch.
Holds the plists returned by `claude-code-ide--agents-parse'.  The
overview draws from this cache so redisplay never blocks on the CLI.
Shared rather than buffer-local: there is one agent view per machine,
so the sidebar's polling keeps the finder's data warm too.")

(defvar claude-code-ide--session-cache-time nil
  "Value of `float-time' when `claude-code-ide--session-cache' was filled.")

(defvar-local claude-code-ide--session-fetch-process nil
  "In-flight `claude agents --json' process for this overview buffer.")

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

(defun claude-code-ide--agents-command ()
  "Return the CLI program and arguments used to list sessions."
  (let ((cli (or (and (boundp 'claude-code-ide-cli-path) claude-code-ide-cli-path)
                 "claude")))
    (unless (executable-find cli)
      (user-error "Claude Code CLI not found: %s" cli))
    (append (list cli "agents" "--json")
            (when claude-code-ide-session-list-include-completed
              (list "--all")))))

(defun claude-code-ide--agents-store (sessions)
  "Record SESSIONS as the current cache and return them."
  (setq claude-code-ide--session-cache sessions
        claude-code-ide--session-cache-time (float-time))
  sessions)

(defun claude-code-ide--agents-fetch-sync ()
  "Fetch and cache the session list, blocking until the CLI returns.
Used where a result is needed before the command can continue, such as
building the completion table.  Signals a `user-error' when the CLI
fails."
  (let* ((command (claude-code-ide--agents-command))
         (output nil)
         (status nil))
    (with-temp-buffer
      (setq status (apply #'call-process (car command) nil t nil (cdr command)))
      (setq output (buffer-string)))
    (unless (eq status 0)
      (user-error "`%s agents --json' failed (exit %s): %s"
                  (car command) status (string-trim output)))
    (claude-code-ide--agents-store (claude-code-ide--agents-parse output))))

(defun claude-code-ide--sessions-now ()
  "Return the session list, reusing the cache while it is fresh.
Falls back to a synchronous fetch once the cache is older than
`claude-code-ide-session-cache-max-age'."
  (if (and claude-code-ide--session-cache-time
           (< (- (float-time) claude-code-ide--session-cache-time)
              claude-code-ide-session-cache-max-age))
      claude-code-ide--session-cache
    (claude-code-ide--agents-fetch-sync)))

(defun claude-code-ide--agents-fetch-async (buffer callback)
  "Fetch sessions for BUFFER and call CALLBACK inside it when done.
CALLBACK receives SESSIONS and ERROR: on success SESSIONS is the parsed
list and ERROR is nil, on failure SESSIONS is nil and ERROR is a string.
The CLI call takes roughly half a second, so it runs in a subprocess
rather than blocking redisplay.  Returns the process."
  (let* ((command (claude-code-ide--agents-command))
         (stdout (generate-new-buffer " *claude-agents-json*")))
    (make-process
     :name "claude-agents"
     :buffer stdout
     :command command
     :noquery t
     :connection-type 'pipe
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((status (process-exit-status process))
               (output (with-current-buffer stdout (buffer-string))))
           (kill-buffer stdout)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (if (eq status 0)
                   (funcall callback (claude-code-ide--agents-parse output) nil)
                 (funcall callback
                          nil
                          (format "`%s agents --json' failed (exit %s): %s"
                                  (car command) status
                                  (string-trim output))))))))))))

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

(defun claude-code-ide--visible-sessions (sessions)
  "Return the members of SESSIONS the overview should show.
Filters on `claude-code-ide-session-list-kinds'.  Applied to the cache
rather than the fetch, so widening the filter needs no new CLI call."
  (if (null claude-code-ide-session-list-kinds)
      sessions
    (seq-filter (lambda (session)
                  (member (plist-get session :kind)
                          claude-code-ide-session-list-kinds))
                sessions)))

(defun claude-code-ide--session-row-name (session)
  "Return the label for SESSION, falling back to its directory basename."
  (let ((name (plist-get session :name))
        (cwd (or (plist-get session :cwd) "")))
    (if (and (stringp name) (> (length name) 0))
        name
      (concat "("
              (if (> (length cwd) 0)
                  (file-name-nondirectory (directory-file-name cwd))
                "session")
              ")"))))

(defun claude-code-ide--session-list-entries-from (sessions now-seconds &optional compact)
  "Build `tabulated-list-entries' rows from SESSIONS at NOW-SECONDS.
SESSIONS is a list of plists as returned by `claude-code-ide--agents-parse'.
The entry id is the session plist itself so commands can read its fields.
With COMPACT, emit the three narrow columns used by the sidebar instead of
the full six, dropping the directory and session id that will not fit."
  (mapcar
   (lambda (session)
     (let* ((cwd (or (plist-get session :cwd) ""))
            (kind (or (plist-get session :kind) ""))
            (display-name (claude-code-ide--session-row-name session))
            (state (claude-code-ide--agent-state-cell session))
            (age (claude-code-ide--format-age (plist-get session :startedAt)
                                              now-seconds)))
       (list session
             (if compact
                 (vector display-name state age)
               (vector
                display-name
                kind
                state
                age
                (if (> (length cwd) 0) (abbreviate-file-name cwd) "-")
                (claude-code-ide--agent-short-id (plist-get session :sessionId)))))))
   sessions))

(defun claude-code-ide--session-list-entries ()
  "Build overview rows from the last completed fetch.
Reads `claude-code-ide--session-cache' rather than calling the CLI, so
redisplay stays synchronous and cheap.  Ages recompute on every print."
  (claude-code-ide--session-list-entries-from
   (claude-code-ide--visible-sessions claude-code-ide--session-cache)
   (float-time)
   claude-code-ide--session-list-compact))

(defconst claude-code-ide--session-state-order
  '(("blocked" . 0)
    ("working" . 1)
    ("busy" . 1)
    ("idle" . 2)
    ("failed" . 3)
    ("done" . 4))
  "Sort rank for session states, most in need of attention first.
Sessions whose state is not listed sort after all of these.")

(defun claude-code-ide--session-state-rank (session)
  "Return the sort rank of SESSION from `claude-code-ide--session-state-order'."
  (let ((label (or (plist-get session :state) (plist-get session :status) "")))
    (or (cdr (assoc-string label claude-code-ide--session-state-order t)) 5)))

(defun claude-code-ide--session-less-p (a b)
  "Return non-nil when session A sorts before session B.
Orders by state rank first, then newest session first within a state."
  (let ((ra (claude-code-ide--session-state-rank a))
        (rb (claude-code-ide--session-state-rank b)))
    (if (/= ra rb)
        (< ra rb)
      (> (or (plist-get a :startedAt) 0)
         (or (plist-get b :startedAt) 0)))))

(defun claude-code-ide--session-sort-predicate (a b)
  "Return non-nil when overview entry A sorts before B.
A and B are `tabulated-list-entries' elements whose car is the session
plist, so this reads the raw fields rather than the rendered columns."
  (claude-code-ide--session-less-p (car a) (car b)))

(defconst claude-code-ide--session-list-format-full
  [("Name" 28 t)
   ("Kind" 6 t)
   ("State" 9 claude-code-ide--session-sort-predicate)
   ("Age" 6 t)
   ("Directory" 32 t)
   ("Session" 8 nil)]
  "Column layout for the overview in a normal window.")

(defconst claude-code-ide--session-list-format-compact
  [("Name" 22 t)
   ("State" 9 claude-code-ide--session-sort-predicate)
   ("Age" 5 t)]
  "Column layout for the overview in a narrow sidebar.")

(defun claude-code-ide--session-list-apply-format ()
  "Install the column layout matching `claude-code-ide--session-list-compact'."
  (setq tabulated-list-format
        (if claude-code-ide--session-list-compact
            claude-code-ide--session-list-format-compact
          claude-code-ide--session-list-format-full))
  (tabulated-list-init-header))

(defun claude-code-ide-session-list-refresh ()
  "Refresh the session overview from the agent view.
The CLI runs asynchronously and the table redraws when it returns, so
this never blocks.  A refresh started while one is already in flight is
ignored rather than queued."
  (interactive)
  (unless (process-live-p claude-code-ide--session-fetch-process)
    (setq claude-code-ide--session-fetch-process
          (claude-code-ide--agents-fetch-async
           (current-buffer)
           (lambda (sessions error)
             (setq claude-code-ide--session-fetch-process nil)
             (if error
                 (message "%s" error)
               (claude-code-ide--agents-store sessions)
               (let ((inhibit-read-only t))
                 (tabulated-list-print t t))))))))

(defun claude-code-ide-session-list-toggle-scope ()
  "Widen the overview to every session, or narrow it back to agents.
The narrow scope matches the `claude agents' TUI: background agents
that have not finished.  The wide scope adds interactive sessions and
completed ones.  Widening needs a fresh fetch because completed
sessions only arrive with the CLI's --all flag."
  (interactive)
  (let ((widen (and claude-code-ide-session-list-kinds t)))
    (setq claude-code-ide-session-list-kinds (if widen nil '("background"))
          claude-code-ide-session-list-include-completed widen
          ;; --all changes the command, so the cached answer no longer applies.
          claude-code-ide--session-cache-time nil)
    (message "Claude sessions: %s"
             (if widen "every session" "background agents"))
    (claude-code-ide-session-list-refresh)))

(defun claude-code-ide--session-at-point ()
  "Return the session plist on the current overview row.
Signal a `user-error' when point is not on a session row."
  (or (tabulated-list-get-id)
      (user-error "No Claude session on this line")))

(defun claude-code-ide--window-reusable-p (window)
  "Return non-nil when WINDOW may be taken over by a session terminal.
Side windows and dedicated windows may not: those hold the sidebar or
another session, and replacing their buffer loses what the user was
looking at."
  (not (or (window-parameter window 'window-side)
           (window-dedicated-p window))))

(defun claude-code-ide--reusable-window ()
  "Return the window a session terminal should take over, or nil.
Prefers the selected window, then any other ordinary window, so a
session opened from the sidebar still lands in the editing area rather
than in the side window `display-buffer-alist' reserves."
  (if (claude-code-ide--window-reusable-p (selected-window))
      (selected-window)
    (seq-find #'claude-code-ide--window-reusable-p (window-list nil 'never))))

(defun claude-code-ide--same-window-action ()
  "Return a `display-buffer' action reusing the selected window, or nil."
  (when (claude-code-ide--window-reusable-p (selected-window))
    '((display-buffer-same-window) (inhibit-same-window . nil))))

(defun claude-code-ide--agent-launch (buffer-name args dir &optional same-window)
  "Run the Claude CLI with ARGS in terminal BUFFER-NAME rooted at DIR.
Uses the configured `claude-code-ide-terminal-backend'.  With
SAME-WINDOW, show the terminal in the selected window instead of letting
`display-buffer-alist' place it, falling back to the usual placement
when the selected window cannot be taken over."
  (claude-code-ide--terminal-ensure-backend)
  ;; Step out of an unusable window first, so the terminal still lands in
  ;; the editing area when the finder runs from the sidebar.
  (when same-window
    (when-let* ((target (claude-code-ide--reusable-window)))
      (unless (eq target (selected-window))
        (select-window target))))
  (let* ((cli (or claude-code-ide-cli-path "claude"))
         (dir (file-name-as-directory
               (or (and (stringp dir) (> (length dir) 0) dir)
                   default-directory)))
         (default-directory dir)
         (display-buffer-overriding-action
          (or (and same-window (claude-code-ide--same-window-action))
              display-buffer-overriding-action)))
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

(defun claude-code-ide--agent-attach-id (session)
  "Return the id used to attach to SESSION with `claude attach'.
Prefers the short :id shown in the overview, falling back to the
full :sessionId.  Returns nil when neither is available."
  (let ((id (plist-get session :id))
        (sid (plist-get session :sessionId)))
    (cond ((and (stringp id) (> (length id) 0)) id)
          ((and (stringp sid) (> (length sid) 0)) sid))))

(defun claude-code-ide--agent-enter-args (session)
  "Return the Claude CLI args to enter SESSION.
Interactive sessions resume by id (\"--resume ID\").  Background
agents attach to the running session in this terminal (\"attach ID\"),
using the id shown in the overview, so the live agent is entered
directly; detach with C-z to leave it running."
  (if (equal (plist-get session :kind) "background")
      (list "attach" (claude-code-ide--agent-attach-id session))
    (list "--resume" (plist-get session :sessionId))))

(defun claude-code-ide--session-enter (session &optional same-window)
  "Enter SESSION in a terminal buffer.
Interactive sessions are resumed by id.  Background agents are attached
to (`claude attach ID'), so the live agent is entered directly; detach
with C-z to leave it running.  SAME-WINDOW reuses the selected window
rather than the side window `display-buffer-alist' would pick."
  (let* ((background (equal (plist-get session :kind) "background"))
         (id (if background
                 (claude-code-ide--agent-attach-id session)
               (plist-get session :sessionId)))
         (buffer-name (format "*claude-agent[%s]*"
                              (claude-code-ide--agent-short-id
                               (or (plist-get session :sessionId) id)))))
    (unless (and (stringp id) (> (length id) 0))
      (user-error "Session has no session id"))
    (claude-code-ide--agent-launch buffer-name
                                   (claude-code-ide--agent-enter-args session)
                                   (plist-get session :cwd)
                                   same-window)))

(defun claude-code-ide-session-list-visit ()
  "Enter the Claude session on the current line."
  (interactive)
  (claude-code-ide--session-enter (claude-code-ide--session-at-point)))

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

(defun claude-code-ide--session-candidate (session now-seconds)
  "Return the completion line for SESSION at NOW-SECONDS.
Name, state, age and directory all sit in one string so a fuzzy
completion style can match on any of them."
  (let ((cwd (or (plist-get session :cwd) "")))
    (format "%-44s  %-8s %-4s  %s"
            (truncate-string-to-width
             (claude-code-ide--session-row-name session) 44 nil nil t)
            (or (plist-get session :state) (plist-get session :status) "")
            (claude-code-ide--format-age (plist-get session :startedAt) now-seconds)
            (if (> (length cwd) 0) (abbreviate-file-name cwd) "-"))))

(defun claude-code-ide--session-completion-table (candidates)
  "Return a completion table over CANDIDATES preserving their order.
CANDIDATES is an alist of display string to session plist.  The table
declines to re-sort so the state-then-age ranking survives into the
minibuffer."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (category . claude-code-ide-session)
                   (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string predicate))))

;;;###autoload
(defun claude-code-ide-session-find ()
  "Pick a Claude session by name and enter it.
Completes over every session in the agent view, ranked the same way as
the overview, and enters the chosen one in a terminal.  Reuses a cache
younger than `claude-code-ide-session-cache-max-age', so this is
instant while the sidebar is polling."
  (interactive)
  (let ((sessions (claude-code-ide--visible-sessions
                   (claude-code-ide--sessions-now))))
    (unless sessions
      (user-error "No Claude sessions found"))
    (let* ((now (float-time))
           (candidates
            (mapcar (lambda (session)
                      (cons (claude-code-ide--session-candidate session now)
                            session))
                    (sort (copy-sequence sessions)
                          #'claude-code-ide--session-less-p)))
           (choice (completing-read
                    "Claude session: "
                    (claude-code-ide--session-completion-table candidates)
                    nil t))
           (session (cdr (assoc choice candidates))))
      (unless session
        (user-error "No session matched %S" choice))
      (claude-code-ide--session-enter session t))))

(defun claude-code-ide--session-list-cancel-timer ()
  "Cancel the auto-refresh timer for the current overview buffer."
  (when (timerp claude-code-ide--session-list-timer)
    (cancel-timer claude-code-ide--session-list-timer))
  (setq claude-code-ide--session-list-timer nil))

(defun claude-code-ide--session-list-start-timer (interval)
  "Refresh the current overview buffer every INTERVAL seconds while visible.
The timer cancels itself once the buffer stops being displayed, so a
closed sidebar costs nothing.  A nil or non-positive INTERVAL starts
no timer."
  (claude-code-ide--session-list-cancel-timer)
  (when (and (numberp interval) (> interval 0))
    (let ((buffer (current-buffer))
          (timer nil))
      (setq timer
            (run-at-time
             interval interval
             (lambda ()
               (if (and (buffer-live-p buffer) (get-buffer-window buffer t))
                   (with-current-buffer buffer
                     (when (derived-mode-p 'claude-code-ide-session-list-mode)
                       (ignore-errors (claude-code-ide-session-list-refresh))))
                 (cancel-timer timer)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq claude-code-ide--session-list-timer nil)))))))
      (setq claude-code-ide--session-list-timer timer)
      (add-hook 'kill-buffer-hook
                #'claude-code-ide--session-list-cancel-timer nil t))))

(defun claude-code-ide--session-list-maybe-start-timer ()
  "Start the auto-refresh timer when an interval is configured."
  (claude-code-ide--session-list-start-timer
   claude-code-ide-session-list-auto-refresh-interval))

(defvar-keymap claude-code-ide-session-list-mode-map
  :doc "Keymap for `claude-code-ide-session-list-mode'."
  "RET" #'claude-code-ide-session-list-visit
  "o"   #'claude-code-ide-session-list-visit
  "f"   #'claude-code-ide-session-list-fork
  "a"   #'claude-code-ide-session-list-toggle-scope
  "g"   #'claude-code-ide-session-list-refresh)

(define-derived-mode claude-code-ide-session-list-mode tabulated-list-mode
  "CC-Agents"
  "Major mode for an overview of all Claude Code agent sessions."
  (setq tabulated-list-padding 2
        tabulated-list-sort-key (cons "State" nil)
        tabulated-list-entries #'claude-code-ide--session-list-entries)
  (claude-code-ide--session-list-apply-format))

;;;###autoload
(defun claude-code-ide-session-list ()
  "Display a live overview of all Claude Code sessions on this machine.
Sessions are read from the native agent view (`claude agents --json'),
including ones started outside Emacs.  Press RET on a row to enter that
session in a terminal (resume interactive sessions, attach to background
agents)."
  (interactive)
  (let ((buffer (get-buffer-create claude-code-ide-session-list-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'claude-code-ide-session-list-mode)
        (claude-code-ide-session-list-mode))
      (setq claude-code-ide--session-list-compact nil)
      (claude-code-ide--session-list-apply-format)
      (tabulated-list-print t)
      (claude-code-ide--session-list-maybe-start-timer)
      (claude-code-ide-session-list-refresh))
    (pop-to-buffer buffer)))

(defvar claude-code-ide--session-list-return-window nil
  "Window to return to when leaving the overview sidebar.")

;;;###autoload
(defun claude-code-ide-session-list-sidebar (&optional no-select)
  "Show the agent overview in a side window and keep it refreshed.
The sidebar uses the narrow column layout and polls every
`claude-code-ide-session-list-sidebar-refresh-interval' seconds while it
stays visible.  Point moves into the sidebar unless NO-SELECT is non-nil.
Returns the sidebar window."
  (interactive)
  (let ((buffer (get-buffer-create claude-code-ide-session-list-sidebar-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'claude-code-ide-session-list-mode)
        (claude-code-ide-session-list-mode))
      (setq claude-code-ide--session-list-compact t)
      (claude-code-ide--session-list-apply-format)
      (tabulated-list-print t))
    (let ((window (display-buffer-in-side-window
                   buffer
                   `((side . ,claude-code-ide-session-list-sidebar-side)
                     (slot . 0)
                     (window-width . ,claude-code-ide-session-list-sidebar-width)
                     (preserve-size . (t . nil))
                     (window-parameters . ((no-delete-other-windows . t)))))))
      (when window
        (set-window-dedicated-p window t)
        (with-current-buffer buffer
          (claude-code-ide--session-list-start-timer
           claude-code-ide-session-list-sidebar-refresh-interval)
          (claude-code-ide-session-list-refresh))
        (unless no-select
          (setq claude-code-ide--session-list-return-window (selected-window))
          (select-window window)))
      window)))

(defun claude-code-ide--session-list-sidebar-window ()
  "Return the window showing the overview sidebar, or nil."
  (let ((buffer (get-buffer claude-code-ide-session-list-sidebar-buffer-name)))
    (and buffer (get-buffer-window buffer t))))

;;;###autoload
(defun claude-code-ide-session-list-sidebar-toggle ()
  "Toggle the agent overview sidebar, moving point into it when it opens.
Closing it stops the refresh timer."
  (interactive)
  (let ((window (claude-code-ide--session-list-sidebar-window)))
    (if window
        (progn
          (with-current-buffer (window-buffer window)
            (claude-code-ide--session-list-cancel-timer))
          (delete-window window))
      (claude-code-ide-session-list-sidebar))))

;;;###autoload
(defun claude-code-ide-session-list-sidebar-toggle-focus ()
  "Move point into the overview sidebar, or back out of it.
Opens the sidebar first when it is not showing.  Leaves it open either
way, so this switches focus without losing the list."
  (interactive)
  (let ((window (claude-code-ide--session-list-sidebar-window)))
    (cond
     ((null window)
      (claude-code-ide-session-list-sidebar))
     ((eq window (selected-window))
      (if (window-live-p claude-code-ide--session-list-return-window)
          (select-window claude-code-ide--session-list-return-window)
        (other-window 1)))
     (t
      (setq claude-code-ide--session-list-return-window (selected-window))
      (select-window window)))))

(provide 'claude-code-ide-sessions)

;;; claude-code-ide-sessions.el ends here
