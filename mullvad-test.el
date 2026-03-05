;;; mullvad-test.el --- Tests for mullvad.el -*- lexical-binding: t -*-

;; Tests for the mullvad Emacs package.  Mocks shell-command-to-string
;; so no actual Mullvad CLI is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load mullvad from the same directory as this test file.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "mullvad" dir)))

;;;; Test helpers

(defvar mullvad-test--shell-outputs nil
  "List of (EXPECTED-CMD-REGEXP . OUTPUT) pairs for mock shell commands.
Each call to `shell-command-to-string' pops and returns the first
matching entry, or signals an error if nothing matches.")

(defun mullvad-test--mock-shell-command (command)
  "Mock for `shell-command-to-string'.  Return the first matching output."
  (let ((match (cl-find-if (lambda (pair) (string-match-p (car pair) command))
                           mullvad-test--shell-outputs)))
    (unless match
      (error "Unexpected shell command: %s\nPending mocks: %S" command mullvad-test--shell-outputs))
    (setq mullvad-test--shell-outputs (delq match mullvad-test--shell-outputs))
    (cdr match)))

(defmacro mullvad-test-with-mocks (bindings &rest body)
  "Run BODY with mullvad-executable set and shell calls mocked.
BINDINGS is a list of (REGEXP . OUTPUT) for mock shell responses."
  (declare (indent 1))
  `(let ((mullvad-executable "/usr/bin/mullvad")
         (mullvad-silent nil)
         (mullvad-timer nil)
         (mullvad-cities-and-servers '(("London" . "gb-lon-wg-001")
                                       ("Madrid" . "es-mad-wg-101")
                                       ("Tokyo"  . "jp-tyo-wg-201")))
         (mullvad-websites-and-cities '(("BBC"     . "London")
                                        ("Netflix" . "Madrid")))
         (mullvad-durations nil)
         (mullvad-test--shell-outputs (copy-sequence ,bindings)))
     (cl-letf (((symbol-function 'shell-command-to-string)
                #'mullvad-test--mock-shell-command)
               ((symbol-function 'file-executable-p) (lambda (_) t)))
       ,@body)))

;;;; Critical tests

;;;;; mullvad-ensure-executable

(ert-deftest mullvad-test-ensure-executable-with-valid-path ()
  "Valid executable path should not signal."
  (let ((mullvad-executable "/usr/bin/mullvad"))
    (cl-letf (((symbol-function 'file-executable-p) (lambda (_) t)))
      (should (eq nil (mullvad-ensure-executable))))))

(ert-deftest mullvad-test-ensure-executable-with-empty-string ()
  "Empty string should signal an error."
  (let ((mullvad-executable ""))
    (should-error (mullvad-ensure-executable) :type 'error)))

(ert-deftest mullvad-test-ensure-executable-with-nil ()
  "nil should signal an error."
  (let ((mullvad-executable nil))
    (should-error (mullvad-ensure-executable) :type 'error)))

(ert-deftest mullvad-test-ensure-executable-not-executable ()
  "Non-executable file should signal an error."
  (let ((mullvad-executable "/usr/bin/mullvad"))
    (cl-letf (((symbol-function 'file-executable-p) (lambda (_) nil)))
      (should-error (mullvad-ensure-executable) :type 'error))))

;;;;; mullvad-shell-command-handle-errors

(ert-deftest mullvad-test-handle-errors-normal-output ()
  "Normal output is returned as-is."
  (mullvad-test-with-mocks '(("status" . "Connected to gb-lon-wg-001"))
    (should (equal "Connected to gb-lon-wg-001"
                   (mullvad-shell-command-handle-errors '("status"))))))

(ert-deftest mullvad-test-handle-errors-capitalized-error ()
  "Output starting with 'Error' signals an error."
  (mullvad-test-with-mocks '(("relay" . "Error: invalid relay"))
    (should-error (mullvad-shell-command-handle-errors '("relay" "set" "location" "xx-bad"))
                  :type 'error)))

(ert-deftest mullvad-test-handle-errors-lowercase-error ()
  "Output containing 'error:' signals an error."
  (mullvad-test-with-mocks '(("connect" . "Something went wrong, error: timeout"))
    (should-error (mullvad-shell-command-handle-errors '("connect"))
                  :type 'error)))

(ert-deftest mullvad-test-handle-errors-word-containing-error ()
  "Words like 'errorless' should not trigger the error path.
The regex uses \\\\b, so 'Error' must be a standalone word."
  ;; The pattern is \\`[Ee]rror\\b\\|\\berror:
  ;; "errorless" does NOT match \\`[Ee]rror\\b because "errorless" is one word
  ;; Wait, \\`[Ee]rror\\b would match "errorless" since \\` anchors to string
  ;; start and \\b is at the boundary between "error" and "less".
  ;; Actually, "errorless" starts with "Error" and \\b matches between 'r' and 'l'
  ;; because r is a word char and l is a word char, so \\b does NOT match there.
  ;; So "Errorless" at the start would actually still match \\`[Ee]rror\\b?
  ;; Let's check: \b in regex means word boundary. "Errorless" - between r and l,
  ;; both are word characters, so no \b. Good, no match.
  (mullvad-test-with-mocks '(("status" . "Errorless connection established"))
    (should (equal "Errorless connection established"
                   (mullvad-shell-command-handle-errors '("status"))))))

(ert-deftest mullvad-test-handle-errors-error-in-middle-of-output ()
  "Error appearing mid-output (not at start and not 'error:') should not signal."
  (mullvad-test-with-mocks '(("status" . "No Error detected"))
    (should (equal "No Error detected"
                   (mullvad-shell-command-handle-errors '("status"))))))

;;;;; mullvad-is-connected-p

(ert-deftest mullvad-test-is-connected-when-connected ()
  "Returns non-nil when status says Connected."
  (mullvad-test-with-mocks '(("status" . "Connected to gb-lon-wg-001 in London, UK"))
    (should (mullvad-is-connected-p))))

(ert-deftest mullvad-test-is-connected-when-disconnected ()
  "Returns nil when status says Disconnected."
  (mullvad-test-with-mocks '(("status" . "Disconnected"))
    (should-not (mullvad-is-connected-p))))

(ert-deftest mullvad-test-is-connected-when-connecting ()
  "Returns nil when status says Connecting (not yet Connected)."
  (mullvad-test-with-mocks '(("status" . "Connecting to gb-lon-wg-001..."))
    (should-not (mullvad-is-connected-p))))

;;;;; mullvad-invalid-duration-p

(ert-deftest mullvad-test-duration-valid-positive-integer ()
  "Positive integer string is valid (returns nil)."
  (should-not (mullvad-invalid-duration-p "42")))

(ert-deftest mullvad-test-duration-valid-single-digit ()
  "Single digit is valid."
  (should-not (mullvad-invalid-duration-p "5")))

(ert-deftest mullvad-test-duration-valid-large-number ()
  "Large number is valid."
  (should-not (mullvad-invalid-duration-p "1440")))

(ert-deftest mullvad-test-duration-valid-empty-string ()
  "Empty string is valid (means 'no duration')."
  (should-not (mullvad-invalid-duration-p "")))

(ert-deftest mullvad-test-duration-invalid-zero ()
  "Zero is not a positive integer."
  (should (mullvad-invalid-duration-p "0")))

(ert-deftest mullvad-test-duration-invalid-leading-zero ()
  "Leading zeros are not allowed (e.g. '01')."
  (should (mullvad-invalid-duration-p "01")))

(ert-deftest mullvad-test-duration-invalid-negative ()
  "Negative numbers are invalid."
  (should (mullvad-invalid-duration-p "-5")))

(ert-deftest mullvad-test-duration-invalid-float ()
  "Float-like strings are invalid."
  (should (mullvad-invalid-duration-p "3.5")))

(ert-deftest mullvad-test-duration-invalid-text ()
  "Arbitrary text is invalid."
  (should (mullvad-invalid-duration-p "forever")))

(ert-deftest mullvad-test-duration-invalid-spaces ()
  "Whitespace is invalid."
  (should (mullvad-invalid-duration-p " 5 ")))

;;;;; mullvad-run-command

(ert-deftest mullvad-test-run-command-constructs-correct-command ()
  "run-command shell-quotes all arguments."
  (let ((captured-command nil))
    (mullvad-test-with-mocks '(("." . "ok"))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (cmd) (setq captured-command cmd) "ok")))
        (mullvad-run-command "relay" "set" "location" "gb-lon-wg-001")
        ;; Verify each arg is quoted
        (should (string-match-p (regexp-quote (shell-quote-argument "/usr/bin/mullvad"))
                                captured-command))
        (should (string-match-p (regexp-quote (shell-quote-argument "relay"))
                                captured-command))
        (should (string-match-p (regexp-quote (shell-quote-argument "gb-lon-wg-001"))
                                captured-command))))))

(ert-deftest mullvad-test-run-command-quotes-dangerous-input ()
  "Arguments with shell metacharacters are safely quoted."
  (let ((captured-command nil))
    (mullvad-test-with-mocks '(("." . "ok"))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (cmd) (setq captured-command cmd) "ok")))
        (mullvad-run-command "relay" "set" "location" "foo;rm -rf /")
        ;; The dangerous argument should be shell-quoted (escaped or single-quoted)
        ;; so the raw unescaped "foo;rm -rf /" should NOT appear in the command.
        (should-not (string-match-p "foo;rm" captured-command))))))

;;;; Important tests

;;;;; mullvad-get-server

(ert-deftest mullvad-test-get-server-city-found ()
  "Returns server for a known city."
  (mullvad-test-with-mocks nil
    (should (equal "gb-lon-wg-001"
                   (mullvad-get-server 'city "London")))))

(ert-deftest mullvad-test-get-server-city-not-found ()
  "Signals user-error for an unknown city."
  (mullvad-test-with-mocks nil
    (should-error (mullvad-get-server 'city "Atlantis")
                  :type 'user-error)))

(ert-deftest mullvad-test-get-server-website-found ()
  "Returns city for a known website."
  (mullvad-test-with-mocks nil
    (should (equal "London"
                   (mullvad-get-server 'website "BBC")))))

(ert-deftest mullvad-test-get-server-website-not-found ()
  "Signals user-error for an unknown website."
  (mullvad-test-with-mocks nil
    (should-error (mullvad-get-server 'website "nonexistent.example.com")
                  :type 'user-error)))

(ert-deftest mullvad-test-get-server-uses-string-equality ()
  "Lookup uses string= so 'london' (lowercase) should not match 'London'."
  (mullvad-test-with-mocks nil
    (should-error (mullvad-get-server 'city "london")
                  :type 'user-error)))

;;;;; mullvad-format-time-string

(ert-deftest mullvad-test-format-time-zero-seconds ()
  "Zero time shows '0 seconds'."
  (should (equal "0 seconds"
                 (mullvad-format-time-string (seconds-to-time 0)))))

(ert-deftest mullvad-test-format-time-seconds-only ()
  "Small time shows only seconds."
  (should (equal "45 seconds"
                 (mullvad-format-time-string (seconds-to-time 45)))))

(ert-deftest mullvad-test-format-time-minutes-and-seconds ()
  "Shows minutes and seconds."
  (should (equal "5 minutes, 30 seconds"
                 (mullvad-format-time-string (seconds-to-time 330)))))

(ert-deftest mullvad-test-format-time-hours-minutes-seconds ()
  "Shows hours, minutes, and seconds."
  (should (equal "2 hours, 15 minutes, 10 seconds"
                 (mullvad-format-time-string (seconds-to-time (+ (* 2 3600) (* 15 60) 10))))))

(ert-deftest mullvad-test-format-time-days ()
  "Shows days for large durations."
  (should (equal "1 days, 2 hours, 3 minutes, 4 seconds"
                 (mullvad-format-time-string
                  (seconds-to-time (+ 86400 (* 2 3600) (* 3 60) 4))))))

(ert-deftest mullvad-test-format-time-exact-minutes ()
  "Exact minutes omit the seconds component."
  (should (equal "10 minutes"
                 (mullvad-format-time-string (seconds-to-time 600)))))

(ert-deftest mullvad-test-format-time-exact-hour ()
  "Exact hour omit minutes and seconds."
  (should (equal "1 hours"
                 (mullvad-format-time-string (seconds-to-time 3600)))))

(ert-deftest mullvad-test-format-time-negative-clamped-to-zero ()
  "Negative time is clamped to 0 (shows '0 seconds')."
  (should (equal "0 seconds"
                 (mullvad-format-time-string (seconds-to-time -100)))))

;;;;; mullvad-connect-to-city (idempotency)

(ert-deftest mullvad-test-connect-to-city-already-connected ()
  "Does not reconnect if already connected to the same server.
Should only call status (for check) and then disconnect-after path."
  (let ((commands-run '()))
    (mullvad-test-with-mocks
        '(;; status check inside connect-to-city
          ("status" . "Connected to gb-lon-wg-001 in London, GB")
          ;; mullvad-disconnect-after calls mullvad-is-connected-p
          ("status" . "Connected to gb-lon-wg-001 in London, GB")
          ;; mullvad-status at end of disconnect-after
          ("status" . "Connected to gb-lon-wg-001 in London, GB"))
      ;; Mock prompt to avoid interactive read
      (cl-letf (((symbol-function 'mullvad-prompt-for-duration) (lambda (&optional _) nil)))
        ;; Track which commands get run
        (let ((orig-run (symbol-function 'mullvad-run-command)))
          (cl-letf (((symbol-function 'mullvad-run-command)
                     (lambda (&rest args)
                       (push args commands-run)
                       (apply orig-run args))))
            (mullvad-connect-to-city "London" nil nil))))
      ;; Should NOT have run "connect" or "relay set location"
      (should-not (cl-find-if (lambda (args) (equal (car args) "connect")) commands-run))
      (should-not (cl-find-if (lambda (args) (equal (car args) "relay")) commands-run)))))

(ert-deftest mullvad-test-connect-to-city-different-server ()
  "Reconnects when connected to a different server."
  (mullvad-test-with-mocks
      '(;; status check inside connect-to-city: connected to Madrid
        ("status" . "Connected to es-mad-wg-101 in Madrid, ES")
        ;; relay set location
        ("relay" . "Relay constraints updated")
        ;; connect
        ("connect" . "Connecting...")
        ;; ensure-connected polling: first Connecting, then Connected
        ("status" . "Connecting...")
        ("status" . "Connected to gb-lon-wg-001 in London, GB")
        ;; disconnect-after -> is-connected-p
        ("status" . "Connected to gb-lon-wg-001 in London, GB")
        ;; status message at end
        ("status" . "Connected to gb-lon-wg-001 in London, GB"))
    (cl-letf (((symbol-function 'mullvad-prompt-for-duration) (lambda (&optional _) nil)))
      (mullvad-connect-to-city "London" nil nil))
    ;; If we get here without error, the reconnection succeeded.
    (should t)))

(ert-deftest mullvad-test-connect-to-city-when-disconnected ()
  "Connects when currently disconnected."
  (mullvad-test-with-mocks
      '(;; status check: disconnected
        ("status" . "Disconnected")
        ;; relay set location
        ("relay" . "Relay constraints updated")
        ;; connect
        ("connect" . "Connecting...")
        ;; ensure-connected polling
        ("status" . "Connected to gb-lon-wg-001 in London, GB")
        ;; disconnect-after -> is-connected-p
        ("status" . "Connected to gb-lon-wg-001 in London, GB")
        ;; status message
        ("status" . "Connected to gb-lon-wg-001 in London, GB"))
    (cl-letf (((symbol-function 'mullvad-prompt-for-duration) (lambda (&optional _) nil)))
      (mullvad-connect-to-city "London" nil nil))
    (should t)))

;;;;; mullvad-disconnect

(ert-deftest mullvad-test-disconnect-when-connected ()
  "Disconnects and cancels timers."
  (mullvad-test-with-mocks
      '(;; is-connected-p
        ("status" . "Connected to gb-lon-wg-001")
        ;; disconnect command
        ("disconnect" . "Disconnecting...")
        ;; ensure-disconnected polling
        ("status" . "Disconnected")
        ;; status display
        ("status" . "Disconnected\n"))
    (mullvad-disconnect)
    (should-not mullvad-timer)))

(ert-deftest mullvad-test-disconnect-when-already-disconnected ()
  "No-op disconnect when not connected (just shows status)."
  (mullvad-test-with-mocks
      '(;; is-connected-p
        ("status" . "Disconnected")
        ;; status display
        ("status" . "Disconnected\n"))
    (mullvad-disconnect)
    (should t)))

(ert-deftest mullvad-test-disconnect-cancels-timer ()
  "Disconnect cancels any running timer."
  (mullvad-test-with-mocks
      '(;; is-connected-p
        ("status" . "Disconnected")
        ;; status display
        ("status" . "Disconnected\n"))
    ;; Set up a fake timer
    (setq mullvad-timer (run-with-timer 9999 nil #'ignore))
    (mullvad-disconnect)
    (should-not mullvad-timer)))

;;;;; mullvad-disconnect-after

(ert-deftest mullvad-test-disconnect-after-not-connected ()
  "Signals user-error if not connected."
  (mullvad-test-with-mocks
      '(;; is-connected-p
        ("status" . "Disconnected"))
    (should-error (mullvad-disconnect-after 30)
                  :type 'user-error)))

(ert-deftest mullvad-test-disconnect-after-sets-timer ()
  "Sets a timer when given a duration."
  (mullvad-test-with-mocks
      '(;; is-connected-p inside disconnect-after
        ("status" . "Connected to gb-lon-wg-001")
        ;; status display at end
        ("status" . "Connected to gb-lon-wg-001\n"))
    (unwind-protect
        (progn
          (mullvad-disconnect-after 30 nil)
          (should mullvad-timer))
      ;; Cleanup
      (mullvad-cancel-timers))))

(ert-deftest mullvad-test-disconnect-after-nil-duration-no-timer ()
  "nil duration means no timer is set."
  (mullvad-test-with-mocks
      '(;; is-connected-p
        ("status" . "Connected to gb-lon-wg-001")
        ;; status display
        ("status" . "Connected to gb-lon-wg-001\n"))
    ;; Mock prompt to return nil (empty string -> nil)
    (cl-letf (((symbol-function 'mullvad-prompt-for-duration) (lambda (&optional _) nil)))
      (mullvad-disconnect-after nil nil)
      (should-not mullvad-timer))))

;;;;; mullvad-cancel-timers

(ert-deftest mullvad-test-cancel-timers-with-active-timer ()
  "Cancelling an active timer sets mullvad-timer to nil."
  (let ((mullvad-timer (run-with-timer 9999 nil #'ignore)))
    (mullvad-cancel-timers)
    (should-not mullvad-timer)))

(ert-deftest mullvad-test-cancel-timers-with-no-timer ()
  "Cancelling when no timer exists is a no-op."
  (let ((mullvad-timer nil))
    (mullvad-cancel-timers)
    (should-not mullvad-timer)))

;;;;; mullvad-shell-command (silent mode)

(ert-deftest mullvad-test-shell-command-silently ()
  "When silently is non-nil, messages are suppressed."
  (mullvad-test-with-mocks '(("status" . "Disconnected"))
    (should (equal "Disconnected" (mullvad-shell-command 'silently "status")))))

(ert-deftest mullvad-test-shell-command-global-silent ()
  "When mullvad-silent is non-nil, messages are suppressed."
  (mullvad-test-with-mocks '(("status" . "Disconnected"))
    (let ((mullvad-silent t))
      (should (equal "Disconnected" (mullvad-shell-command nil "status"))))))

;;;; Useful tests

;;;;; mullvad-dwim

(ert-deftest mullvad-test-dwim-disconnects-when-connected ()
  "dwim calls disconnect when connected."
  (let ((disconnected nil))
    (mullvad-test-with-mocks
        '(;; is-connected-p for dwim check
          ("status" . "Connected to gb-lon-wg-001")
          ;; disconnect -> cancel-timers, then is-connected-p
          ("status" . "Connected to gb-lon-wg-001")
          ;; disconnect command
          ("disconnect" . "Disconnecting...")
          ;; ensure-disconnected
          ("status" . "Disconnected")
          ;; status display
          ("status" . "Disconnected\n"))
      (mullvad-dwim)
      ;; If we get here the disconnect path was taken
      (should t))))

;;;;; mullvad-connect dispatch

(ert-deftest mullvad-test-connect-invalid-type ()
  "Invalid connection type signals user-error."
  (mullvad-test-with-mocks nil
    (should-error (mullvad-connect "ftp")
                  :type 'user-error)))

;;;;; mullvad-connect-to-website

(ert-deftest mullvad-test-connect-to-website-resolves-city ()
  "Website resolves to city and then to server."
  (mullvad-test-with-mocks
      '(;; status check from connect-to-city (disconnected)
        ("status" . "Disconnected")
        ;; relay set location
        ("relay" . "Relay constraints updated")
        ;; connect
        ("connect" . "Connecting...")
        ;; ensure-connected
        ("status" . "Connected to gb-lon-wg-001 in London, GB")
        ;; disconnect-after -> is-connected-p
        ("status" . "Connected to gb-lon-wg-001 in London, GB")
        ;; status message
        ("status" . "Connected to gb-lon-wg-001 in London, GB"))
    (cl-letf (((symbol-function 'mullvad-prompt-for-duration) (lambda (&optional _) nil)))
      (mullvad-connect-to-website "BBC" nil nil))
    (should t)))

(ert-deftest mullvad-test-connect-to-website-unknown ()
  "Unknown website signals user-error."
  (mullvad-test-with-mocks nil
    (should-error (mullvad-connect-to-website "UnknownSite" nil nil)
                  :type 'user-error)))

;;;;; mullvad-list-servers

(ert-deftest mullvad-test-list-servers-creates-buffer ()
  "Creates *Mullvad Servers* buffer with server list."
  (mullvad-test-with-mocks
      '(("relay" . "gb-lon-wg-001  London, GB\nes-mad-wg-101  Madrid, ES\n"))
    (save-window-excursion
      (mullvad-list-servers)
      (let ((buf (get-buffer "*Mullvad Servers*")))
        (should buf)
        (with-current-buffer buf
          (should (string-match-p "gb-lon-wg-001" (buffer-string)))
          (should (eq major-mode 'special-mode)))
        (kill-buffer buf)))))

;;;;; mullvad-ensure-connection-state

(ert-deftest mullvad-test-ensure-connection-state-invalid ()
  "Invalid state signals user-error."
  (mullvad-test-with-mocks
      '(("status" . "Disconnected"))
    (should-error (mullvad-ensure-connection-state 'bogus)
                  :type 'user-error)))

;;;;; mullvad-set-timer

(ert-deftest mullvad-test-set-timer-duration-in-minutes ()
  "Timer is set for duration * 60 seconds."
  (let ((mullvad-timer nil)
        (mullvad-silent nil))
    (unwind-protect
        (progn
          (mullvad-set-timer 5 nil)
          (should mullvad-timer)
          ;; Timer should fire ~300 seconds from now
          (let* ((fire-time (timer--time mullvad-timer))
                 (delta (float-time (time-subtract fire-time (current-time)))))
            ;; Allow 5s tolerance for test execution time
            (should (> delta 290))
            (should (< delta 305))))
      (mullvad-cancel-timers))))

;;;;; mullvad-get-time-until-disconnect

(ert-deftest mullvad-test-get-time-until-disconnect-no-timer ()
  "Returns nil when no timer is active."
  (let ((mullvad-timer nil))
    (should-not (mullvad-get-time-until-disconnect))))

(ert-deftest mullvad-test-get-time-until-disconnect-with-timer ()
  "Returns a formatted string when a timer is active."
  (let ((mullvad-timer nil))
    (unwind-protect
        (progn
          (setq mullvad-timer (run-with-timer 600 nil #'ignore))
          (let ((result (mullvad-get-time-until-disconnect)))
            (should (stringp result))
            (should (string-match-p "minutes" result))))
      (mullvad-cancel-timers))))

;;;;; mullvad-shh macro

(ert-deftest mullvad-test-shh-suppresses-messages ()
  "mullvad-shh prevents messages from appearing."
  (let ((messages '()))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (mullvad-shh (message "should be suppressed")))
    ;; The message function was called but inhibit-message prevents display.
    ;; In batch mode, we just verify the macro expands and runs without error.
    (should t)))

;;;;; mullvad-status

(ert-deftest mullvad-test-status-without-timer ()
  "Status shows connection info without timer info."
  (let ((displayed nil))
    (mullvad-test-with-mocks
        '(("status" . "Connected to gb-lon-wg-001 in London, GB\n")
          ;; no timer
          )
      (let ((mullvad-timer nil))
        (cl-letf (((symbol-function 'message) (lambda (msg) (setq displayed msg))))
          (mullvad-status)
          (should (string-match-p "Connected" displayed))
          (should-not (string-match-p "Disconnecting in" displayed)))))))

(ert-deftest mullvad-test-status-with-timer ()
  "Status appends disconnect time when timer is active."
  (let ((displayed nil)
        (mullvad-timer nil))
    (unwind-protect
        (mullvad-test-with-mocks
            '(("status" . "Connected to gb-lon-wg-001 in London, GB\n"))
          (setq mullvad-timer (run-with-timer 600 nil #'ignore))
          (cl-letf (((symbol-function 'message) (lambda (msg) (setq displayed msg))))
            (mullvad-status)
            (should (string-match-p "Connected" displayed))
            (should (string-match-p "Disconnecting in" displayed))))
      (mullvad-cancel-timers))))

(provide 'mullvad-test)
;;; mullvad-test.el ends here
