;;; mullvad.el --- Convenience functions for Mullvad -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.3

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; This package collects a few functions for interfacing with Mullvad, a
;; VPN service.

;;; Code:

(require 'transient)

;;;; User options

(defgroup mullvad ()
  "Convenience functions for interfacing with `mullvad'."
  :group 'emacs)

(defcustom mullvad-executable
  (or (executable-find "mullvad") "")
  "Path to the `mullvad' executable."
  :type 'file
  :group 'mullvad)

(defcustom mullvad-cities-and-servers '()
  "Association list of cities and optimal servers."
  :type '(alist :key-type string :value-type string)
  :group 'mullvad)

(defcustom mullvad-websites-and-cities '()
  "Association list of websites and optimal server cities."
  :type '(alist :key-type string :value-type string)
  :group 'mullvad)

(defcustom mullvad-durations nil
  "List of connection durations, in minutes.
Integers in this list will be presented as selection candidates when prompted to
enter the connection duration. A custom duration may also be entered. If nil,
always enter a duration manually."
  :type '(repeat integer)
  :group 'mullvad)

(defcustom mullvad-silent nil
  "Whether to inhibit `mullvad' messages after connecting or disconnecting."
  :type 'boolean
  :group 'mullvad)

;;;; Variables

(defvar mullvad-timer nil
  "The running `mullvad' timer object, if any.")

;;;; Functions

;;;;; General

;;;###autoload
(defun mullvad-dwim ()
  "Connect if disconnected, and vice versa."
  (interactive)
  (if (mullvad-is-connected-p)
      (mullvad-disconnect)
    (call-interactively #'mullvad-connect)))

(defmacro mullvad-shh (&rest body)
  "Suppress messages in the execution of BODY."
  `(let ((inhibit-message t)
	 (message-log-max nil))
     ,@body))

(defun mullvad-shell-command (command &optional silently)
  "Execute a `mullvad' shell COMMAND and return its output as a string.
If SILENTLY is non-nil, do not return the output."
  (mullvad-ensure-executable)
  (if (or mullvad-silent silently)
      (mullvad-shh
       (mullvad-shell-command-handle-errors command))
    (mullvad-shell-command-handle-errors command)))

(defun mullvad-shell-command-handle-errors (command)
  "Execute a `mullvad' shell COMMAND, handling errors."
  (let ((message (shell-command-to-string command)))
    (if (string-match "error" message)
	(error "Calling command `%s' returned:\n\n%s" command message)
      message)))

(defun mullvad-ensure-executable ()
  "Ensure the Mullvad executable is present, or signal an error."
  (unless (and mullvad-executable (file-executable-p mullvad-executable))
    (error "Mullvad executable not found or is not executable: %s" mullvad-executable)))

;;;###autoload
(defun mullvad-status ()
  "Get the current `mullvad' status."
  (interactive)
  (let ((status (string-trim
		 (mullvad-shell-command (format "%s status" mullvad-executable))))
	(time (mullvad-get-time-until-disconnect)))
    (message (concat status (when time (format ". Disconnecting in %s." time))))))

(defun mullvad-ensure-connection-state (state)
  "Ensure that te `mullvad' is in STATE.
STATE may be either `connected' or `disconnected'."
  (mullvad-shh
   (let ((may-proceed-p (lambda ()
			  (pcase state
			    ('connected (mullvad-is-connected-p))
			    ('disconnected (not (mullvad-is-connected-p)))
			    (_ (user-error "Invalid state: `%s'" state)))))
	 (retries 0))
     (while (and (not (funcall may-proceed-p))
		 (< retries 20))
       (sleep-for 0.1)
       (setq retries (1+ retries))))))

;;;;; Connect

;;;###autoload
(defun mullvad-connect (connection)
  "Connect to a Mullvad server, prompting the user for a CONNECTION type."
  (interactive (list (progn (mullvad-ensure-executable)
			    (completing-read "Select connection: " '(city website)))))
  (pcase connection
    ("city" (call-interactively #'mullvad-connect-to-city))
    ("website" (call-interactively #'mullvad-connect-to-website))))

;;;###autoload
(defun mullvad-connect-to-city (&optional city duration silently)
  "Connect to server associated with CITY for DURATION.
If CITY or DURATION are nil, prompt the user accordingly.

If SILENTLY is non-nil, do not display the Mullvad status.

The association between cities and servers is defined in
`mullvad-cities-and-servers'."
  (interactive)
  (let* ((server (mullvad-get-server 'city city))
	 (command (format "%1$s relay set location %s; %1$s connect"
			  mullvad-executable server)))
    (mullvad-shell-command command 'silently)
    (mullvad-ensure-connected)
    (mullvad-disconnect-after duration (or mullvad-silent silently))))

;;;###autoload
(defun mullvad-connect-to-website (&optional website duration silently)
  "Connect to server associated with WEBSITE for DURATION.
Prompt the user to select from a list of websites and connection durations, and
connect to the corresponding server for that duration.

If SILENTLY is non-nil, do not display the Mullvad status.

The association between websites and cities is defined in
`mullvad-websites-and-cities'."
  (interactive)
  (let ((city (mullvad-get-server 'website website)))
    (mullvad-connect-to-city city duration (or mullvad-silent silently))))

(defun mullvad-get-server (connection &optional selection)
  "Return the Mullvad server associated with CONNECTION and SELECTION.
CONNECTION can be either `city' or `website'. SELECTION can be either a city or
a website. If SELECTION is nil, prompt the user for one"
  (let* ((alist (pcase connection
		  ('city mullvad-cities-and-servers)
		  ('website mullvad-websites-and-cities)))
	 (selection (or selection (completing-read "Select: " alist))))
    (alist-get selection alist nil nil #'string=)))

;;;###autoload
(defun mullvad-list-servers ()
  "List all available `mullvad' servers buffer."
  (interactive)
  (let ((servers (mullvad-shell-command (format "%s relay list" mullvad-executable))))
    (with-current-buffer (get-buffer-create "*Mullvad Servers*")
      (erase-buffer)
      (insert servers)
      (goto-char (point-min)))
    (switch-to-buffer "*Mullvad Servers*")))

(defun mullvad-is-connected-p ()
  "Return t if connected to Mullvad VPN, nil otherwise."
  (let ((output (shell-command-to-string (format "%s status" mullvad-executable))))
    (not (string-match-p "Disconnected" output))))

(defun mullvad-ensure-connected ()
  "Ensure that `mullvad' is connected."
  (mullvad-ensure-connection-state 'connected))

;;;;; Disconnect

(defun mullvad-disconnect (&optional silently)
  "Disconnect from the server if currently connected.
Cancel any running timers. If SILENTLY is non-nil, do not display the Mullvad
status."
  (interactive)
  (mullvad-cancel-timers)
  (when (mullvad-is-connected-p)
    (mullvad-shell-command (format "%s disconnect" mullvad-executable) silently)
    (mullvad-ensure-disconnected))
  (unless (or mullvad-silent silently) (mullvad-status)))

(defun mullvad-ensure-disconnected ()
  "Ensure that `mullvad' is disconnected."
  (mullvad-ensure-connection-state 'disconnected))

(defun mullvad-disconnect-after (&optional duration silently)
  "Disconnect from server after DURATION, in minutes.
If DURATION is nil, prompt the user for one, If SILENTLY is non-nil, do not
display the Mullvad status."
  (interactive)
  (unless (mullvad-is-connected-p)
    (user-error "Not currently connected"))
  (let ((duration (or duration (mullvad-prompt-for-duration))))
    (mullvad-cancel-timers)
    (when duration (mullvad-set-timer duration silently)))
  (unless (or mullvad-silent silently) (mullvad-status)))

(defun mullvad-prompt-for-duration (&optional warn)
  "Prompt the user to select or enter a duration.
If WARN is non-nil, warn the user that the input is invalid."
  (let* ((prompt
	  (if warn
	      "Invalid input. Please enter a positive integer or leave blank: "
	    "Disconnect after how many minutes (leave blank to remain connected indefinitely)? "))
	 (duration (if mullvad-durations
		       (completing-read prompt (mapcar #'number-to-string mullvad-durations))
		     (read-string prompt))))
    (while (mullvad-invalid-duration-p duration)
      (mullvad-prompt-for-duration 'warn))
    (unless (string-empty-p duration)
      (string-to-number duration))))

(defun mullvad-invalid-duration-p (str)
  "Return t iff STR is not a positive integer or an empty string."
  (not (or (string-empty-p str)
	   (< 0 (string-to-number str)))))

;;;;; Timers

(defun mullvad-set-timer (duration silently)
  "Set a timer to disconnect after DURATION.
If SILENTLY is non-nil, do not display a message when disconnecting."
  (setq mullvad-timer
	(run-with-timer
	 (* duration 60) nil
	 (lambda ()
	   (mullvad-disconnect (or mullvad-silent silently))))))

(defun mullvad-cancel-timers ()
  "Cancel any running Mullvad timers."
  (when mullvad-timer
    (cancel-timer mullvad-timer)
    (setq mullvad-timer nil)))

(defun mullvad-get-time-until-disconnect ()
  "Get remaining time in timer for `mullvad-disconnect'.
If more than one timer found, signal an error."
  (when mullvad-timer
    (let ((time (timer--time mullvad-timer)))
      (mullvad-format-time-string (time-subtract time (current-time))))))

(defun mullvad-format-time-string (time)
  "Format TIME to a string in the form `days:hours:minutes:seconds'."
  (let* ((high (car time))
	 (low (cadr time))
	 (total-seconds (+ (* high (expt 2 16)) low))
	 (days (/ total-seconds 86400))
	 (hours (/ (% total-seconds 86400) 3600))
	 (minutes (/ (% total-seconds 3600) 60))
	 (seconds (% total-seconds 60)))
    (concat
     (if (> days 0) (format "%d days, " days) "")
     (if (> hours 0) (format "%d hours, " hours) "")
     (if (> minutes 0) (format "%d minutes, " minutes) "")
     (if (> seconds 0) (format "%d seconds" seconds) ""))))

;;;;; Menu

;; TODO: add silently option
;;;###autoload (autoload 'mullvad-dispatch "mullvad" nil t)
(transient-define-prefix mullvad ()
  "`mullvad' menu."
  [["connect"
    ("c" "to city"            mullvad-connect-to-city)
    ("w" "to website"         mullvad-connect-to-website)]
   ["disconnect"
    ("n" "now"                mullvad-disconnect)
    ("l" "later"              mullvad-disconnect-after)]
   [""
    ("d" "dwim"               mullvad-dwim)
    ("s" "status"             mullvad-status)
    ("r" "servers"            mullvad-list-servers)]])

(make-obsolete 'mullvad-dispatch 'mullvad "0.2")
(make-obsolete 'mullvad-check-executable-exists 'mullvad-ensure-executable "0.3")

(provide 'mullvad)
;;; mullvad.el ends here
