;;; mullvad.el --- Convenience functions for Mullvad -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1

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

(defcustom mullvad-executable "mullvad"
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

;;;; Functions

;;;;; General

(defmacro mullvad-shell-command (command)
  "Execute a `mullvad' shell COMMAND."
  `(progn
     (mullvad-check-executable-exists)
     (shell-command-to-string ,command)))

(defun mullvad-check-executable-exists ()
  "Check that the `mullvad' executable is present."
  (unless (executable-find mullvad-executable)
    (user-error "Mullvad not found. Please make sure it is installed and set `mullvad-executable' accordingly")))

;;;###autoload
(defun mullvad-status ()
  "Get status of connection."
  (interactive)
  (message (concat (string-trim
		    (mullvad-shell-command (format "%s status" mullvad-executable)))
		   (when-let ((time (mullvad-get-time-until-disconnect)))
		     (concat ". Disconnecting in " time))
		   ".")))

(defun mullvad-dwim ()
  "Connect if disconnected, and vice versa."
  (interactive)
  (if (mullvad-is-connected-p)
      (mullvad-disconnect)
    (call-interactively #'mullvad-connect)))

;;;;; Connect

;;;###autoload
(defun mullvad-connect (connection)
  "Connect to a Mullvad server, prompting the user for a CONNECTION type."
  (interactive
   (list
    (progn
      (mullvad-check-executable-exists)
      (completing-read "Select connection: " '(city website)))))
  (pcase connection
    ("city" (call-interactively #'mullvad-connect-to-city))
    ("website" (call-interactively #'mullvad-connect-to-website))))

(defun mullvad-connect-to-city (&optional city duration)
  "Connect to server associated with CITY for DURATION.
If CITY or DURATION are nil, prompt the user accordingly.

The association between cities and servers is defined in
`mullvad-cities-and-servers'."
  (interactive)
  (let ((server (mullvad-connect-to-city-or-website 'city city)))
    (mullvad-shell-command (format "%s relay set location %s; mullvad connect"
				   mullvad-executable server))
    (if duration
	(mullvad-disconnect-after duration)
      (call-interactively #'mullvad-disconnect-after))
    (mullvad-status)))

(defun mullvad-connect-to-website (&optional website duration)
  "Connect to server associated with WEBSITE for DURATION.
Prompt the user to select from a list of websites and connection durations, and
connect to the corresponding server for that duration.

The association between websites and cities is defined in
`mullvad-websites-and-cities'."
  (interactive)
  (let ((city (mullvad-connect-to-city-or-website 'website website)))
    (mullvad-connect-to-city city duration)))

(defun mullvad-connect-to-city-or-website (connection &optional selection)
  "Connect to a Mullvad server using CONNECTION type.
Prompt the user for a SELECTION if necessary. Disconnect if already connected."
  (mullvad-disconnect 'no-status)
  (let* ((var (pcase connection
		('city mullvad-cities-and-servers)
		('website mullvad-websites-and-cities)))
	 (selection (or selection (completing-read "Select: " var))))
    (alist-get selection var nil nil #'string=)))

;;;;; Disconnect

(defun mullvad-disconnect (&optional no-status)
  "Disconnect from the server if currently connected.
Cancel any running timers. If NO-STATUS is non-nil, do not diplay the Mullvad
status."
  (interactive)
  (mullvad-check-executable-exists)
  (mullvad-cancel-timers)
  (when (mullvad-is-connected-p)
    (mullvad-shell-command (format "%s disconnect" mullvad-executable))
    (while (string-match-p "Disconnecting..." (mullvad-status))
      (sleep-for 0.1))
    (unless no-status
      (mullvad-status))))

(defun mullvad-disconnect-after (duration)
  "Disconnect from server after DURATION, in minutes."
  (interactive
   (list (completing-read
	  "Select duration (minutes): "
	  '("1" "5" "10" "30" "60" "120" "∞" "custom"))))
  (when (string= duration "custom")
    (setq duration (read-string "Select duration (minutes): ")))
  (cancel-function-timers #'mullvad-disconnect)
  (unless (equal duration "∞")
    (run-with-timer
     (* (string-to-number duration) 60) nil #'mullvad-disconnect)))

(defun mullvad-is-connected-p ()
  "Return t iff connected to server."
  (let ((inhibit-message t))
    (null (string-match-p "Disconnected" (mullvad-status)))))

;;;;; Timers

(defun mullvad-get-mullvad-timers ()
  "Get the time remaining for the timer that will trigger `mullvad-disconnect'."
  (let ((timers (cl-remove-if-not
		 (lambda (timer)
		   (eq (timer--function timer) 'mullvad-disconnect))
		 timer-list)))
    timers))

(defun mullvad-get-time-until-disconnect ()
  "Get remaining time in timer for `mullvad-disconnect'.
If more than one timer found, signal an error."
  (when-let ((timers (mullvad-get-mullvad-timers)))
    (if (< (length timers) 2)
	(let* ((timer (car timers))
	       (time (timer--time timer)))
	  (mullvad-format-time-string (time-subtract time (current-time))))
      (user-error "Multiple Mullvad timers found"))))

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

(defun mullvad-cancel-timers ()
  "Cancel any Mullvad running timers."
  (when (mullvad-get-time-until-disconnect)
    (cancel-function-timers 'mullvad-disconnect)))

;;;;; Dispatcher

(transient-define-prefix mullvad-dispatch ()
  "Dispatch a `mullvad' command."
  [["connect"
    ("c" "to city"            mullvad-connect-to-city)
    ("w" "to website"         mullvad-connect-to-website)
    ]
   ["disconnect"
    ("n" "now"                mullvad-disconnect)
    ("l" "later"              mullvad-disconnect-after)
    ]
   [""
    ("d" "dwim"               mullvad-dwim)
    ("s" "status"             mullvad-status)
    ]
   ]
  )

(provide 'mullvad)
;;; mullvad.el ends here
