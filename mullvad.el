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

;; To use these functions, you must have `mullvad' installed and have run
;; `mullvad account login [your account number]' in the terminal. To find your
;; account number, go to "settings" > "account" in the graphical interface.

;;; Code:

(require 'transient)

;;;; User options

(defgroup mullvad ()
  "Convenience functions for interfacing with `mullvad'."
  :group 'emacs)

(defcustom mullvad-executable "mullvad"
  "Path to the `mullvad' executable."
  :type 'string
  :group 'mullvad)

(defcustom mullvad-cities-and-servers
  '(("London" . "gb-lon-wg-001")
    ("Madrid" . "es-mad-wg-101")
    ("Malmö" . "se-sto-wg-001")
    ("Frankfurt" . "de-fra-wg-001")
    ("New York" . "us-nyc-wg-601")
    ("São Paulo" . "br-sao-wg-001"))
  "Association list of cities and optimal servers."
  :type 'alist
  :group 'mullvad)

(defcustom mullvad-websites-and-cities
  '(("Betfair" . "London")
    ("Criterion Channel" . "New York")
    ("HathiTrust" . "New York")
    ("Library Genesis" . "Malmö")
    ("Pirate Bay" . "Malmö")
    ("Wise" . "Madrid"))
  "Association list of websites and optimal server cities."
  :type 'alist
  :group 'mullvad)

;;;; Functions

;;;###autoload
(defun mullvad-dwim ()
  "Connect if disconnected, and vice versa."
  (interactive)
  (if (mullvad-is-disconnected-p)
      (call-interactively #'mullvad-connect)
    (mullvad-disconnect)))

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

(defun mullvad-check-executable-exists ()
  "Check that the `mullvad' executable is present in the system."
  (unless (executable-find mullvad-executable)
    (user-error "Mullvad not found, please install it first")))

(defun mullvad-connect-to-city (&optional city)
  "Connect to server associated with CITY for a certain duration.
Prompt the user to select from a list of cities and connection
duration, and connect to the corresponding server for that
duration.

The association between cities and servers is defined in
`mullvad-cities-and-servers'."
  (interactive)
  (let ((server (mullvad-connect-to-city-or-website 'city city)))
    (shell-command (format "%s relay set hostname %s; mullvad connect"
			   mullvad-executable server))
    (call-interactively #'mullvad-disconnect-after)
    (mullvad-status)))

(defun mullvad-connect-to-website (&optional website)
  "Connect to server associated with WEBSITE for DURATION.
Prompt the user to select from a list of websites and connection
duration, and connect to the corresponding server for that
duration.

The association between websites and cities is defined in
`mullvad-websites-and-cities'."
  (interactive)
  (let ((city (mullvad-connect-to-city-or-website 'website website)))
    (mullvad-connect-to-city city)))

(defun mullvad-connect-to-city-or-website (connection &optional selection)
  "Connect to a Mullvad server using CONNECTION type.
Prompt the user for a SELECTION if necessary. Disconnect if already connected."
  (mullvad-disconnect)
  (let* ((var (pcase connection
		('city mullvad-cities-and-servers)
		('website mullvad-websites-and-cities)))
	 (selection (or selection (completing-read "Select: " var))))
    (alist-get selection var nil nil #'string=)))

;; TODO: develop this
(defun mullvad-async-shell-command (command on-finish)
  "Execute shell COMMAND asynchronously in the background.

ON-FINISH is the callback function called with the result
when the process sentinels."

  ;; Start the process
  (let ((process (start-process-shell-command "mullvad-process" nil command)))
    
    ;; Sentinels is Emacs' way of handling events from subprocesses
    (set-process-sentinel process
                          (lambda (process signal)

                            ;; If the process has exited
                            (when (memq (process-status process) '(exit signal))

                              ;; Call the callback function
                              (funcall on-finish (process-exit-status process)))))))

(defun mullvad-disconnect ()
  "Disconnect from server if currently connected."
  (interactive)
  (unless (mullvad-is-disconnected-p)
    (shell-command (format "%s disconnect" mullvad-executable))
    (mullvad-status)))

(defun mullvad-disconnect-after (duration)
  "End connection to Mullvad VPN server after DURATION minutes."
  (interactive
   (list (completing-read
	  "Select duration (minutes): "
	  '("1" "5" "10" "30" "60" "120" "custom" "unlimited"))))
  (when (string= duration "custom")
    (setq duration (read-string "Select duration (minutes): ")))
  (cancel-function-timers #'mullvad-disconnect)
  (unless (equal duration "unlimited")
    (run-with-timer
     (* (string-to-number duration) 60)
     nil
     #'mullvad-disconnect)
    duration))

(defun mullvad-status ()
  "Get status of connection."
  (interactive)
  (message (string-trim
	    (shell-command-to-string
	     (format "%s status" mullvad-executable)))))

(defun mullvad-is-disconnected-p ()
  "Return non-nil if disconnected from Mullvad server."
  (string-match-p "Disconnected" (mullvad-status)))

(transient-define-prefix mullvad-dispatch ()
  "Dispatch a `mullvad' command."
  [["Mullvad"
    ("m" "dwim"                          mullvad-dwim)
    ("c" "connect"                       mullvad-connect)
    ("w" "connect to website"            mullvad-connect-to-website)
    ("y" "connect to city"               mullvad-connect-to-city)
    ("d" "disconnect"                    mullvad-disconnect)
    ("a" "disconnect after"              mullvad-disconnect-after)
    ("s" "status"                        mullvad-status)
    ]
   ]
  )

(provide 'mullvad)
;;; mullvad.el ends here
