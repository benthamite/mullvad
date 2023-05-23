;;; mullvad.el --- Convenience functions for the VPN program Mullvad -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1
;; Keywords: convenience


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

;; commentary

;; This package collects a few functions for interfacing with Mullvad, a
;; VPN service. To use these functions, you must first run `mullvad
;; account login [your account number]' in the terminal. To find your
;; account number, go to "settings" > "account".

;;; Code:

(defvar mullvad-cities-and-servers
  '(("London" . "gb-lon-wg-001")
    ("Madrid" . "es-mad-wg-101")
    ("Malmö" . "se-sto-wg-001")
    ("Frankfurt" . "de-fra-wg-001")
    ("New York" . "us-nyc-wg-601")
    ("São Paulo" . "br-sao-wg-001"))
  "Association list of cities and optimal servers.")

(defvar mullvad-websites-and-cities
  '(("Library Genesis" . "Malmö")
    ("HathiTrust" . "New York")
    ("Criterion Channel" . "New York")
    ("Pirate Bay" . "Malmö")
    ("Wise" . "Madrid"))
  "Association list of websites and optimal server cities.")

(defun mullvad-connect-to-city (city)
  "Connect to server associated with CITY.
Prompt the user to select from a list of cities and connection
durations, and connect to the corresponding server for that
duration.

The association between cities and servers is defined in
`mullvad-servers'."
  (interactive
   (list
    (completing-read
     "Select server: "
     mullvad-cities-and-servers)))
  (let* ((duration (call-interactively 'mullvad-disconnect-after))
	 (server (alist-get city mullvad-servers nil nil #'string=))
	 (connection (replace-regexp-in-string
		      "Setting location constraint to \\(.*\\)\n.*\n.*" "\\1"
		      (shell-command-to-string (format
						"mullvad relay set hostname %s; mullvad connect"
						server)))))
    (message (concat (format "Connected to Mullvad server `%s'." connection)
		     (when duration (format " Disconnecting in %s minute(s)." duration))))))

(defun mullvad-connect-to-website (website)
  "Connect to server associated with WEBSITE.
Prompt the user to select from a list of websites and connection
durations, and connect to the corresponding server for that
duration.

The association between websites and cities is defined in
`mullvad-websites-and-cities'."
  (interactive
   (list
    (completing-read
     "Select website: "
     mullvad-websites-and-cities)))
  (let ((city (alist-get website mullvad-websites-and-cities nil nil #'string=)))
    (mullvad-connect-to-server city)))

(defun mullvad-disconnect ()
  "Disconnect from server."
  (interactive)
  (shell-command "mullvad disconnect")
  (message "Disconnected from Mullvad server."))

(defun mullvad-disconnect-after (duration)
  "End connection to Mullvad VPN server after DURATION minutes."
  (interactive
   (list (completing-read
	  "Select duration (minutes): "
	  '("1" "5" "10" "30" "60" "120" "custom" "unlimited"))))
  (when (equal duration "custom")
    (setq duration (read-string "Select duration (minutes): ")))
  (unless (equal duration "unlimited")
    ;; If a previous timer is running, cancel it.
    (cancel-function-timers #'mullvad-disconnect)
    ;; Now run a new timer.
    (run-with-timer
     (* (string-to-number duration) 60)
     nil
     #'mullvad-disconnect)
    duration))

(provide 'mullvad)
;;; mullvad.el ends here
