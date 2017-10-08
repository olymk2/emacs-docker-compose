;;; dc-docker.el --- Take control of your docker containers -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Oliver Marks

;; Author: Oliver Marks <oly@digitaloctave.com>
;; URL: https://github.com/olymk2/emacs-docker
;; Keywords: Docker control magit popups tests
;; Version: 0.1
;; Created 13 October 2017
;; Package-Requires: ((magit "2.5")(helm "2.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implid warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides a set of magit style popups for interacting with your containers.
;; It wraps docker and docker-compose commands and allows you to select containers and toggle
;; the various paramters passed to the commands.

;; It can be extended to run tests inside containers and comes with some predefined setups, it
;; should also be easy to add in your own common commands to the interactive popups

;;; Code:

(require 'cl)


(defun dc-docker-process (command &rest params)
  (interactive)
  (let ((commands (append (list dc-buffer-name dc-docker-cmd command) params)))
  (message "dc-docker-process %s" commands)
  (apply 'dc-process commands)))

;;wrapper for docker shell commands backgrounded
(defun dc-docker-run (name command params)
  "Wrapper for docker commands, takes container name command params and background
`name'       -- Container name
`command'    -- Docker command
`params'     -- Extra params
`background' -- pass \"&\" for background"
  (dc-process command name params))

;;wrapper for docker shell command but return as string not backgrounded
(defun dc-docker-run-return (name command &rest params)
  (message "dc-docker-run-return%s" (concat dc-docker-cmd " " command " " name " " (mapconcat 'identity params " ")))
  (let ((default-directory (dc-compose-root)))
  (shell-command-to-string
   (concat dc-docker-cmd " " command " " name " " (mapconcat 'identity params " ")))))

(defun dc-docker-shell (command &rest params)
  (interactive)
  (apply 'dc-process (append (list dc-buffer-shell-name dc-docker-cmd command) params))) 


(defun dc-docker-build (tagname)
  "Starts a docker build, will prompt for a tag name
  `tagname' -- Name to tag the build with"
  (interactive (list (read-string "Tag name:")))
  (dc-dockerfile-exists-check)
  (dc-docker-process "build" "-t" tagname "."))

(defun dc-docker-logs (name &rest flags)
  "Runs docker logs against a container"
  (interactive (list (read-string "Container name:")))
  (unless flags (setq flags (list)))
  (apply 'dc-docker-process (append (list "logs" name) flags)))

;; Docker IP Addresses
(defun dc-docker-network-test ()
  (interactive)
  (dc-docker-run "gogs" dc-str-addresses ""))

;; Docker IP Addresses
(defun dc-docker-network ()
  (with-current-buffer dc-buffer 
  (loop for name in (dc-docker-names) collect
    (dc-docker-run-return name dc-str-addresses "" ""))))

;; run a command on a docker container
(defun dc-docker-exec (name &rest command)
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (unless command (setq command (read-string "Shell Command:")))
  (message "dc-docker-exec %s %s" name command)
  (apply 'dc-docker-shell (append (list "exec" "-it" name) command)))

;; run a command on a docker container
(defun dc-docker-pull (name &rest command)
  (interactive (list (read-string "Image name:")))
  (message "dc-docker-pull %s %s" name command)
  (apply 'dc-docker-shell (append (list "pull" name) command)))

;; return list of docker names
(defun dc-docker-names ()
  (interactive)
  (let ((container_ids (split-string (dc-docker-run-return "" "ps" "-q" "" "") "\n" t)))
    (message "dc-docker-names %s" container_ids)
    (message "dc-docker-names %s" (car container_ids))
    (message "dc-docker-names %s" (substring (car container_ids) 1 -1))
    (loop for el in container_ids collect
      (substring (dc-docker-run-return el "inspect" "-f\"{{ .Name }}\"" "") 1 -1))))


;; return list of docker names
(defun dc-docker-ps (&rest params)
  (interactive)
  (message "dc-docker-ps %s" params)
  (apply 'dc-docker-shell (append (list "ps") params)))


(provide 'dc-docker)
;;; dc-docker.el ends here
