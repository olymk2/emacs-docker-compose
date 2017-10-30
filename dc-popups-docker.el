;;; dc-popups-docker.el --- Take control of your docker containers -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Oliver Marks

;; Author: Oliver Marks <oly@digitaloctave.com>
;; URL: https://github.com/olymk2/emacs-docker
;; Keywords: Processes tools
;; Version: 0.1
;; Created 13 October 2017
;; Package-Requires: ((magit "2.5")(helm "2.5")(emacs "24.3"))

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

(defun dc-popups-docker-select-container()
  "Prompt user to pick container from a list."
  (interactive)
  (setq dc-popups-current-docker-container (completing-read "Select container:" (dc-popups-docker-names))))

(defun dc-popups-docker-process (command &rest params)
  "Core docker wrapper function, to add buffer and cmd path.
`COMMAND'    -- Docker command
`PARAMS'     -- Extra params"
  (interactive)
  (let ((commands (append (list dc-popups-buffer-name dc-popups-docker-cmd command) params)))
  (message "dc-popups-docker-process %s" commands)
  (apply 'dc-popups-core-process commands)))

;;wrapper for docker shell commands backgrounded
(defun dc-popups-docker-run (name command params)
  "Wrapper for docker commands can probably be replaced with dc-popups-docker-process.
`NAME'       -- Container name
`COMMAND'    -- Docker command
`PARAMS'     -- Extra params"
  (dc-popups-core-process command name params))

;;wrapper for docker shell command but return as string not backgrounded
(defun dc-popups-docker-run-return (name command &rest params)
  "Use shell to string and return result instantly blocking.
`NAME'       -- Container name
`COMMAND'    -- Docker command
`PARAMS'     -- Extra params"
  (message "dc-popups-docker-run-return%s" (concat dc-popups-docker-cmd " " command " " name " " (mapconcat 'identity params " ")))
  (let ((default-directory (dc-popups-core-compose-root)))
  (shell-command-to-string
   (concat dc-popups-docker-cmd " " command " " name " " (mapconcat 'identity params " ")))))

;; wrapper command should probably deprecate
(defun dc-popups-docker-shell (command &rest params)
  "Wrapper should probably use dc-process now.
`COMMAND'    -- Docker command
`PARAMS'     -- Extra params"
  (interactive)
  (apply 'dc-popups-core-process (append (list dc-popups-buffer-shell-name dc-popups-docker-cmd command) params)))


(defun dc-popups-docker-build (tagname)
  "Start a docker build, will prompt for a tag name.
`TAGNAME'    -- Name to tag the build with"
  (interactive (list (read-string "Tag name:")))
  (dc-popups-dockerfile-exists-check)
  (dc-popups-docker-process "build" "-t" tagname "."))

(defun dc-popups-docker-logs (&rest flags)
  "Run docker logs against a container.
FLAGS        -- extra build flags"
  (dc-popups-docker-select-container)
  (unless flags (setq flags (list)))
  (apply 'dc-popups-docker-process (append (list "logs" dc-popups-current-docker-container) flags)))


;; Docker IP Addresses
(defun dc-popups-docker-network ()
  "Wrapper command for ps to return the ip addresses."
  (with-current-buffer dc-popups-buffer
  (loop for name in (dc-popups-docker-names) collect
    (dc-popups-docker-run-return name dc-popups-str-addresses "" ""))))

;; run a command on a docker container
(defun dc-popups-docker-exec (name &rest command)
  "Docker exec wrapper.
NAME         -- name of container
COMMAND      -- command to exec inside container"
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (unless command (setq command (read-string "Shell Command:")))
  (message "dc-popups-docker-exec %s %s" name command)
  (apply 'dc-popups-docker-shell (append (list "exec" "-it" name) command)))

;; run a command on a docker container
(defun dc-popups-docker-pull (name &rest params)
  "Docker pull wrapper.
NAME         -- name of image
PARAMS       -- flags to append to pull"
  (interactive (list (read-string "Image name:")))
  (message "dc-popups-docker-pull %s %s" name params)
  (apply 'dc-popups-docker-shell (append (list "pull" name) params)))

;; return list of docker names
(defun dc-popups-docker-names ()
  "Return a list of all docker containers."
  (interactive)
  (let ((container_ids (split-string (dc-popups-docker-run-return "" "ps" "-q" "" "") "\n" t)))
    (loop for el in container_ids collect
      (substring (dc-popups-docker-run-return el "inspect" "-f\"{{ .Name }}\"" "") 1 -1))))


;; return list of docker names
(defun dc-popups-docker-ps (&rest params)
  "Wrapper for ps command.
PARAMS      -- extra params"
  (interactive)
  (message "dc-popups-docker-ps %s" params)
  (apply 'dc-popups-docker-shell (append (list "ps") params)))


(provide 'dc-popups-docker)
;;; dc-popups-docker.el ends here
