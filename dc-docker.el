;;; dc-docker.el --- Take control of your docker containers -*- lexical-binding: t; -*-

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

(defun dc-docker-process (command &rest params)
  "Core docker wrapper function, to add buffer and cmd path.
`COMMAND'    -- Docker command
`PARAMS'     -- Extra params"
  (interactive)
  (let ((commands (append (list dc-buffer-name dc-docker-cmd command) params)))
  (message "dc-docker-process %s" commands)
  (apply 'dc-core-process commands)))

;;wrapper for docker shell commands backgrounded
(defun dc-docker-run (name command params)
  "Wrapper for docker commands can probably be replaced with dc-docker-process.
`NAME'       -- Container name
`COMMAND'    -- Docker command
`PARAMS'     -- Extra params"
  (dc-core-process command name params))

;;wrapper for docker shell command but return as string not backgrounded
(defun dc-docker-run-return (name command &rest params)
  "Use shell to string and return result instantly blocking.
`NAME'       -- Container name
`COMMAND'    -- Docker command
`PARAMS'     -- Extra params"
  (message "dc-docker-run-return%s" (concat dc-docker-cmd " " command " " name " " (mapconcat 'identity params " ")))
  (let ((default-directory (dc-core-compose-root)))
  (shell-command-to-string
   (concat dc-docker-cmd " " command " " name " " (mapconcat 'identity params " ")))))

;; wrapper command should probably deprecate
(defun dc-docker-shell (command &rest params)
  "Wrapper should probably use dc-process now.
`COMMAND'    -- Docker command
`PARAMS'     -- Extra params"
  (interactive)
  (apply 'dc-core-process (append (list dc-buffer-shell-name dc-docker-cmd command) params)))


(defun dc-docker-build (tagname)
  "Start a docker build, will prompt for a tag name.
`TAGNAME'    -- Name to tag the build with"
  (interactive (list (read-string "Tag name:")))
  (dc-dockerfile-exists-check)
  (dc-docker-process "build" "-t" tagname "."))

(defun dc-docker-logs (&rest flags)
  "Run docker logs against a container.
FLAGS        -- extra build flags"
  (dc-helm-choose-container "Container name:")
  (unless flags (setq flags (list)))
  (apply 'dc-docker-process (append (list "logs" dc-current-docker-container) flags)))


;; Docker IP Addresses
(defun dc-docker-network ()
  "Wrapper command for ps to return the ip addresses."
  (with-current-buffer dc-buffer
  (loop for name in (dc-docker-names) collect
    (dc-docker-run-return name dc-str-addresses "" ""))))

;; run a command on a docker container
(defun dc-docker-exec (name &rest command)
  "Docker exec wrapper.
NAME         -- name of container
COMMAND      -- command to exec inside container"
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (unless command (setq command (read-string "Shell Command:")))
  (message "dc-docker-exec %s %s" name command)
  (apply 'dc-docker-shell (append (list "exec" "-it" name) command)))

;; run a command on a docker container
(defun dc-docker-pull (name &rest params)
  "Docker pull wrapper.
NAME         -- name of image
PARAMS       -- flags to append to pull"
  (interactive (list (read-string "Image name:")))
  (message "dc-docker-pull %s %s" name params)
  (apply 'dc-docker-shell (append (list "pull" name) params)))

;; return list of docker names
(defun dc-docker-names ()
  "Return a list of all docker containers."
  (interactive)
  (let ((container_ids (split-string (dc-docker-run-return "" "ps" "-q" "" "") "\n" t)))
    (message "dc-docker-names %s" container_ids)
    (message "dc-docker-names %s" (car container_ids))
    (message "dc-docker-names %s" (substring (car container_ids) 1 -1))
    (loop for el in container_ids collect
      (substring (dc-docker-run-return el "inspect" "-f\"{{ .Name }}\"" "") 1 -1))))


;; return list of docker names
(defun dc-docker-ps (&rest params)
  "Wrapper for ps command.
PARAMS      -- extra params"
  (interactive)
  (message "dc-docker-ps %s" params)
  (apply 'dc-docker-shell (append (list "ps") params)))


(provide 'dc-docker)
;;; dc-docker.el ends here
