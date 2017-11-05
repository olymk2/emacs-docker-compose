;;; dc-popups-compose.el --- Take control of your docker containers -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Oliver Marks

;; Author: Oliver Marks <oly@digitaloctave.com>
;; URL: https://github.com/olymk2/emacs-docker
;; Keywords: Processes tools
;; Version: 0.1
;; Created 13 October 2017
;; Package-Requires: ((magit "2.5")(emacs "24.3"))

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
  (setq dc-popups-current-compose-container (completing-read "Select container:" (dc-popups-compose-names))))

(defun dc-popups-compose-names ()
  "Return a list of compose containers for current project."
  (interactive)
  (if (dc-popups-core-compose-exists)
      (split-string (dc-popups-compose-run-return "" "config" "--services" "") "\n" t)
    nil))

(defun dc-popups-compose-process (command &rest params)
  "Core compose wrapper function, to add buffer and cmd path.
`COMMAND'    -- Docker command
`PARAMS'     -- Extra params"
  (interactive)
  (dc-popups-core-compose-exists-check)
  (message "dc-popups-compose-process %s %s" command params)
  (apply 'dc-popups-core-process (append (list dc-popups-buffer-name  dc-popups-compose-cmd (concat "--file=" dc-popups-compose-file) command) params)))

;; wrapper for compose shell commands
(defun dc-popups-compose-run (container_name &rest params)
  "Wrapper for compose run commands.
CONTAINER_NAME   -- name of container to run command against
PARAMS           -- extra run params"
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (let ((bind_path (docker-compose-bound-project-path container_name)))
    (apply 'dc-popups-compose-process (append (list container_name "run") command))))

;; wrapper for compose shell commands
(defun dc-popups-compose-run-return (name command &rest params)
  "Wrapper for compose commands but blocking instant return.
NAME             -- name of container to run command against
COMMAND          -- compose command to run
PARAMS           -- extra run params"
  (dc-popups-core-compose-exists-check)
  (let ((default-directory (dc-popups-core-compose-root)))
  (shell-command-to-string
   (concat dc-popups-compose-cmd " " command " " name " " (mapconcat 'identity params " ")))))

(defun dc-popups-compose-build (&rest params)
  "Build your compose project.
PARAMS       -- extra build params"
  (interactive)
  (dc-popups-compose-process "build"))

(defun dc-popups-compose-up (&optional flag)
  "Run compose up, with optional -d parameter.
FLAG              -- extra up params"
  (interactive)
  (unless flag (setq flag ""))
  (dc-popups-compose-process "up" "-d"))

(defun dc-popups-compose-down ()
  "Run compose down."
  (interactive)
  (dc-popups-compose-process "down"))

(defun dc-popups-compose-logs (&optional flag)
  "Run docker compose logs against the current project.
FLAG       -- extra build params"
  (interactive)
  (unless flag (setq flag "")
    (dc-popups-compose-process "logs" "--no-color")))

;; bring up your compose container
(defun dc-popups-compose-ps (&rest args)
  "Run docker compose ps against the current project.
ARGS       -- extra build params"
  (interactive (list (docker-ps-popup)))
  (magit-popup-quit)
  (dc-popups-compose-process (format "%s%s" (mapconcat 'identity args " ") "ps" "--no-color")))

;; Docker IP Addresses
(defun dc-popups-compose-network ()
  "PS wrapper with nicer format."
  (loop for name in (dc-popups-compose-names) collect
    (dc-popups-docker-run-return name dc-popups-str-addresses "" "")))

;; run a command on a compose container
(defun dc-popups-compose-exec (name &rest flags)
  "Wrapper around compose exec command.
NAME          -- name of container
FLAGS         -- extra flags"
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (let ((bind_path (docker-compose-bound-project-path name)))
    (apply 'dc-popups-compose-process (append (list name "exec") flags))))

;; give a container name, map the project path to the container path if possible
(defun dc-popups-compose-bound-project-path (container)
  "Helper function match path in container with path outside container.
CONTAINER          -- name of container"
  (let ((container_path
    (split-string
    (substring
      (shell-command-to-string
      (format "docker inspect --format='{{ json .HostConfig.Binds }}' %s" container)) 1 -1) ",")))

    ;; clean up the result of json .HostConfig.Binds
    (defsubst dc-popups-compose-clean-bind (name listpos)
      (nth listpos (split-string (substring name 1 -1) ":")))

    ;; loop through the binds and filters ones that don't match the root path
    (loop for el in container_path if
          (string=
           (dc-popups-compose-clean-bind el 0)
           (projectile-project-root)) collect
           (dc-popups-compose-clean-bind el 1))))


(provide 'dc-popups-compose)
;;; dc-popups-compose.el ends here
