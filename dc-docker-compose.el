;;; dc-docker-compose.el --- Take control of your docker containers -*- lexical-binding: t; -*-

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

;; return list of docker names
(defun dc-compose-names ()
  (interactive)
  (if (dc-compose-exists)
      (split-string (dc-compose-run-return "" "config" "--services" "") "\n" t)
    nil))

(defun dc-compose-process (command &rest params)
  (interactive)
  (dc-compose-exists-check)
  (message "dc-compose-process %s %s" command params)
  (apply 'dc-process (append (list dc-buffer-name  dc-compose-cmd (concat "--file=" dc-compose-file) command) params)))

;; wrapper for compose shell commands
(defun dc-compose-run (container_name &rest params)
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (let ((bind_path (docker-compose-bound-project-path container_name)))
    (apply 'dc-compose-process (append (list container_name "run") command))))

;; wrapper for compose shell commands
(defun dc-compose-run-return (name command &rest params)
  (dc-compose-exists-check)
  (let ((default-directory (dc-compose-root)))
  (shell-command-to-string
   (concat dc-compose-cmd " " command " " name " " (mapconcat 'identity params " ")))))

(defun dc-compose-build (&rest params)
  "Build your compose project"
  (interactive)
  (dc-compose-process "build"))

(defun dc-compose-up (&optional flag)
  "Runs compose up, with optional -d parameter"
  (interactive)
  (unless flag (setq flag ""))
  (dc-compose-process "up" "-d"))

(defun dc-compose-down ()
  "Runs compose down"
  (interactive)
  (dc-compose-process "down"))

(defun dc-compose-logs (&optional flag)
  "Runs docker compose logs against the current project"
  (interactive)
  (unless flag (setq flag "")
    (dc-compose-process "logs" "--no-color")))

;; bring up your compose container
(defun dc-compose-ps (&rest args)
  (interactive (list (docker-ps-popup)))
  (magit-popup-quit)
  (dc-compose-process (format "%s%s" (mapconcat 'identity args " ") "ps" "--no-color")))

;; Docker IP Addresses
(defun dc-compose-network ()
  (loop for name in (dc-compose-names) collect
    (dc-docker-run-return name dc-str-addresses "" "")))

;; run a command on a compose container
(defun dc-compose-exec (name &rest command)
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (let ((bind_path (docker-compose-bound-project-path name)))
    (apply 'dc-compose-process (append (list name "exec") command))))

;; give a container name, map the project path to the container path if possible
(defun docker-compose-bound-project-path (container)
  (let ((container_path
    (split-string
    (substring
      (shell-command-to-string
      (format "docker inspect --format='{{ json .HostConfig.Binds }}' %s" container)) 1 -1) ",")))

    ;; clean up the result of json .HostConfig.Binds
    (defsubst clean-bind (name listpos)
      (nth listpos (split-string (substring name 1 -1) ":")))

    ;; loop through the binds and filters ones that don't match the root path
    (loop for el in container_path if
      (string= (clean-bind el 0) (projectile-project-root)) collect (clean-bind el 1))))

(ert-deftest pp-test-compose-process-function ()
  "Test process wrapper joins parameters together correctly"
  (setq dc-current-buffer "/")
  (cl-letf (((symbol-function 'dc-process)
             (lambda (&rest params) params)))
    (should (equal
             (dc-process "a" "b" "c")
             (list "*Docker Info*" "/usr/bin/docker" "a" "b" "c")))))

(provide 'dc-docker-compose)
;;; dc-docker-compose.el ends here
