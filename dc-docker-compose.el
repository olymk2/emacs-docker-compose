;;; docker-compose.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Oliver Marks

;; Author: Oliver Marks <oly@digitaloctave.com>
;; URL: https://github.com/olymk2/emacs-docker
;; Keywords: Docker Test 
;; Version: 0.1
;; Created 29 October 2016
;; TODO remove / update this
;; Package-Requires: ((projectile "0.14"))

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

;; TODO write this

;;; Code:

(require 'cl)
(require 'dc-core)

(defun dc-docker-compose-process (command &rest params)
  (interactive)
  (dc-compose-exists-check)
  (message "dc-docker-compose-process %s %s" command params)
  (apply 'dc-process (append (list dc-buffer-name dc-docker-compose-cmd command) params)))

;; TODO use default dir
;; wrapper for compose shell commands &optional backgrounded
(defun dc-docker-compose-run (container_name docker_cmd &rest params)
  (let ((cmd (append (list container_name) params)))
    (message "dc-docker-compose-run %s %s" cmd)
  (dc-compose-exists-check)
  (apply 'dc-docker-compose-process (append (list docker_cmd) cmd))))

;;TODO use default dir
;; wrapper for compose shell commands not backgrounded
(defun dc-docker-compose-run-return (name command &rest params)
  (unless background (setq background ""))
  (message "%s" (concatenate 'string "dc-docker-compose-run-return docker" command name params background))
  (dc-compose-exists-check)
  (let ((default-directory (dc-compose-root)))
  (shell-command-to-string
   (concatenate 'string "docker-compose " command name params background))))

(defun dc-docker-compose-build ()
  "Build your compose project"
  (interactive)
  (dc-docker-compose-process "build" "--no-color"))

(defun dc-docker-compose-up (container_name &optional flag)
  "Runs compose up, with optional -d parameter"
  (interactive (list (read-string "Container name:")))
  (unless flag (setq flag ""))
  (dc-docker-compose-process "up" "-d"))

(defun dc-docker-compose-down ()
  "Runs compose down"
  (interactive)
  (dc-docker-compose-process "down"))

(defun dc-docker-compose-logs (&optional flag)
  "Runs docker compose logs against the current project"
  (interactive)
  (unless flag (setq flag "")
    (dc-docker-compose-process "logs" "--no-color")))

;; bring up your compose container
(defun dc-docker-compose-ps (&optional flag)
  (interactive)
  (unless flag (setq flag ""))
  ;;(with-current-buffer dc-buffer 
  ;;(dc-docker-compose-run "" "ps" flag "")))
  (dc-docker-compose-process "logs"))

;; Docker IP Addresses
(defun dc-docker-compose-network ()
  (loop for name in (dc-docker-compose-names) collect 
    (dc-docker-run-return name dc-str-addresses "" "")))

;; run a command on a compose container
(defun dc-docker-compose-exec (name &rest command)
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  ;;(defvar name)
  ;;(unless command (setq command (read-string "Shell Command:")))
  (message "dc-dcker-compose-exec %s" name)
  (message "dc-dcker-compose-exec %s" command)
  (message "dc-dcker-compose-exec %s %s" name command)
  (let ((bind_path (docker-compose-bound-project-path name)))
    (apply 'dc-docker-compose-run (append (list name "exec") command))))


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


(provide 'dc-docker-compose) 
;;; docker-compose.el ends here
