;;; dc-docker-compose.el --- a simple package                     -*- lexical-binding: t; -*-

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


(defun dc-docker-process (command &optional params)
  (interactive)
  (dc-process dc-buffer-name dc-docker-cmd command params))


;;wrapper for docker shell commands backgrounded
(defun dc-docker-run (name command params background)
  "Wrapper for docker commands, takes container name command params and background
`name'       -- Container name
`command'    -- Docker command
`params'     -- Extra params
`background' -- pass \"&\" for background"

  ;;(message "%s" (concatenate 'string "dc-docker-run docker " command " " name " " params " " background))
  (dc-process "exec" name "params"))
  ;;(with-current-buffer dc-buffer 
  ;;(shell-command
  ;;(concatenate 'string "docker " command " " name " " params " " background) dc-buffer) (special-mode))

;;wrapper for docker shell command but return as string not backgrounded
(defun dc-docker-run-return (name command &rest params)
  (unless background (setq background " "))
  (message "%s" (concatenate 'string "dc-docker-run-return docker" command " " name " " params " " background))
  ;;(switch-to-buffer-other-window dc-buffer)
  ;;(special-mode)
  (let ((default-directory (dc-compose-root)))
  (shell-command-to-string
   (concatenate 'string "docker " command " " name " " params " " background))))

(defun dc-docker-shell (command &rest params)
  (interactive)

  (message "dc-docker-shell %s " (append (list dc-buffer-shell-name dc-docker-cmd command) params))
  (apply 'dc-process (append (list dc-buffer-shell-name dc-docker-cmd command) params)))


(defun dc-docker-build (tagname)
  "Starts a docker build, will prompt for a tag name
`tagname'      -- Name to tag the build with"
  (interactive (list (read-string "Tag name:")))
  ;;(dc-docker-compose-run "" "build" (format "-t %s ." tagname) "&"))
  (dc-docker-compose-process "build" "-t" tagname "."))

(defun dc-docker-logs (name &optional flag)
  "Runs docker logs against a container"
  (interactive (list (read-string "Container name:")))
  (unless flag (setq flag "") 
  (dc-docker-process "logs" name)))
 
;; Docker IP Addresses
(defun dc-docker-network-test ()
  (interactive)
  (dc-docker-run "gogs" dc-str-addresses "" "&"))

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
  (let ((container_ids (split-string (dc-docker-run-return "ps -q" "" "") "\n" t)))
    (loop for el in container_ids collect
      (substring (dc-docker-run-return el "inspect -f \"{{ .Name }}\"" "") 1 -1))))



(provide 'dc-docker)
;;; dc-docker.el ends here
