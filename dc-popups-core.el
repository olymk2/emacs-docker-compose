;;; dc-popups-core.el --- Take control of your docker containers -*- lexical-binding: t; -*-

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


;; Variables to override
(setq dc-shell-command "sh -c")

;; Internal states
(defun dc-popups-core-reset-state ()
  "Reset state values back to defaults."
  (setq dc-popups-docker-file "Dockerfile")
  (setq dc-popups-compose-file "docker-compose.yml")
  (setq dc-popups-current-buffer nil)
  (setq dc-popups-current-docker-container nil)
  (setq dc-popups-current-compose-container nil))
(dc-popups-core-reset-state)
;; (setq dc-popups-docker-file "Dockerfile")
;; (setq dc-popups-compose-file "docker-compose.yml")
;; (setq dc-popups-current-buffer nil)
;; (setq dc-popups-current-docker-container nil)
;; (setq dc-popups-current-compose-container nil)

;; Internal settings
(setq dc-popups-buffer-shell (get-buffer-create "*Docker Shell*"))
(setq dc-popups-buffer-name "*Docker Info*")
(setq dc-popups-buffer-shell-name "*Docker Shell*")
(setq dc-popups-buffer (get-buffer-create "*Docker Info*"))

(setq dc-popups-docker-cmd
  (replace-regexp-in-string "\n$" ""
    (shell-command-to-string "which docker")))

(setq dc-popups-compose-cmd
  (replace-regexp-in-string "\n$" ""
    (shell-command-to-string "which docker-compose")))


(setq dc-popups-str-addresses "inspect --format=\"{{printf \\\"%.30s\\\" .Name}} @ {{printf \\\"%.20s\\\" .Config.Image}} @ http://{{if ne \\\"\\\" .NetworkSettings.IPAddress}}{{ printf \\\"%.22s\\\" .NetworkSettings.IPAddress}}{{else}}{{range .NetworkSettings.Networks}}{{printf \\\"%.22s\\\" .IPAddress}}{{end}}{{end}} @ {{printf \\\"%.1emaresult of format as parametercs toggle debug on error jump to file0s\\\" .State.Status}}\" | column -t -s@ -c 80")


(defun dc-popups-select-container()
  "Prompt user to pick container from a list."
  (interactive)
  (message "%s" (completing-read "Open bookmark:" (dc-popups-docker-names))))


(defun dc-popups-core-match-param-value (param args)
  "Regex out a parameter value from  string.
PARAM is the parameter to find
ARGS is the string to match against"
  (unless args (setq args ""))
  (if (string-match (format "%s%s%s"  "\\(" param "\\)\\([0-9A-z.]+\\)") args)
  (match-string 2 args) nil))

(defun dc-popups-core-fetch-dockerfile-from-param (args)
  "Specifically regex out the file param.
ARGS is the string to search with in"
  (let ((fname (dc-popups-core-match-param-value "--file" args)))
    (unless fname (setq fname "docker-compose.yml"))
    (setq dc-popups-compose-file fname)))


(defun dc-popups-core-preflight-checks ()
  "Check if docker is availble on the system, might as well bail if its not."
  (dc-popups-core-compose-cmd)
  (file-exists-p (format "%sdocker-compose.yml" (dc-popups-core-compose-root))))


(defun dc-popups-core-sentinel-gettext (process signal)
  "Callback when something happens to the sentinal process.
PROCESS where the message came from
SIGNAL the processes signal"
  (message "sentinal %s %s" process signal))

(defun dc-popups-core-reset-buffer ()
  "Reset the sentinal's buffer."
  (read-only-mode)
  (erase-buffer))

(defun dc-popups-core-setup-buffer (buffer-name)
  "Setup the sentinal's buffer.
BUFFER-NAME name of the buffer to configure"
  (message "switching buffer to %s" buffer-name)
  (display-buffer buffer-name)
  (with-current-buffer buffer-name (special-mode)))

;; Async method which will call the sentinel on exit
(defun dc-popups-core-process (buffer-name docker-cmd command &rest args)
  "Wrapper around start process, connect to buffer and spawn a watcher.
BUFFER-NAME buffer the output will be sent to
DOCKER-CMD path to the docker command
COMMAND the docker command to run
ARGS parameters to append to the command"
  (let ((params (list "dc-popups-core-process" buffer-name docker-cmd command)))
    ;; Append extra args
    (if args (setq params (append params args)))
    (let ((default-directory (dc-popups-core-compose-root)))
      (message "dc-popups-core-process %s" params)
      ;; use apply to call function with list of params
      (set-process-sentinel
       (apply 'start-process params) 'dc-popups-core-sentinel-gettext)))
  (dc-popups-core-setup-buffer buffer-name))


(defun dc-popups-core-compose-root ()
  "Try and match project root to mounted volume inside container, return root if failure."
  (let ((root-path
         (locate-dominating-file
          (file-name-directory dc-popups-current-buffer)
          dc-popups-compose-file)))
    (if root-path root-path "/")))

(defun dc-popups-core-compose-exists ()
  "Test if docker-compose.yml is present and return t or f."
  (message "dc-popups-compose-exists%s%s" (dc-popups-core-compose-root) dc-popups-compose-file)
  (file-exists-p (format "%s%s" (dc-popups-core-compose-root) dc-popups-compose-file)))

(defun dc-popups-core-compose-exists-check ()
  "Error if there is no compose file else return true."
  (if (dc-popups-core-compose-exists) t (error "Missing %s in project root aborting current command %s" dc-popups-compose-file (dc-popups-core-compose-root))))

(defun dc-popups-core-dockerfile-root ()
  "Try and find the root path by matching the Dockerfile."
  (let ((root-path (locate-dominating-file default-directory "Dockerfile")))
    (if root-path root-path "/")))

(defun dc-popups-core-dockerfile-exists ()
  "Test if Dockerfile is present and return t or f."
  (file-exists-p (format "%sDockerfile" (dc-popups-core-dockerfile-root))))

(defun dc-popups-core-dockerfile-exists-check ()
  "Error if there is no Docker file else return true."
  (if (dc-popups-core-dockerfile-exists) t
    (error "Missing Dockerfile in project root aborting current command")))

(provide 'dc-popups-core)
;;; dc-popups-core.el ends here
