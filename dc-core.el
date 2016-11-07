;;; dc-core.el --- Take control of your docker containers -*- lexical-binding: t; -*-

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

;; Variables to override
(setq dc-shell-command "sh -c")

;; Internal states
(defun dc-reset-state ()
  (setq dc-docker-file "Dockerfile")
  (setq dc-compose-file "docker-compose.yml")
  (setq dc-current-buffer nil)
  (setq dc-current-docker-container nil)
  (setq dc-current-compose-container nil))
(dc-reset-state)
;; (setq dc-docker-file "Dockerfile")
;; (setq dc-compose-file "docker-compose.yml")
;; (setq dc-current-buffer nil)
;; (setq dc-current-docker-container nil)
;; (setq dc-current-compose-container nil)

;; Internal settings
(setq dc-buffer-shell (get-buffer-create "*Docker Shell*"))
(setq dc-buffer-name "*Docker Info*")
(setq dc-buffer-shell-name "*Docker Shell*")
(setq dc-buffer (get-buffer-create "*Docker Info*"))

(setq dc-docker-cmd
  (replace-regexp-in-string "\n$" ""
    (shell-command-to-string "which docker")))

(setq dc-compose-cmd
  (replace-regexp-in-string "\n$" ""
    (shell-command-to-string "which docker-compose")))


(setq dc-str-addresses "inspect --format=\"{{printf \\\"%.30s\\\" .Name}} @ {{printf \\\"%.20s\\\" .Config.Image}} @ http://{{if ne \\\"\\\" .NetworkSettings.IPAddress}}{{ printf \\\"%.22s\\\" .NetworkSettings.IPAddress}}{{else}}{{range .NetworkSettings.Networks}}{{printf \\\"%.22s\\\" .IPAddress}}{{end}}{{end}} @ {{printf \\\"%.1emaresult of format as parametercs toggle debug on error jump to file0s\\\" .State.Status}}\" | column -t -s@ -c 80")



(defun dc-match-param-value (param args)
  (unless args (setq args ""))
  (if (string-match (format "%s%s%s"  "\\(" param "\\)\\([0-9A-z.]+\\)") args)
  (match-string 2 args) nil))

(defun dc-fetch-dockerfile-from-param (args)
  (let ((fname (dc-match-param-value "--file" args)))
    (unless fname (setq fname "docker-compose.yml"))
    (setq dc-compose-file fname)))


(defun dc-preflight-checks ()
  "Check if docker is availble on the system, might as well bail if its not"
  (dc-compose-cmd)
  (file-exists-p (format "%sdocker-compose.yml" (dc-compose-root))))


(defun dc-sentinel-gettext (process signal)
  (message "sentinal %s %s" process signal))

(defun dc-reset-buffer
  (read-only-mode)
  (erase-buffer))

(defun dc-setup-buffer (buffer-name)
  (message "switching buffer to %s" buffer-name)
  (display-buffer buffer-name)
  (with-current-buffer buffer-name (special-mode)))

;; Async method which will call the sentinel on exit
(defun dc-process (buffer-name docker-cmd command &rest args)
  "Wrapper around start process, that connects to buffer and spawn a sentinel watcher"
  (let ((params (list "dc-process" buffer-name docker-cmd command)))
    ;; Append extra args
    (if args (setq params (append params args)))
    (let ((default-directory (dc-compose-root)))
      (message "dc-process %s" params)
      ;; use apply to call function with list of params
      (set-process-sentinel
       (apply 'start-process params) 'dc-sentinel-gettext)))
  (dc-setup-buffer buffer-name))


(defun dc-compose-root ()
  "Try and match project root to mounted volume inside container, return root if failure"
  (let ((root-path
         (locate-dominating-file
          (file-name-directory dc-current-buffer)
          dc-compose-file)))
    (if root-path root-path "/")))

(defun dc-compose-exists ()
  "Test if docker-compose.yml is present and return t or f"
  (message "dc-compose-exists%s%s" (dc-compose-root) dc-compose-file)
  (file-exists-p (format "%s%s" (dc-compose-root) dc-compose-file)))

(defun dc-compose-exists-check ()
  "Error if there is no compose file else return true"
  (if (dc-compose-exists) t (error "Missing %s in project root aborting current command %s" dc-compose-file (dc-compose-root))))

(defun dc-dockerfile-root ()
  "Try and find the root path by matching the Dockerfile"
  (let ((root-path (locate-dominating-file default-directory "Dockerfile")))
    (if root-path root-path "/")))

(defun dc-dockerfile-exists ()
  "Test if Dockerfile is present and return t or f"
  (file-exists-p (format "%sDockerfile" (dc-dockerfile-root))))

(defun dc-dockerfile-exists-check ()
  "Error if there is no Docker file else return true"
  (if (dc-dockerfile-exists) t
    (error "Missing Dockerfile in project root aborting current command")))

(provide 'dc-core) 
;;; dc-core.el ends here
