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

(setq dc-shell-command "sh -c")

(setq dc-docker-cmd
  (replace-regexp-in-string "\n$" ""
    (shell-command-to-string "which docker")))

(setq dc-docker-compose-cmd
  (replace-regexp-in-string "\n$" ""
    (shell-command-to-string "which docker-compose")))

(setq dc-buffer-shell (get-buffer-create "*Docker Shell*"))
(setq dc-buffer-name "*Docker Info*")
(setq dc-buffer-shell-name "*Docker Shell*")
(setq dc-buffer (get-buffer-create "*Docker Info*"))

(setq dc-str-addresses "inspect --format=\"{{printf \\\"%.30s\\\" .Name}} @ {{printf \\\"%.20s\\\" .Config.Image}} @ http://{{if ne \\\"\\\" .NetworkSettings.IPAddress}}{{ printf \\\"%.22s\\\" .NetworkSettings.IPAddress}}{{else}}{{range .NetworkSettings.Networks}}{{printf \\\"%.22s\\\" .IPAddress}}{{end}}{{end}} @ {{printf \\\"%.1emaresult of format as parametercs toggle debug on error jump to file0s\\\" .State.Status}}\" | column -t -s@ -c 80")

(defun dc-preflight-checks ()
  "Check if docker is availble on the system, might as well bail if its not"
  (dc-docker-compose-cmd)
  (file-exists-p (format "%sdocker-compose.yml" (dc-compose-root))))

(defun dc-docker-root ()
  "Try and find the root path by matching the Dockerfile"
  (let ((root-path (locate-dominating-file default-directory "Dockerfile")))
    (if root-path
        root-path
      "/")))

(defun dc-sentinel-gettext (process signal)
  (message "sentinal %s %s" process signal))


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
  (message "switching buffer to %s" buffer-name)
  (display-buffer buffer-name)
  (with-current-buffer buffer-name (special-mode)))


(defun dc-compose-root ()
  "Try and match project root to mounted volume inside container, return root if failure"
  (let ((root-path (locate-dominating-file (file-name-directory buffer-file-name) "docker-compose.yml")))
    (message "%s" root-path)
    (if root-path
      root-path
      "/")))

(defun dc-compose-exists ()
  "Test if docker-compose.yml is present and return t or f"
  (file-exists-p (format "%sdocker-compose.yml" (dc-compose-root))))

(defun dc-compose-exists-check ()
  "Error if there is no compose file else return true"
  (if (dc-compose-exists) t (error "Missing docker-compose.yml aborting current command %s" (dc-compose-root))))

(defun dc-docker-exists ()
  "Test if Dockerfile is present and return t or f"
  (file-exists- indentatiobp (format "%sDockerfile" (dc-docker-root))))

(defun dc-dockerfile-exists-check ()
 indentati indentatiobob  "Error if there is no Docker file else return true"
  (if (dc-dockerfile-exists) t (error "Missing docker-compose.yml aborting current command")))

(provide 'dc-core) 
;;; docker-compose.el ends here
