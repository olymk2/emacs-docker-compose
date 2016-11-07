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

;;http://jakemccrary.com/blog/2013/08/10/emacs-capture-shell-command-output-in-temporary-buffer
;;(start-process "docker-process" "* Docker *" "shell) d

(require 'cl)
(require 'hydra)

(setq dc-shell-command "sh -c")
(setq dc-test-commands '(
  (php-mode . phpunit)
  (python-mode . pytest)))

(setq dc-docker-cmd
  (replace-regexp-in-string "\n$" ""
    (shell-command-to-string "which docker")))
(setq dc-docker-compose-cmd
  (replace-regexp-in-string "\n$" ""
    (shell-command-to-string "which docker-compose")))

;;(setq dc-buffer "*Docker Info*")
(setq dc-buffer-shell (get-buffer-create "*Docker Shell*"))
(setq dc-buffer-name "*Docker Info*")
(setq dc-buffer (get-buffer-create "*Docker Info*"))
;;(get-buffer-create dc-buffer)
;;(get-buffer-create dc-buffer-shell)
;;(switch-to-buffer-other-window dc-buffer)
;;(with-current-buffer dc-buffer (special-mode))
;;(with-current-buffer dc-buffer-shell (special-mode))
;;(with-current-buffer dc-buffer-shell (eshell-mode))
;;(buffer-local-value (special-mode) dc-buffer)

;;(setq major-mode 'special-mode)
;;(set-buffer-major-mode dc-buffer)
(setq dc-str-addresses "inspect --format=\"{{printf \\\"%.30s\\\" .Name}} @ {{printf \\\"%.20s\\\" .Config.Image}} @ http://{{if ne \\\"\\\" .NetworkSettings.IPAddress}}{{ printf \\\"%.22s\\\" .NetworkSettings.IPAddress}}{{else}}{{range .NetworkSettings.Networks}}{{printf \\\"%.22s\\\" .IPAddress}}{{end}}{{end}} @ {{printf \\\"%.10s\\\" .State.Status}}\" | column -t -s@ -c 80")

(defun dc-preflight-checks ()
  "Check if docker is availble on the system, might as well bail if its not"
  (shell-command "which docker"))

(defun dc-docker-root ()
  "Try and find the root path by matching the Dockerfile"
  (let ((root-path (locate-dominating-file default-directory "Dockerfile")))
    (if root-path
        root-path
      "/")))

(defun dc-sentinel-gettext (process signal)
  (message "sentinal %s %s" process signal))

(defun dc-process (command &rest args)
  (let ((params
         (list "dc-process" dc-buffer-name dc-docker-compose-cmd command)))
  ;;(message "%s" args)
  ;;(if (eq args (list nil)) (setq params (append params args)))
  (let ((default-directory (dc-compose-root)))
  (with-current-buffer dc-buffer
    (message "%s" "dcprocess paths")
    (message "%s" params)
    (message "%s" default-directory)
   ;; use apply to call function with list of params
  (set-process-sentinel
    (apply 'start-process params) 'dc-sentinel-gettext)
   (special-mode)))))

(defun dc-compose-root ()
  "Try and match project root to mounted volume inside container, return root if failure"
  ;;(message "%s" (buffer-file-name))
  ;;(message "%s" (or load-file-name (buffer-file-name)))
  (message "%s" "dc-compose-root")
  (message "%s" (file-name-directory buffer-file-name))
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
  (file-exists-p (format "%sDockerfile" (dc-docker-root))))

(defun dc-dockerfile-exists-check ()
  "Error if there is no Docker file else return true"
  (if (dc-dockerfile-exists) t (error "Missing docker-compose.yml aborting current command")))

;;wrapper for docker shell commands backgrounded
(defun dc-docker-run (name command params background)
  "Wrapper for docker commands, takes container name command params and background
`name'       -- Container name
`command'    -- Docker command
`params'     -- Extra params
`background' -- pass \"&\" for background"

  (message "%s" (concatenate 'string "dc-docker-run docker " command " " name " " params " " background))

  (with-current-buffer dc-buffer 
  (shell-command
   (concatenate 'string "docker " command " " name " " params " " background) dc-buffer) (special-mode)))

;;wrapper for docker shell command but return as string not backgrounded
(defun dc-docker-run-return (name command params &optional background)
  (unless background (setq background "&"))
  (message "%s" (concatenate 'string "dc-docker-run-return docker" command " " name " " params " " background))
  ;;(switch-to-buffer-other-window dc-buffer)
  ;;(special-mode)
  (let ((default-directory (dc-compose-root)))
  (shell-command-to-string
   (concatenate 'string "docker " command " " name " " params " " background))))

(defun dc-docker-compose-process (command &rest params)
  (interactive)
  (dc-compose-exists-check)
  ;;(let ((default-directory (dc-compose-root)))
    (dc-process command params));;)

;; TODO use default dir
;; wrapper for compose shell commands &optional backgrounded
(defun dc-docker-compose-run (name command params &optional background)
  (unless background (setq background ""))
  (message "%s" (concatenate 'string "dc-docker-compose-run docker " command " " name " " params " " background))
  (dc-compose-exists-check)
  (dc-process "" (concatenate 'string command " " name " " params)))
  ;;(let ((default-directory (dc-compose-root)))
    ;;(dc-process "" (concatenate 'string command " " name " " params))))
    ;;(with-current-buffer dc-buffer 
      ;;(dc-process 
       ;;("/usr/bin/docker-compose" (concatenate 'string "command " " name " " params " )))))

  ;;(with-current-buffer dc-buffer 
  ;;(shell-command
   ;;(concatenate 'string "docker-compose " command " " name " " params " " background) dc-buffer) (special-mode))))

;;TODO use default dir
;; wrapper for compose shell commands not backgrounded
(defun dc-docker-compose-run-return (name command params &optional background)
  (unless background (setq background ""))
  (message "%s" (concatenate 'string "dc-docker-compose-run-return docker" command name params background))
  (dc-compose-exists-check)
  (let ((default-directory (dc-compose-root)))
  (shell-command-to-string
   (concatenate 'string "docker-compose " command name params background))))

(defun dc-docker-build (tagname)
  "Starts a docker build, will prompt for a tag name
`tagname'      -- Name to tag the build with"
  (interactive (list (read-string "Tag name:")))
  (dc-docker-compose-run "" "build" (format "-t %s ." tagname) "&"))

(defun dc-docker-compose-up (&optional flag)
  "Runs compose up, with optional -d parameter"
  (interactive)
  (unless flag (setq flag ""))
  (dc-docker-compose-process "up"))

(defun dc-docker-compose-down ()
  "Runs compose down"
  (interactive)
  (dc-docker-compose-process "down"))

(defun dc-docker-logs (&optional flag)
  "Runs docker logs against the current project"
  (interactive (list (read-string "Container name:")))
  (unless flag (setq flag "") (dc-docker-run "" "logs" flag "&")))

(defun dc-docker-compose-logs (&optional flag)
  "Runs docker compose logs against the current project"
  (interactive)
  (unless flag (setq flag "")
  (dc-docker-compose-run "" "logs" flag "&")))

;; bring up your compose container
(defun dc-docker-compose-ps (&optional flag)
  (interactive)
  (unless flag (setq flag ""))
  (with-current-buffer dc-buffer 
  (dc-docker-compose-run "" "ps" flag "")))
 
;; Docker IP Addresses
(defun dc-docker-network-test ()
  (interactive)
  (dc-docker-run "gogs" dc-str-addresses "" "&"))

;; Docker IP Addresses
(defun dc-docker-network ()
  (with-current-buffer dc-buffer 
  (loop for name in (dc-docker-names) collect
    (dc-docker-run-return name dc-str-addresses "" ""))))

;; Docker IP Addresses
(defun dc-docker-compose-network ()
  (loop for name in (dc-docker-compose-names) collect 
    (dc-docker-run-return name dc-str-addresses "" "")))

;; run a command on a docker container
(defun dc-docker-exec (name &optional command)
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (unless command (setq command (read-string "Shell Command:")))
  (dc-docker-run name "exec -it" command "&"))

;; run a command on a compose container
(defun dc-docker-compose-exec (name &optional command)
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  ;;(defvar name)
  (unless command (setq command (read-string "Shell Command:")))
  (let ((bind_path (docker-compose-bound-project-path name)))
    (dc-docker-compose-run name "exec" command "&")))

;; return list of docker names
(defun dc-docker-names ()
  (interactive)
  (let ((container_ids (split-string (dc-docker-run-return "ps -q" "" "") "\n" t)))
    (loop for el in container_ids collect
      (substring (dc-docker-run-return el "inspect -f \"{{ .Name }}\"" "") 1 -1))))

;; return list of docker names
(defun dc-docker-compose-names ()
  (interactive)
  (message "%s" (dc-compose-exists))
  (if (dc-compose-exists)
    (split-string (dc-docker-compose-run-return "" "config --services" "") "\n" t)
    nil))

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

;; show helm view to select container by name
(defun dc-helm-select-container ()
  (interactive)
  ;; helm source for docker containers
  (setq helm-docker-containers
        '((name . "Docker Containers")
          (candidates . dc-docker-names)
          (action . (("Run command inside container" . (lambda (candidate)
                      (dc-docker-exec candidate)))
                     ("Alternate command" . (lambda (candiadte) () ))))))

  ;; helm source for docker compose containers
  (setq helm-docker-compose-containers
        '((name . "Docker Compose Containers")
          (candidates . dc-docker-compose-names)
          (action . (lambda (candidate)
                      (dc-docker-compose-exec candidate)))))

  (helm :sources '(helm-docker-compose-containers helm-docker-containers) :buffer "*helm container*"))

(defun dc-run-test (function_name)
  (interactive)
  (message "%s" function_name)
)

(defun dc-test-file ()
    (interactive)
    (replace-regexp-in-string (docker-compose-bound-project-path) "" buffer-file-name)
)

(defun dc-python-test ()
  ;;(dc-docker-compose-exec "mhackspace_uwsgi" "import nose; nose.run()")pytest
  (dc-docker-compose-exec "mhackspace_uwsgi" "pytest")
)

(defun dc-php-test ()
  ;;(dc-docker-compose-exec "mhackspace_uwsgi" "import nose; nose.run()")pytest
  (dc-docker-compose-exec "mhackspace_uwsgi" "/var/www/vendor/bin/phpunit ")
)

(defhydra dc-launcher (:color blue :columns 4)
"
Docker Compose Menu
| Docker Compose                                | Docker               |
|-----------------------------------------------|----------------------|
| _u_: Up Background    | _U_: Up Foreground    | _b_: Build Container |
| _l_: Logs             | _t_: Tailed logs      | _l_: Logs            |
| _s_: Select Container | _p_: List Containers  | _n_: Info            
| _p_: List Containers  | _N_: Info             |
"

  ("\\" dc-launcher/body "back")
  ("U" (dc-docker-compose-up) "Startup")
  ("u" (dc-docker-compose-up "-d") "Startup Background")
  ("d" (dc-docker-compose-down) "Shutdown" :color red)
  ("L" (dc-docker-compose-logs) "Logs")
  ("t" (dc-docker-compose-logs) "Logs")
  ("l" (dc-docker-logs) "Logs")
  ("p" (dc-docker-compose-ps) "Process list")
  ("n" (dc-docker-network) "Address list")
  ("b" (dc-docker-build) "Build container")
  ("N" (dc-docker-compose-network) "Compose Address list")
  ("L" (dc-docker-compose-logs "-f") "Logs Realtime")
  ("e" (dc-docker-compose-exec) "Run command")
  ("s" (dc-helm-select-container) "Select Container")
  ("q" nil "Quit"))

(provide 'docker-compose) 
;;; docker-compose.el ends here
