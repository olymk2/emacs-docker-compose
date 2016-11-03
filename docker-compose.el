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
;;(start-process "docker-process" "* Docker *" "shell)

(require 'cl)
(require 'hydra)


(setq dc-test-commands '(
  (php-mode . phpunit)
  (python-mode . pytest)))

(setq dc-buffer "*Docker Info*")
(setq dc-buffer-shell "*Docker Shell*")
(get-buffer-create dc-buffer)
(get-buffer-create dc-buffer-shell)
;;(switch-to-buffer-other-window dc-buffer)
(with-current-buffer dc-buffer (special-mode))
;;(with-current-buffer dc-buffer-shell (shell-mode))

(setq dc-str-addresses "inspect --format=\"{{printf \\\"%.30s\\\" .Name}} @ {{printf \\\"%.20s\\\" .Config.Image}} @ http://{{if ne \\\"\\\" .NetworkSettings.IPAddress}}{{ printf \\\"%.22s\\\" .NetworkSettings.IPAddress}}{{else}}{{range .NetworkSettings.Networks}}{{printf \\\"%.22s\\\" .IPAddress}}{{end}}{{end}} @ {{printf \\\"%.10s\\\" .State.Status}}\" | column -t -s@ -c 80")

;;(setq dc-str-addresses "inspect --format=\"{{printf \\\"%%.30s\\\" .Name}}")
 
(concatenate 'string "test " dc-str-addresses " string ")
;;(message (concatenate 'string "test " dc-str-addresses " string "))


;;(format "%s" dc-str-addresses)
;; (message (format "inspect -f %s" dc-str-addresses))

;;(message (format "inspect -f \"%s\"" dc-str-addresses))
;; hunt for compose project root
(defun dc-compose-root ()
  (message "%s" (format "dc-compose-root %s" default-directory))
  (let ((root-path (locate-dominating-file default-directory "docker-compose.yml")))
    (if root-path
      root-path
      "/")))

;; hunt for compose project root
(defun dc-compose-exists ()
  (file-exists-p (format "%sdocker-compose.yml" (dc-compose-root))))

(defun dc-compose-exists-check ()
  (if (dc-compose-exists)
    t
    (error "Missing docker-compose.yml aborting current command")))

;;wrapper for docker shell commands backgrounded
(defun dc-docker-run (name command params background)
  (message "%s" "dc-docker-run")
  (message "%s" name)
  (message "%s" (concatenate 'string "dc-docker-run docker " command " " name " " params " " background))
  (shell-command
   (concatenate 'string "docker " command " " name " " params " " background)))

;;wrapper for docker shell command but return as string not backgrounded
(defun dc-docker-run-return (name command params &optional background)
  (unless background (setq background "&"))
  (message "%s" (concatenate 'string "dc-docker-run-return docker" command " " name " " params " " background))
  ;;(switch-to-buffer-other-window dc-buffer)
  ;;(special-mode)
  (shell-command-to-string
   (concatenate 'string "docker " command " " name " " params " " background)))

;;TODO use default dir
;; wrapper for compose shell commands &optional backgrounded
(defun dc-docker-compose-run (name command params &optional background)
  (unless background (setq background ""))
  (message "%s" (concatenate 'string "dc-docker-compose-run docker" command " " name " " params " " background))
  (dc-compose-exists-check)
  ;;(switch-to-buffer-other-window dc-buffer)
  ;;(with-current-buffer dc-buffer 
  ;;  (special-mode))
  (shell-command
   (concatenate 'string "cd " (dc-compose-root) ";docker-compose " command " " name " " params " " background)))

;;TODO use default dir
;; wrapper for compose shell commands not backgrounded
(defun dc-docker-compose-run-return (name command params &optional background)
  (unless background (setq background ""))
  (message "%s" (concatenate 'string "dc-docker-compose-run-return docker" command name params background))
  (dc-compose-exists-check)
  (shell-command-to-string
   (concatenate 'string "cd " (dc-compose-root) ";docker-compose " command name params background)))

;; bring up your compose container
(defun dc-docker-compose-up (&optional flag)
  (interactive)
  (unless flag (setq flag ""))
  (dc-docker-compose-run "" "up" flag "&"))

;; shutdown your compose container
(defun dc-docker-compose-down ()
  (interactive)
  (dc-docker-compose-run "down" "" "" "&"))

;; bring up your compose containewdr
(defun dc-docker-compose-logs (&optional flag)
  (interactive)
  (unless flag (setq flag "")
  (dc-docker-compose-run "" "up" flag "&")))

;; bring up your compose container
(defun dc-docker-compose-ps (&optional flag)
  (interactive)
  (unless flag (setq flag ""))
  (dc-docker-compose-run "" "ps" flag "&"))
 
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
| Docker Compose                                | Docker |
|-----------------------------------------------|--------|
| _u_: Start Background | _U_: Start Foreground |
| _l_: Logs             | _L_: Logs realtime    |
| _p_: List Containers  | _n_: Addresses        |
| _s_: Select Container | _N_: Addresses        |
"
  ("U" (dc-docker-compose-up) "Startup")
  ("u" (dc-docker-compose-up "-d") "Startup Background")
  ("d" (dc-docker-compose-down) "Shutdown")
  ("l" (dc-docker-compose-logs) "Logs")
  ("p" (dc-docker-compose-ps) "Process list")
  ("n" (dc-docker-network) "Address list")
  ("N" (dc-docker-compose-network) "Compose Address list")
  ("L" (dc-docker-compose-logs "-f") "Logs Realtime")
  ("e" (dc-docker-compose-exec) "Run command")
  ("s" (dc-helm-select-container) "Select Container")
  ("q" nil "Quit"))

;;(global-set-key (kbd "C-c d") 'dc-launcher/body)
;;(evil-leader/set-key "d" 'dc-launcher/body)
;;; docker-compose.el ends here
;;(provide 'docker-compose) 
