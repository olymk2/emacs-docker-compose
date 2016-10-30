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
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO write this

;;; Code:

(require 'cl)
(require 'hydra)


(setq dc-test-commands '(
  (php-mode . phpunit)
  (python-mode . pytest)))

;; hunt for compose project root
(defun dc-compose-root ()
  (let ((root-path (locate-dominating-file default-directory "docker-compose.yml")))
    (if root-path
      root-path
      (error "Missing docker-compose.yml not found in directory tree"))))

;;wrapper for docker shell commands backgrounded
(defun dc-docker-run (name command params)
  (message (format "dc-docker-run docker %s %s %s &" command name params))
  (shell-command
    (format "docker %s %s %s &" command name params)))

;;wrapper for docker shell command but return as string not backgrounded
(defun dc-docker-run-return (name command params)
  (message (format "dc-docker-run-return docker %s %s %s" command name params))
  (shell-command-to-string
    (format "docker %s %s %s" command name params)))

;; wrapper for compose shell commands backgrounded
(defun dc-docker-compose-run (name command params)
  (message (format "dc-docker-compose-run docker-compose %s %s %s &" command name params))
  (shell-command
   (format "cd %s;docker-compose %s %s %s &" (dc-compose-root) command name params)))

;; wrapper for compose shell commands not backgrounded
(defun dc-docker-compose-run-return (name command params)
  (message (format "dc-docker-compose-run-return docker-compose %s %s %s" command name params))
  (shell-command-to-string
   (format "cd %s;docker-compose %s %s %s" (dc-compose-root) command name params)))

;; bring up your compose container
(defun dc-docker-compose-up (&optional flag)
  (interactive)
  (unless flag (setq command (read-string "Shell Command:")))
  (dc-docker-compose-run "up" flag))

;; bring up your compose container
(defun dc-docker-compose-logs (&optional flag)
  (interactive)
  (unless flag (setq command (read-string "Shell Command:")))
  (dc-docker-compose-run "up" flag))

;; shutdown your compose container
(defun dc-docker-compose-down ()
  (interactive)
  (dc-docker-compose-run "down" "" ""))

;; run a command on a docker container
(defun dc-docker-exec (name &optional command)
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (unless command (setq command (read-string "Shell Command:")))
  (dc-docker-run name "exec -it" command))

;; run a command on a compose container
(defun dc-docker-compose-exec (name &optional command)
  (interactive (list (read-string "Container name:") (read-string "Shell command:")))
  (unless command (setq command (read-string "Shell Command:")))
  (let ((bind_path (docker-compose-bound-project-path name)))
    (dc-docker-compose-run name "exec" command)))

;; return list of docker names
(defun dc-docker-names ()
  (interactive)
  (let ((container_ids (split-string (dc-docker-run-return "ps -q" "" "") "\n" t)))
    (loop for el in container_ids collect
      (substring (dc-docker-run-return el "inspect -f \"{{ .Name }}\"" "") 1 -1))))

;; return list of docker names
(defun dc-docker-compose-names ()
  (interactive)
  (let ((dotcomposefile (format "%sdocker-compose.yml" (dc-compose-root))))
    (if (file-exists-p dotcomposefile)
      (split-string (dc-docker-compose-run-return "" "config --services" "") "\n" t)
      (message "Missing compose file @%s" dotcomposefile)
      nil)))

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
          (action . (lambda (candidate)
                      (dc-docker-exec candidate)))))

  ;; helm source for docker compose containers
  (setq helm-docker-compose-containers
        '((name . "Docker Compose Containers")
          (candidates . dc-docker-compose-names)
          (action . (lambda (candidate)
                      (dc-docker-compose-exec candidate)))))

  (helm :sources '(helm-docker-compose-containers helm-docker-containers) :buffer "*helm container*"))

(defun dc-run-test (function_name)
  (interactive)
  (message "%s" "test")
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

(ert-deftest pp-test-docker-compose-container-names ()
  "Test compose container name lookup return values"
  (cl-letf (((symbol-function 'shell-command-to-string) (lambda (_) "")))
    (should-error (dc-docker-compose-names))))


(ert-deftest pp-test-docker-container-names ()
  "Test container name lookup return values"
  (cl-letf (((symbol-function 'shell-command-to-string) (lambda (_) "")))
    (should (equal (dc-docker-names) nil))))

(defhydra dc-launcher (:color blue :columns 4)
"
Docker Compose Menu
| Docker Compose                                | Docker |
|-----------------------------------------------|--------|
| _u_: Start Background | _U_: Start Foreground |
| _l_: Logs             | _L_: Logs realtime    |
"
  ("U" (dc-docker-compose-up) "Startup")
  ("u" (dc-docker-compose-up "-d") "Startup Background")
  ("d" (dc-docker-compose-down) "Shutdown")
  ("l" (dc-docker-compose-logs) "Logs")
  ("L" (dc-docker-compose-logs "-f") "Logs Realtime")
  ("e" (dc-docker-compose-exec) "Run command")
  ("q" nil "Quit"))

(global-set-key (kbd "C-c d") 'dc-launcher/body)
;;; docker-compose.el ends here
;;(provide 'docker-compose) 
