;;; dc-popups-tests.el --- Take control of your docker containers -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Oliver Marks

;; Author: Oliver Marks <oly@digitaloctave.com>
;; URL: https://github.com/olymk2/emacs-docker
;; Keywords: Docker control magit popups tests
;; Version: 0.1
;; Created 13 October 2017
;; Package-Requires: ((magit "2.5"))

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

(require 'magit)

(require 'dc-popups-compose)

(defun dc-python-get-test-name ()
  "Find function nearest the cursor."
  (interactive)
  (re-search-backward "def ")
  (forward-word)
  (forward-char)
  (thing-at-point 'word))

(defun dc-phpunit-get-test-name ()
  "Find function nearest the cursor."
  (interactive)
  (re-search-backward "function ")
  (forward-word)
  (forward-char)
  (thing-at-point 'word))

(defun dc-test-file ()
  "Get current buffers filename and return relative to project root."
  (interactive)
  (replace-regexp-in-string (docker-compose-bound-project-path) "" buffer-file-name)
  )

;; adjust path to be relative to project root
(defun dc-get-current-test-file ()
  "Get current buffers filename and return relative to project root."
  (interactive)
  (file-relative-name buffer-file-name
                      (locate-dominating-file buffer-file-name "docker-compose.yml")))


(defun dc-popups-compose-python-test ()
  "Generate python test command."
  (interactive (list (read-string "Container name:")))
  ;;(dc-popups-docker-compose-exec "mhackspace_uwsgi" "import nose; nose.run()")pytest
  (message "docker-compose exec -it hackdev_django_1 sh -c \"./vendor/bin/phpunit --filter=%s ./%s\"" (dc-python-get-test-name) (dc-get-current-test-file))
  (dc-popups-docker-compose-exec "hackdev_django_1"
                          (format "%s" (concatenate 'string "sh -c \"./vendor/bin/phpunit --filter=" (dc-python-get-test-name) "./" (dc-get-current-test-file) "\""))))
  ;;(dc-popups-docker-compose-exec "mhackspace_uwsgi" "import nose; nose.run()")pytest

;; Assumes you enter into your project root and that phpunit exists in the vendor folder
(defun dc-popups-compose-php-test ()
  "Generate php test command and send to container."
  (dc-popups-docker-select-container)
  (let ((cmd
         (list "sh" "-c"
           (concat "\"./vendor/bin/phpunit --filter=" (dc-phpunit-get-test-name) " ./" (dc-get-current-test-file) "\"")
           )
          )
         )
    (message "dc-php-test %s %s" dc-popups-current-compose-container cmd)
    (apply 'dc-popups-compose-process (append (list "exec" dc-popups-current-compose-container) cmd))))



(provide 'dc-popups-tests)
;;; dc-popups-tests.el ends here
