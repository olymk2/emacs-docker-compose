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

(require 'magit)
(require 'dc-docker-compose)

(defun dc-python-get-test-name ()
  (interactive)
  (re-search-backward "def ")
  (forward-word)
  (forward-char)
  (thing-at-point 'word))

(defun dc-phpunit-get-test-name ()
  (interactive)
  (re-search-backward "function ")
  (forward-word)
  (forward-char)
  (thing-at-point 'word))

(defun dc-test-file ()
  (interactive)
  (replace-regexp-in-string (docker-compose-bound-project-path) "" buffer-file-name)
  )

;; adjust path to be relative to project root
(defun dc-get-current-test-file ()
  (interactive)
  (file-relative-name buffer-file-name
                      (locate-dominating-file buffer-file-name "docker-compose.yml")))

(defun dc-run-test (function_name)
  (interactive)
  (message "%s" function_name)
)


(defun dc-python-test ()
  (interactive (list (read-string "Container name:")))
  ;;(dc-docker-compose-exec "mhackspace_uwsgi" "import nose; nose.run()")pytest
  (message "docker-compose exec -it hackdev_django_1 sh -c \"./vendor/bin/phpunit --filter=%s ./%s\"" (dc-python-get-test-name) (dc-get-current-test-file))
  (dc-docker-compose-exec "hackdev_django_1"
                          (format "%s" (concatenate 'string "sh -c \"./vendor/bin/phpunit --filter=" (dc-python-get-test-name) "./" (dc-get-current-test-file) "\""))))
  ;;(dc-docker-compose-exec "mhackspace_uwsgi" "import nose; nose.run()")pytest

;; Assumes you enter into your project root and that phpunit exists in the vendor folder
(defun dc-php-test (container_name)
  (interactive (list (read-string "Container name:")))
  (let ((cmd
         (list "sh" "-c"
           (concat "\"./vendor/bin/phpunit --filter=" (dc-phpunit-get-test-name) " ./" (dc-get-current-test-file) "\"")
           )
          )
         )
    (message "dc-php-test %s %s" container_name cmd)
    (dc-docker-compose-exec container_name cmd)))



(provide 'dc-tests)
;;; dc-popup.el ends here
