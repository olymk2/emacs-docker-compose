;;; dc-popups.el --- Start, Stop, Build and run docker containers -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Oliver Marks

;; Author: Oliver Marks <oly@digitaloctave.com>
;; URL: https://github.com/olymk2/emacs-docker
;; Keywords: Processes tools
;; Version: 0.1
;; Created 13 October 2017
;; Package-Requires: ((magit "2.5")(helm "2.5")(emacs "24.3"))

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
(require 'dc-popups-core)
(require 'dc-popups-docker)
(require 'dc-popups-compose)
(require 'dc-popups-tests)
(require 'dc-popups-helm)

(setq dc-use-helm t)


;;;###autoload (autoload 'dc-popups-docker-tests-popup "magit" nil t)
(magit-define-popup dc-popups-docker-tests-popup
  "Show popup buffer featuring testing commands."
  'magit-commands
  :man-page "docker"
  :actions  '((?h "php" dc-popups-call-compose-test-php)
              (?p "python" dc-popups-call-compose-test-python))
  :default-action 'dc-popups-call-compose-test-php)

;;;###autoload (autoload 'dc-popups-compose-tests-popup "magit" nil t)
(magit-define-popup dc-popups-compose-tests-popup
  "Show popup buffer to launch tests in container."
  'magit-commands
  :man-page "docker-compose"
  :actions  '((?h "php" dc-popups-call-compose-test-php)
              (?p "python" dc-popups-call-compose-test-python))
  :default-action 'dc-popups-call-compose-test-php)

;;;###autoload (autoload 'dc-popups-docker-up-popup "magit" nil t)
(magit-define-popup dc-popups-docker-up-popup
  "Show popup to launch or stop containers."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?d "daemon" "-d")(?r "Remove orphaned containers" "--remove-orphans"))
  :actions  '((?u "up" dc-popups-call-docker-up)
              (?d "down" dc-popups-call-docker-down))
  :default-action 'dc-popups-call-docker-up)

;;;###autoload (autoload 'dc-popups-compose-up-popup "magit" nil t)
(magit-define-popup dc-popups-compose-up-popup
  "Show popup to launch or stop containers."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?d "daemon" "-d")(?r "Remove orphaned containers" "--remove-orphans"))
  :actions  '((?u "up" dc-popups-call-compose-up)
              (?d "down" dc-popups-call-compose-down))
  :default-action 'dc-popups-call-compose-up)

;;;###autoload (autoload 'dc-popups-docker-build-popup "magit" nil t)
(magit-define-popup dc-popups-docker-build-popup
  "Show popup buffer featuring build commands."
  'magit-commands
  :man-page "docker"
  :switches '((?c "Do not use cache" "--no-cache")
              (?r "Remove intermediate containers" "--force-rm")
              (?p "Pull newer container" "--pull"))
  :actions  '((?b "build" dc-popups-docker-build))
  :default-action 'dc-popups-docker-build)

;;;###autoload (autoload 'dc-popups-compose-build-popup "magit" nil t)
(magit-define-popup dc-popups-compose-build-popup
  "Show popup buffer featuring build commands."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?c "Do not use cache" "--no-cache")
              (?r "Remove intermediate containers" "--force-rm")
              (?p "Pull newer container" "--pull"))
  :actions  '((?b "build" dc-popups-compose-build))
  :default-action 'dc-popups-compose-build)


;;;###autoload (autoload 'dc-popups-docker-ps-popup "magit" nil t)
(magit-define-popup dc-popups-docker-ps-popup
  "Show popup buffer to list running containers."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?t "No cache" "--no-cache"))
  :actions  '((?p "ps" dc-popups-call-docker-ps))
  :default-action 'dc-popups-call-docker-ps)

;;;###autoload (autoload 'dc-popups-compose-ps-popup "magit" nil t)
(magit-define-popup dc-popups-compose-ps-popup
  "Show popup buffer to list project compose containers."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?t "No cache" "--no-cache"))
  :actions  '((?p "ps" dc-popups-call-compose-ps))
  :default-action 'dc-popups-call-compose-ps)


;;;###autoload (autoload 'dc-popups-docker-logs-popup "magit" nil t)
(magit-define-popup dc-popups-docker-logs-popup
  "Show popup buffer to display logs."
  'magit-commands
  :man-page "docker"
  :options '((?n "Number of lines" "--tail"))
  :switches '((?f "follow" "-f")(?t "timestamps" "-t"))
  :actions  '((?l "logs" dc-popups-call-docker-logs))
  :default-action 'dc-popups-call-docker-logs)

;;;###autoload (autoload 'dc-popups-compose-logs-popup "magit" nil t)
(magit-define-popup dc-popups-compose-logs-popup
  "Show popup buffer to display logs."
  'magit-commands
  :man-page "docker-compose"
  :options '((?n "Number of lines" "--tail"))
  :switches '((?f "follow" "-f")(?t "timestamps" "-t"))
  :actions  '((?l "logs" dc-popups-call-compose-logs))
  :default-action 'dc-popups-call-compose-logs)

;;;###autoload (autoload 'dc-popups-compose-run-popup "magit" nil t)
(magit-define-popup dc-popups-compose-run-popup
  "Show buffer featuring compose run commands"
  'magit-commands
  :man-page "docker-compose"
  :switches '((?t "Remove" "--rm"))
  :actions  '((?d "run" dc-popups-call-compose-exec)(?c "run compose" dc-popups-call-compose-exec)))

;;;###autoload (autoload 'dc-popups-docker-run-popup "magit" nil t)
(magit-define-popup dc-popups-docker-run-popup
  "Show buffer featuring docker run commands"
  'magit-commands
  :man-page "docker"
  :switches '((?t "Remove" "--rm"))
  :actions  '((?d "run" dc-popups-call-docker-run)(?c "run compose" dc-popups-call-docker-exec)))

;;;###autoload (autoload 'dc-popups-compose-popup "magit" nil t)
(magit-define-popup dc-popups-compose-popup
  "Show root docker compose commands."
  'magit-commands
  :man-page "docker-compose"
  :options '((?f "Select file" "--file")(?p "Project name" "--project-name"))
  :actions  '((?p "ps" dc-popups-show-compose-ps-popup)
              (?u "up" dc-popups-show-compose-up-popup)
              (?t "test" dc-popups-show-compose-tests-popup)
              (?b "build" dc-popups-show-compose-build-popup)
              (?l "logs" dc-popups-show-compose-logs-popup)
              (?e "exec" dc-popups-show-compose-run-popup)))


;;;###autoload (autoload 'dc-popups-docker-popup "magit" nil t)
(magit-define-popup dc-popups-docker-popup
  "Show root docker commands."
  'magit-commands
  :man-page "docker-compose"
  :options '((?c "Select file" "--config"))
  :switches '((?t "Remove" "--rm"))
  :actions  '((?p "ps" dc-popups-show-docker-ps-popup)
              (?u "up" dc-popups-show-docker-up-popup)
              (?t "test" dc-popups-show-docker-tests-popup)
              (?b "build" dc-popups-show-docker-build-popup)
              (?l "logs" dc-popups-show-docker-logs-popup)
              (?e "exec" dc-popups-show-docker-run-popup)))


(defun dc-popups-close ()
  "Close the current popup before displaying the next."
  (interactive)
  (message "dc-popups-close%s"  (mapconcat 'identity magit-current-popup-args " "))
  (message "dc-popups-close%s" dc-popups-current-buffer)
  (dc-popups-core-fetch-dockerfile-from-param  (mapconcat 'identity magit-current-popup-args " "))
  (magit-popup-quit))

;; popup function call wrappers, simpler to pass a param from popup if possible
(defun dc-popups-call-docker-ps () (interactive) (dc-popups-docker-ps))
(defun dc-popups-call-docker-up () (interactive) (dc-popups-docker-up))
(defun dc-popups-call-docker-down () (interactive) (dc-popups-docker-down))
(defun dc-popups-call-docker-logs () (interactive) (dc-popups-docker-logs))
(defun dc-popups-call-docker-build () (interactive) (dc-popups-docker-build))
(defun dc-popups-call-docker-test-php () (interactive) (dc-popups-compose-php-test))
(defun dc-popups-call-docker-test-python () (interactive) (dc-popups-compose-python-test))
(defun dc-popups-call-docker-run () (interactive) (dc-popups-docker-run))

;; popup function call wrappers, simpler to pass a param from popup if possible
(defun dc-popups-call-compose-ps () (interactive) (dc-popups-compose-ps))
(defun dc-popups-call-compose-up () (interactive) (dc-popups-compose-up))
(defun dc-popups-call-compose-down () (interactive) (dc-popups-compose-down))
(defun dc-popups-call-compose-logs () (interactive) (dc-popups-compose-logs))
(defun dc-popups-call-compose-build () (interactive) (dc-popups-compose-build))
(defun dc-popups-call-compose-test-php () (interactive) (dc-popups-compose-php-test))
(defun dc-popups-call-compose-test-python () (interactive) (dc-popups-compose-python-test))
(defun dc-popups-call-compose-run () (interactive) (dc-popups-compose-run))

;; simple popup wrapper functions for docker
(defun dc-popups-show-docker-ps-popup () (interactive) (dc-popups-close) (dc-popups-docker-ps-popup))
(defun dc-popups-show-docker-up-popup () (interactive) (dc-popups-close) (dc-popups-docker-up-popup))
(defun dc-popups-show-docker-run-popup () (interactive) (dc-popups-close) (dc-popups-docker-run-popup))
(defun dc-popups-show-docker-logs-popup () (interactive) (dc-popups-close) (dc-popups-docker-logs-popup))
(defun dc-popups-show-docker-build-popup () (interactive) (dc-popups-close) (dc-popups-docker-build-popup))
(defun dc-popups-show-docker-tests-popup () (interactive) (dc-popups-close) (dc-popups-docker-tests-popup))

;; simple popup wrapper functions for compose
(defun dc-popups-show-compose-ps-popup () (interactive) (dc-popups-close) (dc-popups-compose-ps-popup))
(defun dc-popups-show-compose-up-popup () (interactive) (dc-popups-close) (dc-popups-compose-up-popup))
(defun dc-popups-show-compose-run-popup () (interactive) (dc-popups-close) (dc-popups-compose-run-popup))
(defun dc-popups-show-compose-logs-popup () (interactive) (dc-popups-close) (dc-popups-compose-logs-popup))
(defun dc-popups-show-compose-build-popup () (interactive) (dc-popups-close) (dc-popups-compose-build-popup))
(defun dc-popups-show-compose-tests-popup () (interactive) (dc-popups-close) (dc-popups-compose-tests-popup))


(defun dc-popups-docker-compose ()
  "Show the compose popup."
  (interactive)
  (setq dc-popups-current-buffer buffer-file-name)
  (dc-popups-compose-popup))


(defun dc-popups-docker ()
  "Show the docker popup."
  (interactive)
  (setq dc-popups-current-buffer buffer-file-name)
  (dc-popups-docker-popup))


;;; (Features)
(provide 'dc-popups)
;;; dc-popups.el ends here
