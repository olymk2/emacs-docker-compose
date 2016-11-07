;;; dc-popups.el --- Start, Stop, Build and run popups for docker and docker compose -*- lexical-binding: t; -*-

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

(require 'magit)
(require 'dc-core)
(require 'dc-docker)
(require 'dc-docker-compose)
(require 'dc-tests)
(require 'dc-helm)

(setq dc-use-helm t)


;;;###autoload (autoload 'dc-docker-tests-popup "magit" nil t)
(magit-define-popup dc-docker-tests-popup
  "Show popup buffer featuring testing commands."
  'magit-commands
  :man-page "docker"
  :actions  '((?h "php" dc-call-compose-test-php)
              (?p "python" dc-call-compose-test-python))
  :default-action 'dc-call-compose-test-php)

;;;###autoload (autoload 'dc-compose-tests-popup "magit" nil t)
(magit-define-popup dc-compose-tests-popup
  "Show popup buffer to launch tests in container."
  'magit-commands
  :man-page "docker-compose"
  :actions  '((?h "php" dc-call-compose-test-php)
              (?p "python" dc-call-compose-test-python))
  :default-action 'dc-call-compose-test-php)

;;;###autoload (autoload 'dc-docker-up-popup "magit" nil t)
(magit-define-popup dc-docker-up-popup
  "Show popup to launch or stop containers."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?d "daemon" "-d")(?r "Remove orphaned containers" "--remove-orphans"))
  :actions  '((?u "up" dc-call-docker-up)
              (?d "down" dc-call-docker-down))
  :default-action 'dc-call-docker-up)

;;;###autoload (autoload 'dc-compose-up-popup "magit" nil t)
(magit-define-popup dc-compose-up-popup
  "Show popup to launch or stop containers."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?d "daemon" "-d")(?r "Remove orphaned containers" "--remove-orphans"))
  :actions  '((?u "up" dc-call-compose-up)
              (?d "down" dc-call-compose-down))
  :default-action 'dc-call-compose-up)

;;;###autoload (autoload 'dc-docker-build-popup "magit" nil t)
(magit-define-popup dc-docker-build-popup
  "Show popup buffer featuring build commands."
  'magit-commands
  :man-page "docker"
  :switches '((?c "Do not use cache" "--no-cache")
              (?r "Remove intermediate containers" "--force-rm")
              (?p "Pull newer container" "--pull"))
  :actions  '((?b "build" dc-docker-build))
  :default-action 'dc-docker-build)

;;;###autoload (autoload 'dc-compose-build-popup "magit" nil t)
(magit-define-popup dc-compose-build-popup
  "Show popup buffer featuring build commands."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?c "Do not use cache" "--no-cache")
              (?r "Remove intermediate containers" "--force-rm")
              (?p "Pull newer container" "--pull"))
  :actions  '((?b "build" dc-compose-build))
  :default-action 'dc-compose-build)


;;;###autoload (autoload 'dc-docker-ps-popup "magit" nil t)
(magit-define-popup dc-docker-ps-popup
  "Show popup buffer to list running containers."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?t "No cache" "--no-cache"))
  :actions  '((?p "ps" dc-call-docker-ps))
  :default-action 'dc-call-docker-ps)

;;;###autoload (autoload 'dc-compose-ps-popup "magit" nil t)
(magit-define-popup dc-compose-ps-popup
  "Show popup buffer to list project compose containers."
  'magit-commands
  :man-page "docker-compose"
  :switches '((?t "No cache" "--no-cache"))
  :actions  '((?p "ps" dc-call-compose-ps))
  :default-action 'dc-call-compose-ps)


;;;###autoload (autoload 'dc-docker-logs-popup "magit" nil t)
(magit-define-popup dc-docker-logs-popup
  "Show popup buffer to display logs."
  'magit-commands
  :man-page "docker"
  :options '((?n "Number of lines" "--tail"))
  :switches '((?f "follow" "-f")(?t "timestamps" "-t"))
  :actions  '((?l "logs" dc-call-docker-logs))
  :default-action 'dc-call-docker-logs)

;;;###autoload (autoload 'dc-compose-logs-popup "magit" nil t)
(magit-define-popup dc-compose-logs-popup
  "Show popup buffer to display logs."
  'magit-commands
  :man-page "docker-compose"
  :options '((?n "Number of lines" "--tail"))
  :switches '((?f "follow" "-f")(?t "timestamps" "-t"))
  :actions  '((?l "logs" dc-call-compose-logs))
  :default-action 'dc-call-compose-logs)

;;;###autoload (autoload 'dc-compose-run-popup "magit" nil t)
(magit-define-popup dc-compose-run-popup
  "Show buffer featuring compose run commands"
  'magit-commands
  :man-page "docker-compose"
  :switches '((?t "Remove" "--rm"))
  :actions  '((?d "run" dc-call-compose-exec)(?c "run compose" dc-call-compose-exec)))

;;;###autoload (autoload 'dc-docker-run-popup "magit" nil t)
(magit-define-popup dc-docker-run-popup
  "Show buffer featuring docker run commands"
  'magit-commands
  :man-page "docker"
  :switches '((?t "Remove" "--rm"))
  :actions  '((?d "run" dc-call-docker-run)(?c "run compose" dc-call-docker-exec)))

;;;###autoload (autoload 'dc-compose-popup "magit" nil t)
(magit-define-popup dc-compose-popup
  "Show root docker compose commands."
  'magit-commands
  :man-page "docker-compose"
  :options '((?f "Select file" "--file")(?p "Project name" "--project-name"))
  :actions  '((?p "ps" dc-show-compose-ps-popup)
              (?u "up" dc-show-compose-up-popup)
              (?t "test" dc-show-compose-tests-popup)
              (?b "build" dc-show-compose-build-popup)
              (?l "logs" dc-show-compose-logs-popup)
              (?e "exec" dc-show-compose-run-popup)))


;;;###autoload (autoload 'dc-docker-popup "magit" nil t)
(magit-define-popup dc-docker-popup
  "Show root docker commands."
  'magit-commands
  :man-page "docker-compose"
  :options '((?c "Select file" "--config"))
  :switches '((?t "Remove" "--rm"))
  :actions  '((?p "ps" dc-show-docker-ps-popup)
              (?u "up" dc-show-docker-up-popup)
              (?t "test" dc-show-docker-tests-popup)
              (?b "build" dc-show-docker-build-popup)
              (?l "logs" dc-show-docker-logs-popup)
              (?e "exec" dc-show-docker-run-popup)))


(defun dc-main-popup-close ()
  (interactive)
  (message "dc-main-popup-close%s"  (mapconcat 'identity magit-current-popup-args " "))
  (message "dc-main-popup-close%s" dc-current-buffer)
  (dc-fetch-dockerfile-from-param  (mapconcat 'identity magit-current-popup-args " "))
  (magit-popup-quit))

;; popup function call wrappers, simpler to pass a param from popup if possible
(defun dc-call-docker-ps () (interactive) (dc-docker-ps))
(defun dc-call-docker-up () (interactive) (dc-docker-up))
(defun dc-call-docker-down () (interactive) (dc-docker-down))
(defun dc-call-docker-logs () (interactive) (dc-docker-logs))
(defun dc-call-docker-build () (interactive) (dc-docker-build))
(defun dc-call-docker-test-php () (interactive) (dc-compose-php-test))
(defun dc-call-docker-test-python () (interactive) (dc-compose-python-test))
(defun dc-call-docker-run () (interactive) (dc-docker-run))

;; popup function call wrappers, simpler to pass a param from popup if possible
(defun dc-call-compose-ps () (interactive) (dc-compose-ps))
(defun dc-call-compose-up () (interactive) (dc-compose-up))
(defun dc-call-compose-down () (interactive) (dc-compose-down))
(defun dc-call-compose-logs () (interactive) (dc-compose-logs))
(defun dc-call-compose-build () (interactive) (dc-compose-build))
(defun dc-call-compose-test-php () (interactive) (dc-compose-php-test))
(defun dc-call-compose-test-python () (interactive) (dc-compose-python-test))
(defun dc-call-compose-run () (interactive) (dc-compose-run))



;; simple popup wrapper functions for docker
(defun dc-show-docker-ps-popup () (interactive) (dc-main-popup-close) (dc-docker-ps-popup))
(defun dc-show-docker-up-popup () (interactive) (dc-main-popup-close) (dc-docker-up-popup))
(defun dc-show-docker-run-popup () (interactive) (dc-main-popup-close) (dc-docker-run-popup))
(defun dc-show-docker-logs-popup () (interactive) (dc-main-popup-close) (dc-docker-logs-popup))
(defun dc-show-docker-build-popup () (interactive) (dc-main-popup-close) (dc-docker-build-popup))
(defun dc-show-docker-tests-popup () (interactive) (dc-main-popup-close) (dc-docker-tests-popup))

;; simple popup wrapper functions for compose
(defun dc-show-compose-ps-popup () (interactive) (dc-main-popup-close) (dc-compose-ps-popup))
(defun dc-show-compose-up-popup () (interactive) (dc-main-popup-close) (dc-compose-up-popup))
(defun dc-show-compose-run-popup () (interactive) (dc-main-popup-close) (dc-compose-run-popup))
(defun dc-show-compose-logs-popup () (interactive) (dc-main-popup-close) (dc-compose-logs-popup))
(defun dc-show-compose-build-popup () (interactive) (dc-main-popup-close) (dc-compose-build-popup))
(defun dc-show-compose-tests-popup () (interactive) (dc-main-popup-close) (dc-compose-tests-popup))

;; (defun dc-show-popup (popup)
;;   (interactive)
;;   (magit-popup-quit)
;;   (funcall popup))

(defun dc-main-docker-compose-popup ()
  (interactive)
  (setq dc-current-buffer buffer-file-name)
  (dc-compose-popup))


(defun dc-main-docker-popup ()
  (interactive)
  (setq dc-current-buffer buffer-file-name)
  (dc-docker-popup))


;;; (Features)
(provide 'dc-popups)
;;; dc-popup.el ends here
