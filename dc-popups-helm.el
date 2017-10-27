;;; dc-popups-helm.el --- Take control of your docker containers -*- lexical-binding: t; -*-

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


(defun dc-popups-docker-select-container ()
  "Wrapper to select container, should fall back if helm is not available."
  (interactive)
  (dc-popups-helm-select-container))

(defun dc-popups-helm-set-docker-container (container_name)
  "Set current container for future commands.
CONTAINER_NAME        -- name of container"
  (message "dc-popups-helm-set-docker-container %s" container_name)
  (setq dc-popups-current-docker-container container_name))

(defun dc-popups-helm-set-compose-container (container_name)
  "Set current compose container for future commands.
CONTAINER_NAME        -- name of container"
  (message "dc-popups-helm-set-docker-compose-container %s" container_name)
  (setq dc-popups-current-compose-container container_name))

;; show helm view to select container by name
(defun dc-popups-helm-select-container ()
  "Build and open the helm container selection."
  (interactive)
  (setq helm-docker-containers
        '((name . "Docker Containers")
          (candidates . dc-popups-docker-names)
          (action . (("Run command inside container" . (lambda (candidate)
                                                         (dc-popups-helm-set-docker-container candidate)))
                     ("Alternate command" . (lambda (candiadte) () ))))))

  ;; helm source for docker compose containers
  (setq helm-compose-containers
        '((name . "Docker Compose Containers")
          (candidates . dc-popups-compose-names)
          (action . (lambda (candidate)
                      (dc-popups-helm-set-compose-container candidate)))))

  (helm :sources '(helm-compose-containers helm-docker-containers) :buffer "*helm container*"))

;;; (Features)
(provide 'dc-popups-helm)
;;; dc-popups-helm.el ends here
