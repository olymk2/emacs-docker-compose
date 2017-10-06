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
(require 'dc-core)
(require 'dc-docker)
(require 'dc-docker-compose)
(require 'dc-tests)


;;;###autoload (autoload 'drone-options-popup "magit" nil t)
(magit-define-popup docker-compose-popup
  "Show popup buffer featuring tagging commands."
  'magit-commands
  :man-page "docker-compose"
  :options '((?f "Select file" "--file"))
  :actions  '((?u "up" docker-up-popup)
              (?b "ps" docker-up-popup)
              (?b "build" docker-build-popup)
              (?r "run" docker-run-popup))
  :default-action 'dc-docker-compose-up)


;;;###autoload (autoload 'drone-options-popup "magit" nil t)
(magit-define-popup docker-up-popup
  "Show popup buffer featuring tagging commands."
  'magit-commands
  :man-page "drone"
  :switches '((?d "daemon" "-d"))
  :actions  '((?u "up" dc-docker-compose-up))
  :default-action 'dc-docker-compose-up)

;;;###autoload (autoload 'drone-options-popup "magit" nil t)
(magit-define-popup docker-build-popup
  "Show popup buffer featuring tagging commands."
  'magit-commands
  :man-page "drone"
  :options '((?f "Select file" "--file"))
  :switches '((?t "No cache" "--no-cache"))
  :actions  '((?d "build" dc-docker-build)(?d "build compose" dc-docker-compose-build))
  :default-action 'dc-docker-build)

;;;###autoload (autoload 'drone-options-popup "magit" nil t)
(magit-define-popup docker-run-popup
  "Show popup buffer featuring tagging commands."
  'magit-commands
  :man-page "drone"
  :options '((?f "Select file" "--file"))
  :switches '((?t "Remove" "--rm"))
  :actions  '((?d "run" dc-docker-exec)(?c "run compose" dc-docker-compose-exec))
  :default-action 'drone-exec)


;;; (Features)
(provide 'dc-popups)
;;; dc-popup.el ends here

;; (load-file "dc-core.el")
;; (load-file "~/.emacs/dc/dc-core")
