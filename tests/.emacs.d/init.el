(require 'package)
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(add-to-list 'load-path (file-name-directory user-init-file))


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-list '(ert hydra evil evil-leader))
(package-initialize)

                                        ; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

                                        ; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (message "Installing %s" package)
    (package-install package)))

(message "%s" "testing init.el")
(require 'hydra)
(message "%s" "testing init.el")
