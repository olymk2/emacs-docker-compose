(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-list '(ert hydra evil))

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (message "Installing %s" package)
    (package-install package)))
