##! emacs-lisp
(setq package-user-dir
      (concat
       (file-name-directory
        (or (buffer-file-name)
            load-file-name
            default-directory))
       ".elpa"))
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(package-refresh-contents)
(require 'cl)
(load-file "../dc-core.el")
(load-file "../dc-docker.el")
(load-file "../dc-compose.el")

(ert-run-tests-batch-and-exit)
