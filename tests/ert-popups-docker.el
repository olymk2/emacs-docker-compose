;;; ert-popups-docker.el --- test

;;; Commentary:
;; tests for docker functions

;;; Code:

(load-file "../dc-popups-core.el")
(load-file "../dc-popups-docker.el")



(ert-deftest pp-test-docker-names-returns-when-no-container ()
  "Test get container list"
  (setq dc-popups-current-buffer "/")
  (cl-letf
      (((symbol-function 'dc-popups-docker-run-return) (lambda (a b c) ""))
       (should (equal (dc-popups-docker-names) (list))))))

(ert-deftest pp-test-docker-names-returns ()
  "Test get container list"
  (setq dc-popups-current-buffer "/")
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (&rest params) "container1\ncontainer2\ncontainer3\n")
             (lambda (&rest params) (list "/container1" "/container2" "/container3")))
            (should
             (equal
              (dc-popups-docker-names)
              (list "container1" "container2" "container3" ))))))

(ert-deftest pp-test-docker-process-function ()
  "Test process wrapper joins parameters together correctly"
  (setq dc-popups-current-buffer "/")
  (setq dc-popups-docker-cmd "/usr/bin/docker")
  (cl-letf (((symbol-function 'dc-popups-core-process)
             (lambda (&rest params) params)))
    (should (equal
             (dc-popups-docker-process "a" "b" "c")
             (list "*Docker Info*" "/usr/bin/docker" "a" "b" "c")))))


(provide 'ert-docker)

;;; ert-docker.el ends here
