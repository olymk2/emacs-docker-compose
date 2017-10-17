(load-file "dc-core.el")
(load-file "dc-docker-compose.el")


(ert-deftest pp-test-compose-container-names ()
  "Test compose container name lookup return values"
  (setq dc-current-buffer "./")
  (cl-letf (((symbol-function 'shell-command-to-string) (lambda (a) "")))
    (should (equal (dc-compose-names) nil))))

(ert-deftest pp-test-compose-names-returns ()
  "Test get composer container list, when compose file exists"
  (setq dc-current-buffer "./")
  (cl-letf
      (((symbol-function 'dc-compose-run-return)
        (lambda (a b &rest c) "container1\ncontainer2\ncontainer3\n")))
    (should (equal (dc-compose-names) (list "container1" "container2" "container3" )))))

(ert-deftest pp-test-compose-names-returns-when-no-container ()
  "Test get composer container list, when compose file exists"
  (setq dc-current-buffer "./")
  (cl-letf
      (((symbol-function 'dc-compose-run-return) (lambda (a b &rest c) "")))
    (should (equal (dc-compose-names) (list)))))

(ert-deftest pp-test-find-compose-path-in-current-folder ()
  "Test container name lookup return values"
  (setq dc-current-buffer "./tests/test_project/")
  (should (equal (not(string-match "\\tests/test_project\/$" (dc-compose-root))) nil)))  

(ert-deftest pp-test-find-compose-path-in-child-folder ()
  "Test container name lookup return values"
  (setq dc-current-buffer "./tests/test_project/child1/")
  (should (equal (not(string-match "\\tests/test_project\/$" (dc-compose-root))) nil)))


(ert-deftest pp-test-compose-process-function ()
  "Test process wrapper joins parameters together correctly"
  (setq dc-current-buffer "./tests/test_project/")
  (setq dc-compose-cmd "/usr/local/bin/docker-compose")
  (cl-letf (((symbol-function 'dc-process)
             (lambda (&rest params) params)))
    (should (equal
             (dc-compose-process "a" "b" "c")
             (list "*Docker Info*" "/usr/local/bin/docker-compose" "--file=docker-compose.yml" "a" "b" "c")))))
