(load-file "docker-compose.el")

(ert-deftest pp-test-missing-docker-compose-errors ()
  "Test compose container name lookup return values"
  (should-error (dc-compose-exists-check)))

(ert-deftest pp-test-docker-compose-container-names ()
  "Test compose container name lookup return values"
  (cl-letf (((symbol-function 'shell-command-to-string) (lambda (a) "")))
    (should (equal (dc-docker-compose-names) nil))))

(ert-deftest pp-test-docker-container-names ()
  "Test container name lookup return values"
  (cl-letf (((symbol-function 'shell-command-to-string) (lambda (a) "" "")))
    (should (equal (dc-docker-names) nil))))

(ert-deftest pp-test-find-docker-compose-path-in-current-folder ()
  "Test container name lookup return values"
  (find-file "./tests/test_project/child1/child2/child2_test_file.txt")
  (should (equal (not(string-match "\\tests/test_project\/$" (dc-compose-root))) nil)))  

(ert-deftest pp-test-find-docker-compose-path-in-child-folder ()
  "Test container name lookup return values"
  (find-file "./tests/test_project/test_file.txt")
  (should (equal (not(string-match "\\tests/test_project\/$" (dc-compose-root))) nil)))


(ert-deftest pp-test-docker-names-returns-when-no-container ()
  "Test get container list"
  (cl-letf
      (((symbol-function 'dc-docker-run) (lambda (a b c) "deadd2867f59\n90ab2818c5f2a\nf68ce5a307d7"))
       ((symbol-function 'dc-docker-run-return) (lambda (a b c) ""))
       (should (equal (dc-docker-names) (list))))))

(ert-deftest pp-test-docker-names-returns ()
  "Test get container list"
  (cl-letf
      (((symbol-function 'dc-docker-run) (lambda (a b c) "deadd2867f59\n90ab2818c5f2a\nf68ce5a307d7"))
       ((symbol-function 'dc-docker-run-return)
        (lambda (a b c) "/container1\n")
        (lambda (a b c) "/container2\n")
        (lambda (a b c) "/container3\n"))
       (should (equal (dc-docker-names) (list "container3" "container3" "container3" ))))))

(ert-deftest pp-test-docker-compose-names-returns-when-no-container ()
  "Test get composer container list, when compose file exists"
  (find-file "./tests/test_project/test_file.txt")
  (cl-letf
    (((symbol-function 'dc-docker-compose-run-return) (lambda (a b c) "")))
    (should (equal (dc-docker-compose-names) (list)))))

(ert-deftest pp-test-docker-compose-names-returns ()
  "Test get composer container list, when compose file exists"
  (find-file "./tests/test_project/test_file.txt")
  (cl-letf
       (((symbol-function 'dc-docker-compose-run-return)
        (lambda (a b c) "container1\ncontainer2\ncontainer3\n")))
       (should (equal (dc-docker-compose-names) (list "container1" "container2" "container3" )))))
