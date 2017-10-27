(load-file "../dc-popups-core.el")

(ert-deftest pp-test-compose-file-from-param ()
  "test getting parameter from string"
  (should (equal (dc-popups-core-fetch-dockerfile-from-param "-d --no-cache --filedev.yml") "dev.yml")))

(ert-deftest pp-test-compose-param-defaults-when-missing ()
  "test getting parameter from string returns default if missing"
  (should (equal (dc-popups-core-fetch-dockerfile-from-param "-d --no-cache") "docker-compose.yml")))

(ert-deftest pp-test-dockerfile-is-found ()
  "Test compose container name lookup return values"
  (cl-letf ((default-directory (expand-file-name "./test_project/")))
  (should (equal (dc-popups-core-dockerfile-exists-check) t))))

(ert-deftest pp-test-missing-dockerfile-errors ()
  "Test compose container name lookup return values"
  (should-error (dc-popups-core-dockerfile-exists-check)))

(ert-deftest pp-test-missing-compose-errors ()
  "Test compose file does not exists and errors"
  (setq dc-popups-current-buffer "/")
  (should-error (dc-popups-core-compose-exists-check)))

(ert-deftest pp-test-compose-is-found ()
  "Test compose file gets detected"
  (setq dc-popups-compose-file "docker-compose.yml")
  (setq dc-popups-current-buffer "./test_project/")
  (should (equal (dc-popups-core-compose-exists-check) t)))
