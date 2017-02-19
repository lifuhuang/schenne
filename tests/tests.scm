(load "tests/environment-tests.scm")
(load "tests/table-tests.scm")

(newline)
(test-table)
(write "Passed table tests.")

(newline)
(test-frame)
(write "Passed frame tests.")

(newline)
(test-environment)
(write "Passed environment tests.")
