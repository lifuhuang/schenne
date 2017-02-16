(load "environment-tests.scm")
(load "table-tests.scm")

(newline)
(test-table)
(write "Passed table tests.")

(newline)
(test-frame)
(write "Passed frame tests.")

(newline)
(test-environment)
(write "Passed environment tests.")
