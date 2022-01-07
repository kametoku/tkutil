(defpackage tkutil/tests/main
  (:use :cl
        :tkutil
        :rove))
(in-package :tkutil/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :tkutil)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
