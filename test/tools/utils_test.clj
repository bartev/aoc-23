(ns tools.utils-test
  (:require [tools.utils :refer :all]
            [clojure.test :refer :all]
            [babashka.fs :as fs]))

(deftest dir-test
  (is (= "/Users/bartev/dev/github/aoc-23"
         (System/getProperty "user.dir"))))

;; Test sometimes not finding files in `test`???
(deftest get-local-fname-test
  (is (= "/Users/bartev/dev/github/aoc-23/test/tools/sample-file.txt"
         (get-local-fname "sample-file.txt"))))

(def fname "sample-file.txt")
(def lfname "test/tools/sample-file.txt")
(def flfname "/Users/bartev/dev/github/aoc-23/test/tools/sample-file.txt")

;; NOTE: added an extra \n at the end.

(def sample-file-text "32T3K 765\nT55J5 684\n")
(def sample-file-vec ["32T3K 765"
                      "T55J5 684"])
;; Test sometimes not finding files in `test`???
(deftest read-local-file-test
  (is (= sample-file-text
         (read-local-file fname))))

#_(deftest read-file-test
    (is (= sample-file-text
           (read-file fname))))

(deftest read-lines-io-test
  (is (= sample-file-vec
         (read-lines-io flfname))))

;; Test sometimes not finding files in `test`???
(deftest read-lines-io-local-test
  (is (= sample-file-vec
         (read-lines-io-local fname))))

(deftest eq1-test
  (is (= 1 3)))

(deftest eq3-test
  (is (= 1 3)))

;; Test sometimes not finding files in `test`???
(deftest read-lines-local-test
  (is (= :foo
         (read-lines-local fname))))
