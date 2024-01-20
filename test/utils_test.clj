(ns utils-test
  (:require [utils :refer :all]
            [clojure.test :refer :all]))

(deftest extract-digits-as-nums-test
  (is (= [6 12 344]
         (extract-digits-as-nums " 06 12 344 ")))
  (is (= [6 12 344]
         (extract-digits-as-nums " 06 12 foo 344 ")))
  (is (= [6 12 34 4]
         (extract-digits-as-nums " 06 12 34.4 "))))
