(ns day-1.core-test
  (:require
   [clojure.test :refer :all]
   [day-1.core :refer :all]))

(deftest extract-numbers-test
  (is (= 12 (extract-numbers "1abc2")))
  (is (= 38 (extract-numbers "pqr3stu8vwx")))
  (is (= 15 (extract-numbers "a1b2c3d4e5f")))
  (is (= 77 (extract-numbers "treb7uchet")))
  (is (= 84 (extract-numbers "four82nine74"))))

(deftest sum-calib-data-test
  (is (= 142 (sum-calib-data fname-sample-1))))

(deftest sum-calib-data-live-test
  (is (= 54877 (sum-calib-data fname-live))))

(deftest string->num-test
  (is (= "8y135" (string->num "eightyone3five"))))

#_(deftest string->num->extract-test
    (is (= 85 (string->num->extract "eightone3five"))))

#_(deftest sum-calib-data-pt-2-test
    (is (= 281 (sum-calib-data-pt-2 fname-sample-2))))

(deftest find-all-sub-indices-test
  (is (= [2 6]
         (find-all-sub-indices "twone3one" "one")))
  (is (= []
         (find-all-sub-indices "twone3one" "four"))))

(deftest indices->map-test
  (is (= [{:index 1 :value "one"}
          {:index 2 :value "one"}]
         (indices->map [1 2] "one")))
  (is (nil?
       (indices->map [] "one")))
  (is (nil?
       (indices->map nil "one"))))

(def data {"3" [5],
           "two" [0],
           "eight" [],
           "one" [2 6]})

#_(deftest data->dicts-test
    (is (= [{:index 5, :value "3"}
            {:index 0, :value "two"}
            {:index 2, :value "one"}
            [:index 6, :value "one"]]
           (data->dicts data))))

(deftest strings->sub-index-maps-test
  (is (= [{:index 0, :value "two"}
          {:index 2, :value "one"}
          {:index 5, :value "3"}
          {:index 6, :value "one"}]
         (strings->sub-index-maps "twone3one"))))

(deftest value->num-str-test
  (is (= {:index "idx" :value "1"}
         (value->num-str {:index "idx" :value "one"})))
  (is (= {:index "idx" :value "1"}
         (value->num-str {:index "idx" :value "1"})))
  (is (= {:index "idx" :value "foo"}
         (value->num-str {:index "idx" :value "foo"}))))

(deftest put-it-together-test
  (is (= 21 (put-it-together "twone3one")))
  (is (= 51 (put-it-together "5four3two1"))))

(deftest sum-calib-data-pt-2-test
  (is (= 281
         (sum-calib-data-pt-2 fname-sample-2))))
