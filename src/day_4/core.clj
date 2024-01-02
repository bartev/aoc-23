(ns day-4.core
  (:require
   [utils :refer [read-file-lines-slurp]]
   [clojure.string :as str]
   [clojure.edn :as edn]))

;; Scorecards on Island Island



(defn extract-digits [x]
  (re-seq #"\d+" x))

(defn ->lists [[winners my-nums]]
  {:winners (vec winners)
   :my-nums (vec my-nums)})

(defn parse-cards [fname]
  (->> (read-file-lines-slurp fname)
       ;; (#(map (fn [line] (str/split line #":")) %))
       (map (fn [x] (-> x
                        (str/split #":")
                        second
                        (str/split #"\|")
                        (#(mapv extract-digits %))
                        ->lists)))))


#_(defn common-items [lst1 lst2]
    (set (clojure.set/intersection (set lst1) (set lst2))))

(defn common-items-card [{:keys [winners my-nums]}]
  (set (clojure.set/intersection (set winners) (set my-nums))))


(def card {:winners ["41" "48" "83" "86" "17"],
           :my-nums ["83" "86" "6" "31" "17" "9" "48" "53"]})

(defn ->winnings [num-matches]
  (if (pos? num-matches)
    (Math/pow 2 (dec num-matches))
    0))

(defn part1 [fname]
  (->> (parse-cards fname)
       (map common-items-card)
       (map count)
       (mapv ->winnings)
       (apply +)
       ))

(part1 "input-sample.txt")
;; => 13.0
(part1 "input.txt")
;; => 25010.0


;;; Part 2
