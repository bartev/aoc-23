(ns day-5.core-3
  (:require
   [clojure.java.io :as io]
   [utils :refer [get-local-fname]]))

;; Copy from https://gist.github.com/alexander-yakushev/7f10ec2b18b504533447af0438179507

(def lines (vec (line-seq (io/reader (get-local-fname "input-sample.txt")))))
(def lines (vec (line-seq (io/reader (get-local-fname "input.txt")))))

(defn parse [task]
  (let [seeds (mapv parse-long (re-seq #"\d+" (first lines)))
        seeds (case task
                1 seeds
                2 (->> (partition 2 seeds)
                       (mapv (fn [[a b]] [a (+ a b)]))))
        maps (->> (partition-by empty? (rest lines))
                  (keep (fn [lines]
                          (->> lines
                               ;; drops the name line <src>-to-<dest>
                               ;; returns a list of [start end delta]
                               (keep #(when-let [[dst src n] (re-seq #"\d+" %)]
                                        (let [src (parse-long src)]
                                          [src
                                           (+ src (parse-long n))
                                           (- (parse-long dst) src)])))
                               (sort-by first)
                               seq))))]
    [seeds maps]))

;; (parse 1)
;; ;; => [[79 14 55 13]
;; ;;     (([50 98 2] [98 100 -48])
;; ;;      ([0 15 39] [15 52 -15] [52 54 -15])
;; ;;      ([0 7 42] [7 11 50] [11 53 -11] [53 61 -4])
;; ;;      ([18 25 70] [25 95 -7])
;; ;;      ([45 64 36] [64 77 4] [77 100 -32])
;; ;;      ([0 69 1] [69 70 -69])
;; ;;      ([56 93 4] [93 97 -37]))]

;; NIFTY!
;; At the partition-by empty? stage
;; ;; => (("")
;; ;;     ("seed-to-soil map:" "50 98 2" "52 50 48")
;; ;;     ("")
;; ;;     ("soil-to-fertilizer map:" "0 15 37" "37 52 2" "39 0 15")
;; ;;     ("")
;; ;;     ("fertilizer-to-water map:" "49 53 8" "0 11 42" "42 0 7" "57 7 4")
;; ;;     ("")
;; ;;     ("water-to-light map:" "88 18 7" "18 25 70")
;; ;;     ("")
;; ;;     ("light-to-temperature map:" "45 77 23" "81 45 19" "68 64 13")
;; ;;     ("")
;; ;;     ("temperature-to-humidity map:" "0 69 1" "1 0 69")
;; ;;     ("")
;; ;;     ("humidity-to-location map:" "60 56 37" "56 93 4"))
;; ;; => [79 14 55 13]

;; Only differece is the first element -- seeds
;; (parse 2)
;; ;; => [((79 14) (55 13))
;; ;;     (([50 98 2] [98 100 -48])
;; ;;      ([0 15 39] [15 52 -15] [52 54 -15])
;; ;;      ([0 7 42] [7 11 50] [11 53 -11] [53 61 -4])
;; ;;      ([18 25 70] [25 95 -7])
;; ;;      ([45 64 36] [64 77 4] [77 100 -32])
;; ;;      ([0 69 1] [69 70 -69])
;; ;;      ([56 93 4] [93 97 -37]))]

(defn convert [seed maps]
  (reduce (fn [seed m]
            (or (some (fn [[from to delta]]
                        (when (and (>= seed from) (< seed to))
                          (+ seed delta)))
                      m)
                seed))
          seed maps))

(defn task1 []
  (let [[seeds maps] (parse 1)]
    (->> seeds
         (map #(convert % maps))
         (apply min))))

(task1)
;; => 35
;; => 35
;; => 424490994
;; => 35

;;;

(defn transform-range [[from to] amap]
  (loop [start from, amap amap, res []]
    (if (>= start to)
      res ;; exit

      (let [amap (drop-while #(>= start (second %)) amap)
            [mfrom mto delta] (first amap)]
        (cond (or (nil? mfrom) (<= to mfrom))
              (recur to amap (conj res [start to]))

              (< start mfrom)
              (recur mfrom amap (conj res [start mfrom]))

              :else
              (let [to (min to mto)]
                (recur to amap (conj res [(+ start delta) (+ to delta)]))))))))

(defn task2 []
  (let [[seeds maps] (parse 2)]
    (ffirst
     (reduce (fn [ranges amap]
               (->> (mapcat #(transform-range % amap) ranges)
                    (sort-by first)))
             seeds
             maps))))

;; This version works super fast.
;; Why?
(task2)
;; => 46
;; => 15290096
