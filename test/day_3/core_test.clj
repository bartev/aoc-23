(ns day-3.core-test
  (:require [day-3.core :refer :all]
            [clojure.test :refer :all]))

(deftest find-start-indices-test
  (is (= [1 6]
         (find-start-indices ".#..0.$2.6." regex-symbol)))
  (is (= [0 6]
         (find-start-indices "eightwone" #"eight|two|one")))
  (is (= [1 6 7 8]
         (find-start-indices ".#..0.$^&2.6." regex-symbol))))

(deftest find-start-end-indices-test
  (is (= []
         (find-start-end-indices "" regex-number)))
  (is (= [{:match "123", :start 0, :end 3}]
         (find-start-end-indices "123" regex-number)))
  (is (= [{:match "123", :start 0, :end 3} ; notice end is +1
          {:match "4", :start 4, :end 5}
          {:match "56", :start 6, :end 8}]
         (find-start-end-indices "123.4^56." regex-number))))

(deftest mapify-row-test
  (is (= {:numbers [] :symbols []}
         (mapify-row "")))
  (is (= {:numbers [] :symbols []}
         (mapify-row ".")))
  (is (= {:numbers [{:match "12" :start 0 :end 2}]
          :symbols []}
         (mapify-row "12")))
  (is (= {:numbers [] :symbols [1]}
         (mapify-row ".$.")))
  (is (= {:numbers [{:match "12" :start 1 :end 3}
                    {:match "3" :start 4 :end 5}]
          :symbols [3 5]}
         (mapify-row ".12$3^."))))

(deftest combine-sort-vectors-test
  (is (= [1 2 3 4]
         (combine-sort-vectors [1] [1 4] [3 4] [2 1 3]))))

(def sample-inputs
  ["467..114.."
   "...*......"
   "..35..633."
   "......#..."
   "617*......"
   ".....+.58."
   "..592....."
   "......755."
   "...$.*...."
   ".664.598.."]
  )

(deftest combine-before-after-symbol-indices-test
  (let [row-maps (-> sample-inputs
                     (#(mapv mapify-row %)))]
    (is (= [{:numbers
             [{:match "467", :start 0, :end 3}
              {:match "114", :start 5, :end 8}],
             :symbols []}
            {:numbers [], :symbols [3]}
            {:numbers [{:match "35", :start 2, :end 4}
                       {:match "633", :start 6, :end 9}],
             :symbols []} {:numbers [], :symbols [6]}
            {:numbers [{:match "617", :start 0, :end 3}],
             :symbols [3]}
            {:numbers [{:match "58", :start 7, :end 9}],
             :symbols [5]}
            {:numbers [{:match "592", :start 2, :end 5}],
             :symbols []}
            {:numbers [{:match "755", :start 6, :end 9}],
             :symbols []}
            {:numbers [], :symbols [3 5]}
            {:numbers [{:match "664", :start 1, :end 4}
                       {:match "598", :start 5, :end 8}],
             :symbols []}]
           row-maps))

    (is (= [{:numbers [{:match "467", :start 0, :end 3}
                       {:match "114", :start 5, :end 8}]
             :symbols [], :indices [3]}

            {:numbers [],
             :symbols [3], :indices [3]}

            {:numbers [{:match "35", :start 2, :end 4}
                       {:match "633", :start 6, :end 9}],
             :symbols [], :indices [3 6]}

            {:numbers [],
             :symbols [6], :indices [3 6]}

            {:numbers [{:match "617", :start 0, :end 3}],
             :symbols [3], :indices [3 5 6]}

            {:numbers [{:match "58", :start 7, :end 9}]
             :symbols [5], :indices [3 5]}

            {:numbers [{:match "592", :start 2, :end 5}]
             :symbols [], :indices [5]}

            {:numbers [{:match "755", :start 6, :end 9}]
             :symbols [], :indices [3 5]}

            {:numbers [],
             :symbols [3 5], :indices [3 5]}

            {:numbers [{:match "664", :start 1, :end 4}
                       {:match "598", :start 5, :end 8}]
             :symbols [], :indices [3 5]}]
           (combine-before-after-symbol-indices row-maps))))

  (let [inputs [{:symbols []}
                {:symbols [1]}
                {:symbols [2]}]]
    (is (= [{:symbols [] :indices [1]}
            {:symbols [1] :indices [1 2]}
            {:symbols [2] :indices [1 2]}]
           (combine-before-after-symbol-indices inputs))))

  (let [inputs [{:numbers [{:match "467", :start 0, :end 3}
                           {:match "114", :start 5, :end 8}],
                 :symbols []}
                {:symbols [3]}
                {:symbols []}
                {:symbols [6]}
                {:symbols [3]}
                {:symbols [5]}
                {:symbols []}
                {:symbols []}
                {:symbols [3 5]}]]
    (is (= [{:numbers [{:match "467", :start 0, :end 3} {:match "114", :start 5, :end 8}],
             :symbols [], :indices [3]}
            {:symbols [3], :indices [3]}
            {:symbols [], :indices [3 6]}
            {:symbols [6], :indices [3 6]}
            {:symbols [3], :indices [3 5 6]}
            {:symbols [5], :indices [3 5]}
            {:symbols [], :indices [5]}
            {:symbols [], :indices [3 5]}
            {:symbols [3 5], :indices [3 5]}]
           (combine-before-after-symbol-indices inputs)))))

(deftest get-number-if-overlap-test
  (let [number-map {:match "467", :start 0, :end 3}
        indices [4 5 3]]
    (is (= 467
           (get-number-if-overlap number-map indices))))

  (let [number-map {:match "467", :start 0, :end 3}
        indices [4 5 4]]
    (is (= nil
           (get-number-if-overlap number-map indices)))))

(deftest check-for-overlaps-test
  (let [numbers [{:match "467", :start 0, :end 3}
                 {:match "114", :start 5, :end 8}]]
    ;; No indices
    (is (= []
           (check-for-overlaps {:numbers numbers :indices []})))
    (is (= []
           (check-for-overlaps {:numbers numbers :indices nil})))
    ;; No numbers
    (is (= []
           (check-for-overlaps {:numbers nil :indices []})))
    ;; 1 overlap
    (is (= [467]
           (check-for-overlaps {:numbers numbers :indices [0]})))
    ;; Several symbols on the same number
    (is (= [467 114]
           (check-for-overlaps {:numbers numbers :indices [0 1 2 3 4]})))
    ;; No Overlaps
    (is (= []
           (check-for-overlaps {:numbers numbers :indices [10]})))))

;; Part 2

(deftest mapify-row-star-test
  (is (= {:numbers [] :stars []}
         (mapify-row-star "")))
  (is (= {:numbers [] :stars []}
         (mapify-row-star ".")))
  (is (= {:numbers [{:match "12" :start 0 :end 2}] :stars []}
         (mapify-row-star "12")))
  (is (= {:numbers [] :stars [1]}
         (mapify-row-star ".*.")))
  (is (= {:numbers [{:match "12" :start 1 :end 3}
                    {:match "3" :start 4 :end 5}]
          :stars [3 5]}
         (mapify-row-star ".12*3*."))))

(deftest get-overlapping-numbers-test
  (let [numbers [{:match "467", :start 0, :end 3}
                 {:match "114", :start 3, :end 5}]]
    (is (= [467]
           (get-overlapping-numbers numbers 0)))
    (is (= [467 114]
           (get-overlapping-numbers numbers 3)))
    (is (= [114]
           (get-overlapping-numbers numbers 5)))))

(deftest create-gear-map-test
  (let [row-map {:stars [3],
                 :numbers [{:match "467", :start 0, :end 3}
                           {:match "35", :start 2, :end 4}
                           {:match "114", :start 5, :end 8}
                           {:match "633", :start 6, :end 9}]}]
    (is (= {:gear 3, :product-numbers [467 35], :gear-ratio 16345}
           (create-gear-map 3 (:numbers row-map))
           ))))
