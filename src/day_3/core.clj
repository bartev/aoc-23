(ns day-3.core
  (:require
   [utils :refer [read-file-lines-slurp]]
   [clojure.string :as str]
   [clojure.edn :as edn]))

;; Goal: Add up all the part numbers.
;; Input: Engine schematic

;; Any number adjacent to a symbol, even diagonally, is a "part number"
;; and should be included in your sum

;; Here is an example engine schematic:

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..

;; 1. Find numbers.
;; 2. Find coordinates of numbers (row, columns).
;; 3. Find symbols (not number, not `.`)
;; 4. Find coordinates of dots.
;; 5. Any digit in 8 spots around a symbol?
;; 5a. yes: Put number in a list.
;; 5b. no: replace symbol with `.` (or nil?)


;; 1. Read file into lines.
;; 2. Process a line. (line -> numbers-to-keep)

(defn line->numbers-to-keep
  "Check the line and adjacent line for symbols adjacent to numbers.
  Save any such numbers to a list."
  [line prev-line next-line]
  nil
  )

;; 3. Process all lines
;; 4. Sum up lists from all lines


(defn read-input [fname]
  (read-file-lines-slurp fname))

(def fname-sample "input-sample.txt")

(read-input fname-sample)
;; => ["467..114.."
;;     "...*......"
;;     "..35..633."
;;     "......#..."
;;     "617*......"
;;     ".....+.58."
;;     "..592....."
;;     "......755."
;;     "...$.*...."
;;     ".664.598.."]

(def regex-symbol #"[^0-9.]")
(def regex-number #"\d+")

(defn find-start-indices [s regex]
  (let [matcher (re-matcher regex s)] ; matcher finds all matches
    (loop [indices []]
      (if (.find matcher) ; .find advances through the matches
        (recur (conj indices (.start matcher)))
        indices))))

(defn find-start-end-indices [s regex]
  "Return a list of dicts for each match containing the match, start and end +1 indices."
  (let [matcher (re-matcher regex s)] ; matcher finds all matches
    (loop [result []]
      (if (.find matcher) ; .find advances through the matches
        (recur (conj result {:match (.group matcher)
                             :start (.start matcher)
                             :end (.end matcher)}))
        result))))

(defn mapify-row
  "Process each line of the list"
  [row]
  {:numbers (find-start-end-indices row regex-number)
   :symbols (find-start-indices row regex-symbol)})

(defn combine-sort-vectors [& vectors]
  (->> vectors
       (apply concat)
       set
       sort
       vec))

(defn combine-before-after-symbol-indices
  "Combine lists of symbol indices from prev, current and next row for each row"
  [row-maps]
  (map-indexed
   (fn [idx row]
     (let [before (get row-maps (dec idx))
           after (get row-maps (inc idx))
           before-idx (:symbols before)
           after-idx (:symbols after)
           cur-idx (:symbols row)
           all-idx (vec (combine-sort-vectors cur-idx before-idx after-idx))]
       (merge row {:indices all-idx})))
   row-maps))

(defn get-number-if-overlap [number-map indices]
  (let [start (dec (:start number-map))
        end (:end number-map)]
    (when (-> (mapv #(<= start % end) indices)
              (#(some true? %)))
      (parse-long (:match number-map)))))

(defn check-for-overlaps
  "Check each number in :numbers for an overlap in :indicdes"
  [row-map]
  (let [numbers (:numbers row-map)
        indices (:indices row-map)]
    (->> numbers
         (map #(get-number-if-overlap % indices))
         (remove nil?)
         vec)))

(defn part1 [fname]
  (->> (read-input fname) ; Read input file into a vec of strings.
       (map-indexed (fn [idx row] (merge {:row-idx idx} (mapify-row row))))  ; Mapfify each row (find numbers, symbols and indices of each)
       vec
       combine-before-after-symbol-indices
       (map check-for-overlaps)
       flatten
       (apply +)
       ))
;; => #'day-3.core/part1

(part1 fname-sample)
;; => 4361
;; => (467 35 633 617 592 755 664 598)
;; => ([467] [] [35 633] [] [617] [] [592] [755] [] [664 598])
