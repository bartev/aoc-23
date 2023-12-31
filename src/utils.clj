(ns utils
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]))

(defn get-local-fname
  "Get the full filename when in same dir as this file."
  [fname]
  (-> (fs/parent *file*)
      (fs/path fname)
      str))

(defn read-local-file
  "Read an entire file file in the current directory"
  [fname]
  (let [path (get-local-fname fname)]
    (slurp path)))

;; (read-file "sample-input.txt")

(defn read-file-lines
  "Read a file line-by-line"
  [fname]
  (with-open [reader (io/reader fname)]
    (doall (line-seq reader))))


(defn get-local-fname
  "Get fname relative to source file."
  [fname]
  (-> (fs/parent *file*)
      (fs/path fname)
      str))

(defn read-file-lines
  "Return a vector of lines from the file?"
  [fname]
  (with-open [reader (io/reader fname)]
    (doall (line-seq reader))))


;; Probably won't use this.
(defn read-file
  "Read an entire file file in the current directory"
  [fname]
  (let [path (-> (fs/parent *file*)
                 (fs/path fname)
                 str)]
    (slurp path)))
