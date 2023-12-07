(ns aoc2023.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:use [aoc2023.core]))

(def data (str/split-lines (slurp (io/resource "day06.txt"))))

(def testdata
  ["Time:      7  15   30"
   "Distance:  9  40  200"])

(defn parse-data
  [input]
  {:times (map read-string (re-seq #"\d+" (first input)))
   :distances (map read-string (re-seq #"\d+" (second input)))})

(defn count-wins
  [time record]
  (->> (range (+ time 1))
       (map (fn [n] (* n (- time n))))
       (filter #(> % record))
       count))

(defn solve-part1
  [input]
  (let [parsed (parse-data input)
        times (:times parsed)
        distances (:distances parsed)]
    (->> (map count-wins times distances)
         (reduce *))))

(solve-part1 data)

(let [newinput (map #(str/replace % #" " "") data)]
  (solve-part1 newinput))
