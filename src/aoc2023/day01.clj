(ns aoc2023.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "day01_1.txt"))))

(defn decode-calibration
  [message]
  (let [numbers (->> message
                     (re-seq #"\d")
                     (mapv read-string))]
    (+ (* 10 (first numbers)) (last numbers))))

(def solution_part1
  (->> input
       (map decode-calibration)
       (reduce +)))

(println "Part 1" solution_part1)

(def sr str/replace)

(defn str2digit
  [s]
  (-> s
      (sr #"one" "o1e")
      (sr #"two" "t2o")
      (sr #"three" "t3e")
      (sr #"four" "f4r")
      (sr #"five" "f5e")
      (sr #"six" "s6x")
      (sr #"seven" "s7n")
      (sr #"eight" "e8t")
      (sr #"nine" "n9e")))

(take 10 input)
(->> input (map str2digit) (take 10))
(->> input (map str2digit) (map decode-calibration) (take 10))

(def solution_part2
  (->> input
       (map str2digit)
       (map decode-calibration)
       (reduce +)))

(println "Part 2" solution_part2)