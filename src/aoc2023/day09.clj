(ns aoc2023.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:use [aoc2023.core]))

(def testdata ["0 3 6 9 12 15" "1 3 6 10 15 21" "10 13 16 21 30 45"])

(defn differences
  [s]
  (->> (map vector s (rest s))
       (map #(let [[a b] %] (- b a)))))

(defn process
  [t]
  (->> (iterate differences t)
       (take-while #(not-every? (fn [x] (= 0 x)) %))))

(defn predict
  [update triangle]
  (let [r (reverse triangle)]
    (loop [todo r 
           n 0]
      (if (empty? todo)
        n
        (let [[f & rest] todo]
          (recur rest (update n f)))))))

;; Solution Part 1 & 2
(->> (str/split-lines (slurp (io/resource "day09.txt")))
     ;testdata
     (map #(map read-string (re-seq #"-*\d+" %)))
     (map process)
     ;(map (partial predict #(+ %1 (last %2))))  ; Part 1
     (map (partial predict #(- (first %2) %1))) ; Part 2
     (reduce +))