(ns aoc2023.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def test-data
  ["467..114.."
   "...*......"
   "..35..633."
   "......#..."
   "617*......"
   ".....+.58."
   "..592....."
   "......755."
   "...$.*...."
   ".664.598.."])

(def data (str/split-lines (slurp (io/resource "day03.txt"))))

(defn re-seq-pos [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons {:start (. m start) :end (. m end) :group (. m group)}
               (lazy-seq (step))))))))

(def symbols
  (->> (apply str data)
       (map str)
       (remove #(re-find #"\d+" %))
       (remove #(= % "."))
       (map first)
       (set)))

(defn partnumber?
  [data line start end]
  (defn check-line
    [idx]
    (let [line (nth data idx)]
      (cond
        (< idx 0) []
        (> idx (count data)) []
        :else (let [l (max (- start 1) 0)
                    r (min (+ end 1) (count line))]
                (some #(contains? symbols %) (subs line l r))))))
  (let [above (max (- line 1) 0)
        below (min (+ line 1) (- (count data) 1))]
    (some check-line [above line below])))


(defn part1
  [input]
  (for [[idx line] (map vector (range) input)
        candidate (re-seq-pos #"\d+" line)
        :when (partnumber? input idx (:start candidate) (:end candidate))]
    (read-string (:group candidate))))

(reduce + (part1 data)) ;; Solution Part 1

(defn find-numbers-adjacent-to-pos
  [data i j]
  (defn find-in-line
    [line]
    (->> (re-seq-pos #"\d+" (nth data line))
         (filter #(>= j (- (:start %) 1)))
         (filter #(< j (+ (:end %) 1)))
         (map #(read-string (:group %)))))
  (let [above (max (- i 1) 0)
        below (min (+ i 1) (- (count data) 1))]
    (mapcat find-in-line [above i below])))



(for [[i line] (map vector (range) test-data)
      starmatches (re-seq-pos #"\*" line)]
  (list i starmatches (count (find-numbers-adjacent-to-pos test-data i (:start starmatches)))))

(find-numbers-adjacent-to-pos test-data 1 3)
