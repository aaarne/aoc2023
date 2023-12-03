(ns aoc2023.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "day02.txt"))))

(def test-data
  ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(def cubes-available
  {:red 12
   :green 13
   :blue 14})

(defn parse-line
  [line]
  (let [s (str/split line #": ")]
    (let [id (read-string (re-find #"\d+" (first s)))
          games (str/split (second s) #"; ")]
      [id (for [g games
                :let [entries (str/split g #", ")]]
            (for [en entries]
              (let [e (str/split en #" ")]
                [(->> e (first) (read-string))
                 (->> e (second) (keyword))])))])))

(defn game-possible?
  [game]
  (every? (fn [[n c]]
            (let [available (get cubes-available c)] 
              (>= available n)))
          game))

(def solution1
    (->> input
        (map parse-line)
        (map #(vector (first %) (map game-possible? (second %))))
        (map #(vector (first %) (every? identity (second %))))
        (filter second)
        (map first)
        (reduce +)))

(defn min-cubes
  [game] 
  (defn entry-to-map
    [e]
    (into {} (map #(vector (second %) (first %)) e)))
  (let [trials (map entry-to-map game)]
    {
     :red (apply max (remove nil? (map :red trials)))
     :green (apply max (remove nil? (map :green trials)))
     :blue (apply max (remove nil? (map :blue trials)))
     }))

(reduce * (vals (min-cubes (second (parse-line (first test-data))))))

(->> input
     (map parse-line)
     (map second)
     (map min-cubes)
     (map vals)
     (map #(reduce * %))
     (reduce +))