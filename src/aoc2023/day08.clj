(ns aoc2023.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math])
  (:use [aoc2023.core]))

(def test1
  ["RL"
   ""
   "AAA = (BBB, CCC)"
   "BBB = (DDD, EEE)"
   "CCC = (ZZZ, GGG)"
   "DDD = (DDD, DDD)"
   "EEE = (EEE, EEE)"
   "GGG = (GGG, GGG)"
   "ZZZ = (ZZZ, ZZZ)"])

(def test2 
  ["LLR"
   ""
   "AAA = (BBB, BBB)"
   "BBB = (AAA, ZZZ)"
   "ZZZ = (ZZZ, ZZZ)"])

(defn parse-map-line
  [s]
  (let [node (subs s 0 3)
        l (subs s 7 10)
        r (subs s 12 15)]
    [node {\L l \R r}]))

(defn parse 
  [input]
  (let [[rl _ & maps] input]
    {:instructions rl
     :maps (into {} (map parse-map-line maps))}))

(parse test1)

(def start "AAA")
(def goal "ZZZ")

(defn count-steps
  [instructions maps start goal?]
  (loop [instr instructions
         node start
         n 0]
    (if (goal? node)
      n
      (let [map (maps node)
            [now & later] instr
            next-node (map now)]
        (recur (if (empty? later) instructions later) next-node (inc n))))))

(let [data (parse (str/split-lines (slurp (io/resource "day08.txt"))))
      instructions (:instructions data)
      maps (:maps data)]
  (count-steps instructions maps start #(= % goal)))

(def test-part2
 ["LR"
  ""
  "11A = (11B, XXX)"
  "11B = (XXX, 11Z)"
  "11Z = (11B, XXX)"
  "22A = (22B, XXX)"
  "22B = (22C, 22C)"
  "22C = (22Z, 22Z)"
  "22Z = (22B, 22B)"
  "XXX = (XXX, XXX)"])
        

(let [data (parse (str/split-lines (slurp (io/resource "day08.txt"))))
      instructions (:instructions data)
      maps (:maps data)
      startnodes (filter #(str/ends-with? % "A") (keys maps))
      goal? (fn [s] (str/ends-with? s "Z"))]
  (->> startnodes
       (map #(count-steps instructions maps % goal?))
       (reduce math/lcm)))
