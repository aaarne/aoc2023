(ns aoc2023.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math :as math]
            [clojure.core.match :refer [match]])
  (:use [aoc2023.core]))

(def testdata
  ["32T3K 765"
   "T55J5 684"
   "KK677 28"
   "KTJJT 220"
   "QQQJA 483"])

(def cards [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2])
(def base (count cards))
(def card->strength (zipmap (reverse cards) (range)))
(def score-factor (int (math/pow base 5)))

(defn hand->number
  ([hand cs]
   (->> hand
        (map cs)
        (map vector [4 3 2 1 0])
        (map #(let [[e b] %] (* b (int (math/pow base e)))))
        (reduce +)))
  ([hand] (hand->number hand card->strength)))

(defn create-fingerprint
  [hand]
  (->> hand
       (group-by identity)
       vals
       (map count)
       sort
       reverse
       (apply vector)))

(defn fingerprint->type
  [fingerprint]
  (match fingerprint
    [5] :five-of-a-kind
    [4 1] :four-of-a-kind
    [3 2] :full-house
    [3 1 1] :three-of-a-kind
    [2 2 1] :two-pair
    [2 1 1 1] :one-pair
    [1 1 1 1 1] :high-card))

(defn evaluate-hand
  [hand]
  (let [fingerprint (create-fingerprint hand)]
    (fingerprint->type fingerprint)))
       
(def handtype->score
  (let [types [:high-card 
               :one-pair 
               :two-pair 
               :three-of-a-kind 
               :full-house 
               :four-of-a-kind 
               :five-of-a-kind]
        scores (range (count types))]
    (zipmap types scores)))

(defn handscore
  [num handtype]
  (+ (* (handtype->score handtype) score-factor) num))

(defn parse
  [input]
  (for [entry input
        :let [[hand bid] (str/split entry #" ")]]
    {:hand hand :bid (read-string bid)}))


;; Solution Part 1
(->> testdata ;(str/split-lines (slurp (io/resource "day07.txt")))
     parse
     (map #(assoc % :type (evaluate-hand (:hand %))))
     (map #(assoc % :num (hand->number (:hand %))))
     (map #(assoc % :score (handscore (:num %) (:type %))))
     (sort-by :score)
     (zipmap (iterate inc 1))
     (map #(* (first %) (:bid (second %))))
     (reduce +))

(def cards2 [\A \K \Q \T \9 \8 \7 \6 \5 \4 \3 \2 \J])
(def card2->strength (zipmap (reverse cards2) (range)))

(defn evaluate-hand-part2
  [hand]
  (let [njokers (count (filter #(= \J %) hand))
        fingerprint (create-fingerprint (str/replace hand #"J" ""))]
    (if (= njokers 5) 
      :five-of-a-kind
      (let [[head & rest] fingerprint]
        (fingerprint->type (cons (+ njokers head) rest))))))

;; Solution Part 2
(->> (str/split-lines (slurp (io/resource "day07.txt")))
     parse
     (map #(assoc % :type (evaluate-hand-part2 (:hand %))))
     (map #(assoc % :num (hand->number (:hand %) card2->strength)))
     (map #(assoc % :score (handscore (:num %) (:type %))))
     (sort-by :score)
     (zipmap (iterate inc 1))
     (map #(* (first %) (:bid (second %))))
     (reduce +))
