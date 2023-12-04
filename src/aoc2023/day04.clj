(ns aoc2023.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def data (str/split-lines (slurp (io/resource "day04.txt"))))
(def test-data
  ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(defn read-card
  [s]
  (defn parse-numbers
    [space-seperated]
    (map read-string (remove empty? (str/split space-seperated #" "))))
  (let [s (str/split s #": ")
        id (read-string (re-find #"\d+" (first s)))
        [winning have] (str/split (second s) #"\|")]
    {:id id
     :winning (set (parse-numbers winning))
     :have (parse-numbers have)}))

(defn count-matches
  [card]
  (let [winning (:winning card)
        have (:have card)]
    (->> have
         (filter winning)
         (count))))

(defn evaluate-card
  [card]
  (let [amount (count-matches card)]
    (cond
      (= amount 0) 0
      (= amount 1) 1
      :else (reduce * (take (- amount 1) (repeat 2))))))

(def part1
  (->> data
       (map read-card)
       (map evaluate-card)
       (reduce +))) ;; Solution 1

(defn read-card-map
  [data]
  (into {} (map #(vector (:id %) %) (map read-card data))))

(defn process-part2
  [data]
  (let [cardmap (read-card-map data)]
    (loop [todo (map :id (vals cardmap))
           result nil]
      (if (empty? todo)
        result
        (let [card (cardmap (first todo))
              id (:id card)
              matches (count-matches card)
              newcards (if (= matches 0)
                         nil
                         (map #(+ 1 id %) (range matches)))]
          (recur (concat newcards (rest todo))
                 (conj result id)))))))

(def part2 (count (process-part2 data))) ;; Solution 2