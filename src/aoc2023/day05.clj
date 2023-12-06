(ns aoc2023.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:use [aoc2023.core]))

(def data (str/split-lines (slurp (io/resource "day05.txt"))))
(def testdata (str/split-lines (slurp (io/resource "day05_test.txt"))))

(defn parse-maps
  [lines]
  (loop [todo lines
         currentmap nil
         currentlist nil
         result {}]
    (let [[line & rest] todo]
      (cond
        (nil? line) result
        (= line "") (recur rest nil nil
                           (assoc result (:from currentmap)
                                  (assoc currentmap :ranges currentlist)))
        (nil? currentmap) (let [[s _] (str/split line #" ")
                                [from to] (str/split s #"-to-")]
                            (recur rest {:from (keyword from)
                                         :to (keyword to)} [] result))
        :else (let [numbers (map read-string (re-seq #"\d+" line))
                    [dest source len] numbers]
                (recur rest currentmap
                       (conj currentlist {:deststart dest
                                          :sourcestart source
                                          :len len}) result))))))


(defn parse-data
  ([serial seedparser]
   (let [[seeds _ & maps] serial]
     {:seeds (seedparser seeds)
      :maps (parse-maps (conj (into [] maps) ""))}))
  ([serial]
   (parse-data serial #(map read-string (re-seq #"\d+" %)))))

(def recipe
  [[:seed :soil]
   [:soil :fertilizer]
   [:fertilizer :water]
   [:water :light]
   [:light :temperature]
   [:temperature :humidity]
   [:humidity :location]])

(defn inrange?
  [n range]
  (let [start (:sourcestart range)
        end (+ start (:len range))]
    (and (>= n start)
         (< n end))))

(defn map-by-ranges
  [sourceid ranges]
  (let [range (first (filter #(inrange? sourceid %) ranges))]
    (if (nil? range)
      sourceid
      (let [deststart (:deststart range)
            sourcestart (:sourcestart range)]
        (+ deststart (- sourceid sourcestart))))))

(defn traverse-maps
  [seed maps]
  (loop [rec recipe
         outmap {:seed seed}]
    (if (empty? rec)
      outmap
      (let [[step & remaining] rec
            [from to] step]
        (recur remaining
               (assoc outmap to (map-by-ranges (from outmap) (get-in maps [from :ranges]))))))))

(defn generate-seeds
  [incomplete]
  (let [parsed incomplete
        seeds (:seeds parsed)
        maps (:maps parsed)]
    (map #(traverse-maps % maps) seeds)))

(def part1
  (->> data
       (parse-data)
       (generate-seeds)
       (map :location)
       (reduce min)))

(defn part2-seedparser
  [line]
  (let [nums (map read-string (re-seq #"\d+" line))]
    (loop [[start len & rest] nums
           seeds []]
      (if (nil? start)
        seeds
        (recur rest (conj seeds {:from start :to (+ start len) :len len}))))))

(defn range-to-new-ranges
  [range mapranges]
  (loop [start (:from range)
         n (:len range)
         newranges nil]
    (if (= n 0)
      newranges
      (let [m (first (filter #(inrange? start %) mapranges))]
        (if (nil? m)
          (let [nextstart (->> mapranges
                               (map :deststart)
                               (filter #(> % start))
                               sort
                               first)]
            (if (nil? nextstart)
              (recur (+ start n) 0 (conj newranges {:from start :to (+ start n) :len n}))
              (let [n-to-next (- nextstart start)]
                (if (< n n-to-next)
                  (recur (+ start n) 0 (conj newranges {:from start :to (+ start n) :len n}))
                  (recur nextstart (- n n-to-next) (conj newranges {:from start :to (- nextstart 1) :len n-to-next}))))))
          (let [deststart (:deststart m)
                sourcestart (:sourcestart m)
                len (- (:len m) (- start sourcestart))
                n-in-range (min n len)
                newstart (+ deststart (- start sourcestart))]
            (recur (+ start n-in-range)
                   (- n n-in-range)
                   (conj newranges {:from newstart
                                    :to (+ newstart n-in-range)
                                    :len n-in-range}))))))))


(defn ranges-to-new-ranges
  [ranges mapranges]
  (mapcat #(range-to-new-ranges % mapranges) ranges))

(def part2
  (let [d (parse-data data part2-seedparser)
        {seeds :seeds maps :maps} d]
    (loop [rec recipe
           rangemap {:seed seeds}]
      (if (empty? rec)
        rangemap
        (let [[step & remaining] rec
              [from to] step]
          (recur remaining
                 (assoc rangemap to (ranges-to-new-ranges (from rangemap) (get-in maps [from :ranges])))))))))

(->> part2
     :location
     (map :from)
     (reduce min))  

