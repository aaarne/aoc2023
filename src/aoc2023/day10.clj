(ns aoc2023.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:use [aoc2023.core]))

(def actionmap
  {\| {:S :S, :N :N}
   \- {:W :W, :E :E}
   \L {:S :E, :W :N}
   \J {:E :N, :S :W}
   \7 {:E :S, :N :W}
   \F {:N :E, :W :S}})

(def moves
  {:S #(let [[r c] %] [(+ r 1) c])
   :N #(let [[r c] %] [(- r 1) c])
   :E #(let [[r c] %] [r (+ c 1)])
   :W #(let [[r c] %] [r (- c 1)])})


(def testdata
  ["7-F7-"
   ".FJ|7"
   "SJLL7"
   "|F--J"
   "LJ.LJ"])


(defn find-start
  [data]
  (->> data
       (map-indexed (fn [i s] [i (str/index-of s "S")]))
       (filter second)
       first))

(defn find-start-directions
  [pos data]
  (let [north (get-in data ((moves :N) pos))
        east (get-in data ((moves :E) pos))
        west (get-in data ((moves :W) pos))
        south (get-in data ((moves :S) pos))]
    (->> [(if (#{\| \7 \F} north) :N nil)
          (if (#{\- \J \7} east) :E nil)
          (if (#{\- \L \F} west) :W nil)
          (if (#{\| \7 \F} south) :S nil)]
         (remove nil?))))

(defn generate-loop
  [data]
  (let [startpos (find-start data)
        startdir (second (find-start-directions startpos data))]
    (letfn [(traverse
              [x]
              (let [[_ pos dir] x
                    newp ((moves dir) pos)
                    char (get-in data newp)]
                (if (= char \S)
                  [\S newp startdir]
                  (let [action (actionmap char)
                        newd (action dir)]
                    [char newp newd]))))]
      (iterate traverse [\S startpos startdir]))))


;(def data testdata)
(def data (str/split-lines (slurp (io/resource "day10.txt"))))

;; Solution 1

(let [looplen (->> (generate-loop data)
                   rest
                   (take-while #(not= (first %) \S))
                   count)
      n (+ looplen 1)]
  (/ n 2))


(def pathmap (->> (generate-loop data)
                  rest
                  (take-while #(not= (first %) \S))
                  (cons (first (generate-loop data)))
                  (map #(vector (second %) (nth % 2)))
                  (into {})))

(defn label-pos
  [pos]
  (defn in-out?
    [pos]
    (loop [p pos]
      (let [[r c] p
            dir (pathmap p)]
        (if (nil? dir)
          (if (= c 0)
            :O
            (recur [r (- c 1)]))
          (case dir
            :N :I
            :S :O
            :W (let [above (pathmap [(- r 1) c])
                     below (pathmap [(+ r 1) c])]
                 (cond
                   (= above :S) :O
                   (= above :N) :I
                   (= below :S) :O
                   (= below :N) :I))))))) 
  (if (contains? pathmap pos)
    (pathmap pos)
    (in-out? pos)))

;; Draw image
(def labeled-points
  (let [nr (count data)
        nc (count (first data))]
    (for [r (range nr)]
      (str/join (->> (range nc) (map #(name (label-pos [r %]))))))))

;; Solution Part 2

(->> (str/join labeled-points)
     (filter #(= % \I))
     count)
