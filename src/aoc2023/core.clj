(ns aoc2023.core)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Advent of Code 2023!"))

(defn re-seq-pos [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons {:start (. m start) :end (. m end) :group (. m group)}
               (lazy-seq (step))))))))