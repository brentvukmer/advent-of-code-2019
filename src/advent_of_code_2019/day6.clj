(ns advent-of-code-2019.day6
  (:require [ubergraph.core :as uber]))

(defn parse-input
  [fn]
  (->> (slurp (clojure.java.io/resource fn))
       clojure.string/split-lines
       (mapcat #(clojure.string/split % #"\)"))
       (map keyword)
       (partition 2)
       (map reverse)
       (map vec)
       vec))

(def test-input (parse-input "day6-test"))

(def input (parse-input "day6"))

(defn make-directed-graph
  [edges]
  (->> edges
       (reduce (fn [graph edge] (uber/add-edges graph edge)) (uber/digraph []))))

(def test-graph (make-directed-graph test-input))

(def part1-graph (make-directed-graph input))

;
; Get all objects except COM
; For each object, recursively build path to COM
; Sum the lengths of all paths
;

(defn collect-paths
  [input]
  (let [objs (set (apply concat input))
        graph (make-directed-graph input)]
    (for [obj (remove :COM objs)]
      (loop [path []
             node obj]
        (if (= :COM node)
          path
          (recur (conj path node)
                 (->> (uber/find-edge graph {:src node})
                     (uber/dest))))))))

(defn part1
  ([input]
   (->> (collect-paths input)
        (map count)
        (apply +)))
  ([]
   (part1 input)))