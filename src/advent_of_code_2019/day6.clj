(ns advent-of-code-2019.day6
  (:require [ubergraph.core :as uber]))

;;
;; https://adventofcode.com/2019/day/6
;;

;;
;; PART 1
;;

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

;;
;; PART 2
;;

(def part2-test-input (parse-input "day6-part2-test"))

(def part2-test-graph (make-directed-graph part2-test-input))

(defn find-path
  [start end g]
  (loop [path []
         node start]
    (if (= node end)
      path
      (recur (conj path node)
             (->> (uber/find-edge g {:src node})
                  (uber/dest))))))

(defn find-shortest-hop
  [g]
  (let [you-path (find-path :YOU :COM g)
        san-path (find-path :SAN :COM g)
        [common-y common-s] (loop [y you-path
                              s san-path]
                         (if (not= (peek y) (peek s))
                           [y s]
                           (recur (pop y)
                                  (pop s))))]
    (-> (conj common-y (-> (uber/find-edge g {:src (last common-y)})
                           uber/dest))
        (concat (reverse common-s)))))

(defn part2
  ([g]
   (let [c (count (find-shortest-hop g))]
     (dec (- c 2))))
  ([]
   (part2 part1-graph)))