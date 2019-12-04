(ns advent-of-code-2019.day3
  (:import (org.apache.commons.lang3 Range)))

;;
;; https://adventofcode.com/2019/day/3
;;

;;
;; PART 1
;;

(defn parse-command
  [command]
  (let [direction (keyword (str (first command)))
        num-moves (read-string (apply str (rest command)))]
    {:direction direction :num-moves num-moves}))

(defn parse-path-commands
  [line]
  (let [inputs (clojure.string/split line #",")]
    (map parse-command inputs)))

(defn parse-input
  [fn]
  (->> (clojure.java.io/resource fn)
       (slurp)
       (clojure.string/split-lines)
       (map parse-path-commands)))

(def test-input (parse-input "day3-test"))

(def input (parse-input "day3"))

(defn command->delta
  [command]
  (let [{:keys [direction num-moves]} command]
    (cond
      (= direction :U)
      [0 num-moves]
      (= direction :D)
      [0 (* -1 num-moves)]
      (= direction :L)
      [num-moves 0]
      (= direction :R)
      [(* -1 num-moves) 0])))

(defn path-points
  [commands]
  (let [deltas (map command->delta commands)]
    (reductions (fn [point delta] (mapv + point delta)) [0 0] deltas)))

(def test-paths (map #(path-points %) test-input))

(def paths (map path-points input))

(defn path-points->lines
  [path-points]
  (partition 2 1 path-points))

(defn paths-intersection-points
  [p1 p2]
  ; I know in my heart that cgrand could solve this in 3-4 elegant lines
  (let [lines1 (path-points->lines p1)
        lines2 (path-points->lines p2)]
    (for [l1 lines1
          l2 lines2
          :let [x-intersect (try
                              ; I also know in my heart that Clojure can handle intervals/ranges
                              ; Ah well, Java interop to the rescue
                              (let [x1r (Range/between (ffirst l1) (first (second l1)))
                                    x2r (Range/between (ffirst l2) (first (second l2)))]
                                (.intersectionWith x1r x2r))
                              (catch IllegalArgumentException e nil))
                y-intersect (try
                              (let [y1r (Range/between (second (first l1)) (second (second l1)))
                                    y2r (Range/between (second (first l2)) (second (second l2)))]
                                (.intersectionWith y1r y2r))
                              (catch IllegalArgumentException e nil))]
          :when (not-any? nil? [x-intersect y-intersect])]
      (->> [x-intersect y-intersect]
           (mapv #(.getMinimum %))))))

(defn manhattan-dist
  [coordinates]
  (->> (map #(Math/abs ^int %) coordinates)
      (apply +)))

(defn part1
  [[p1 p2]]
  (let [points (paths-intersection-points p1 p2)]
    (->> (map manhattan-dist points)
        (filter pos-int?)
         (apply min))))

