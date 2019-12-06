(ns advent-of-code-2019.day5
  (:require [advent-of-code-2019.day4 :as day4]
            [clojure.java.io :as io]))

;;
;; https://adventofcode.com/2019/day/5
;;

(defn read-and-save
  [v write-index]
  (println "Please enter TEST input")
  (->> (read-line)
       Integer/parseInt
       (assoc v write-index)))

(defn output
  [input]
  (println (str input)))

(defn param-val
  [mode v val]
  (if (= :immediate mode)
    val
    (get v val)))

(defn- jump-if
  [v instruction-index params param-modes f]
  (let [[p1 p2] params
        [m1 m2] param-modes
        val1 (param-val m1 v p1)
        val2 (param-val m2 v p2)]
    ;(println (str "jump-if: params = " (into [] params)))
    ;(println (str "jump-if: param-modes = " param-modes))
    ;(println (str "jump-if: val1 = " val1))
    ;(println (str "jump-if: val2 = " val2))
    (if (f val1)
      [instruction-index
       (assoc v instruction-index val2)]
      [(+ instruction-index (inc (count params)))
       v])))

(defn jump-if-true
  [v instruction-index params param-modes]
  (jump-if v instruction-index params param-modes (complement zero?)))

(defn jump-if-false
  [v instruction-index params param-modes]
  (jump-if v instruction-index params param-modes zero?))

(defn- write-one-or-zero
  [v instruction-index params param-modes f]
  (let [[p1 p2 p3] params
        [m1 m2 m3] param-modes
        val1 (param-val m1 v p1)
        val2 (param-val m2 v p2)
        val3 (param-val m3 v p3)
        store-val (if (f val1 val2) 1 0)
        updated-v (assoc v val3 store-val)]
    (if (= val3 instruction-index)
      [instruction-index
       updated-v]
      [(+ instruction-index (inc (count params)))
       updated-v])))

(defn less-than
  [v instruction-index params param-modes]
  (write-one-or-zero v instruction-index params param-modes <))

(defn equals
  [v instruction-index params param-modes]
  (write-one-or-zero v instruction-index params param-modes =))

(def test-opcodes (assoc {1  +
                          2  *
                          99 :stop}
                    3 read-and-save
                    4 output
                    5 jump-if-true
                    6 jump-if-false
                    7 less-than
                    8 equals))
(defn num-opcode-params
  [opcode]
  (cond
    (contains? #{3 4} opcode)
    1
    (contains? #{5 6} opcode)
    2
    :else
    3))

(def param-modes {0 :position
                  1 :immediate})

(defn parse-opcode
  [num]
  (let [d (day4/digits num)
        opcode (->> (take-last 2 d)
                    (apply str)
                    read-string)
        num-params (num-opcode-params opcode)
        param-mode-inputs (drop-last 2 d)
        num-pads (- num-params (count param-mode-inputs))
        param-modes (->> param-mode-inputs
                         (concat (take num-pads (repeat 0)))
                         reverse
                         (mapv #(get param-modes %)))]
    {:opcode      opcode
     :num-params  num-params
     :param-modes param-modes}))

(defn run-op
  [{:keys [opcode num-params param-modes]} program index opcode-lookup]
  (let [op (get opcode-lookup opcode)
        params (take num-params (drop (+ index 1) program))
        default-next-index (+ index (inc num-params))]
    ;(println (str "run-op: opcode = " opcode))
    ;(println (str "run-op: op = " op))
    ;(println (str "run-op: num-params = " num-params))
    ;(println (str "run-op: params = " (into [] params)))
    ;(println (str "run-op: param-modes = " param-modes))
    ;(println (str "run-op: program = " program))
    ;(println (str "run-op: index = " index))
    (cond
      (contains? #{+ *} op)
      (let [[p1 p2 p3] params
            [m1 m2 _] param-modes
            result (op (param-val m1 program p1) (param-val m2 program p2))]
        [default-next-index
         (assoc program p3 result)])

      (= op read-and-save)
      [default-next-index
       (let [write-index (first params)]
         (op program write-index))]

      (= op output)
      (let [mode (first param-modes)
            val (param-val mode program (first params))]
        (op val)
        [default-next-index
         program])

      (contains? #{jump-if-true
                   jump-if-false
                   less-than
                   equals} op)
      (op program index params param-modes))))

(defn stop?
  [opcode-info]
  (let [op (get test-opcodes (:opcode opcode-info))]
    (= op :stop)))

(defn run-test-program
  [program opcode-lookup]
  (loop [index 0
         updated-program program]
    ;(println (str "run-test-program: index =" index))
    ;(println (str "run-test-program: updated-program =" updated-program))
    (let [opcode-input (last (take (+ index 1) updated-program))
          opcode-info (parse-opcode opcode-input)]
      (if (stop? opcode-info)
        updated-program
        (let [[latest-index latest-program] (run-op opcode-info updated-program index opcode-lookup)]
          (recur latest-index latest-program))))))

(def part1-test-input-1 [3, 0, 4, 0, 99])

(def part1-test-input-2 [1002, 4, 3, 4, 33])

(def part2-test-input1 [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8])

(def part2-test-input2 [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8])

(def part2-test-input3 [3, 3, 1108, -1, 8, 3, 4, 3, 99])

(def part2-test-input4 [3, 3, 1107, -1, 8, 3, 4, 3, 99])

(def part2-test-input5 [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9])

(def part2-test-input6 [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1])

(def part2-test-input7 [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99])

(def input (read-string (str "[" (slurp (clojure.java.io/resource "day5")) "]")))

(comment
  ; Part 1
  ; (Enter 1)
  (run-test-program input test-opcodes)

  ; Part 2
  ; (Enter 5)
  (run-test-program input test-opcodes))

