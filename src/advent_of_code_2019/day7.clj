(ns advent-of-code-2019.day7
  (:require [advent-of-code-2019.day5 :as day5]))

;;
;; https://adventofcode.com/2019/day/7
;;

(defn read-program
  [fn]
  (read-string (str "[" (slurp (clojure.java.io/resource fn)) "]")))

(def day7-program (read-program "day7"))

(def day7-test-fixture1
  {:max-thruster-signal    43210
   :phase-setting-sequence [4, 3, 2, 1, 0]
   :program                [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]})

(def day7-test-fixture2
  {:max-thruster-signal    54321
   :phase-setting-sequence [0, 1, 2, 3, 4]
   :program                [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
                            101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0]})

(def day7-test-fixture3
  {:max-thruster-signal    65210
   :phase-setting-sequence [1, 0, 4, 3, 2]
   :program                [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
                            1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0]})

(defn read-and-save
  [a v write-index]
  (let [inputs (:inputs @a)]
    ;(println (str "read-and-save: a = " @a " v = " v " write-index = " write-index))
    (if ((complement empty?) inputs)
      (let [input (peek inputs)
            q (pop inputs)]
        (swap! a assoc :inputs q)
        ;(println (str "read-and-save: updated a = " @a))
        (assoc v write-index input))
      (day5/read-and-save v write-index))))

(defn output
  [a val]
  (swap! a assoc :output val))

(def part1-amplifier-settings
  (let [r (range 0 5)]
    (-> (-> (for [x (range 0 1000)]
              (shuffle r))
            set
            vec))))

(def test-opcodes
  {1  +
   2  *
   99 :stop
   3  read-and-save
   4  output
   5  day5/jump-if-true
   6  day5/jump-if-false
   7  day5/less-than
   8  day5/equals})

(defn run-op
  [{:keys [opcode num-params param-modes]} a program index]
  (let [op (get test-opcodes opcode)
        params (take num-params (drop (+ index 1) program))
        default-next-index (+ index (inc num-params))]
    ;(println (str "run-op: op = " op " params = " (into [] params)))
    (cond
      (nil? op)
      (throw (AssertionError. (str "Invalid opcode: " opcode)))
      (contains? #{+ *} op)
      (let [[p1 p2 p3] params
            [m1 m2 _] param-modes
            result (op (day5/param-val m1 program p1) (day5/param-val m2 program p2))]
        [default-next-index
         (assoc program p3 result)])

      (= op read-and-save)
      [default-next-index
       (let [write-index (first params)]
         ;(println (str "run-op: a = " @a " write-index = " write-index))
         (read-and-save a program write-index))]

      (= op output)
      (let [mode (first param-modes)
            val (day5/param-val mode program (first params))]
        (output a val)
        [default-next-index
         program])

      (contains? #{day5/jump-if-true
                   day5/jump-if-false
                   day5/less-than
                   day5/equals} op)
      (op program index params param-modes))))


(defn run-test-program
  [program a]
  (loop [index 0
         updated-program program]
    (let [opcode-input (last (take (+ index 1) updated-program))
          opcode-info (day5/parse-opcode opcode-input)]
      ;(println (str "run-test-program: program = " program " index = " index " a = " @a " opcode-info = " opcode-info))
      (if (day5/stop? opcode-info)
        (do
          ;(println (str "run-test-program: STOP (index = " index ")"))
          updated-program)
        (let [[latest-index latest-program] (run-op opcode-info a updated-program index)]
          (recur latest-index latest-program))))))

(defn amplifier-test-fn
  [amplifier-setting input program]
  (fn [] (let [a (atom {:inputs [input amplifier-setting] :output nil})]
           ;(println (str "amplifier-test-fn: a = " @a " amplifier-setting " amplifier-setting " input = " input))
           (run-test-program program a)
           ;(println (str "amplifier-test-fn: a = " @a))
           (:output @a))))

(defn test-phase-settings
  [program]
  (->> (for [s part1-amplifier-settings]
         (let [test-chain-result (-> (reduce
                                       (fn [output amplifier-setting]
                                         (let [atf (amplifier-test-fn amplifier-setting output program)] (atf)))
                                       0
                                       s)
                                     max)]
           {:phase-setting s :signal-level test-chain-result})
         )
       (sort-by :signal-level)
       last)
  )

(defn part1
  []
  (test-phase-settings day7-program))

;;
;; https://adventofcode.com/2019/day/7#part2
;;

(def part2-amplifier-settings
  (let [r (range 5 10)]
    (-> (-> (for [x (range 0 1000)]
              (shuffle r))
            set
            vec))))

(def day7-part2-test-fixture1 {:max-thruster-signal    139629729
                               :phase-setting-sequence [9, 8, 7, 6, 5]
                               :program                [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
                                                        27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]})


(def day7-part2-test-fixture2 {:max-thruster-signal    18216
                               :phase-setting-sequence [9, 7, 8, 5, 6]
                               :program                [3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
                                                        -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
                                                        53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10]})