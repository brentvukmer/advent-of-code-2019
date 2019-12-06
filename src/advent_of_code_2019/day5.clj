(ns advent-of-code-2019.day5
  (:require [advent-of-code-2019.day2 :as intcode]
            [advent-of-code-2019.day4 :as day4]
            [clojure.java.io :as io]))

(defn read-and-save
  [v write-index]
  (println "Please enter TEST input")
  (->> (read-line)
       Integer/parseInt
       (assoc v write-index)))

(defn output
  [input]
  (println (str input)))

(def test-opcodes (assoc intcode/opcodes
                    3 read-and-save
                    4 output))
(defn num-opcode-params
  [opcode]
  (if (contains? #{3 4} opcode)
    1
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
                         (mapv #(get param-modes %)))]
    {:opcode      opcode
     :num-params  num-params
     :param-modes param-modes}))

(defn param-val
  [mode v index-or-val]
  (if (= :immediate mode)
    index-or-val
    (get v index-or-val)))

(defn run-op
  [{:keys [opcode num-params param-modes]} program index]
  (let [op (get test-opcodes opcode)
        params (take num-params (drop (+ index 1) program))
        next-index (+ index (inc num-params))]
    (cond
      (contains? #{+ *} op)
      (let [[p1 p2 p3] params
            [m1 m2 _] param-modes
            result (op (param-val m1 program p1) (param-val m2 program p2))]
        [next-index
         (assoc program p3 result)])

      (= op read-and-save)
      [next-index
       (let [write-index (first params)]
         (read-and-save program write-index))]

      (= op output)
      (let [mode (first param-modes)
            val (param-val mode program (first params))]
        (output val)
        [next-index
         program]))))

(defn stop?
  [opcode-info]
  (let [op (get test-opcodes (:opcode opcode-info))]
    (= op :stop)))

(defn run-test-program
  [program]
  (loop [index 0
         updated-program program]
    ;(println (str "index =" index))
    ;(println (str "updated-program =" updated-program))
    (let [opcode-input (last (take (+ index 1) updated-program))
          opcode-info (parse-opcode opcode-input)]
      (if (stop? opcode-info)
        updated-program
        (let [[latest-index latest-program] (run-op opcode-info updated-program index)]
          (recur latest-index latest-program))))))

(defn part1
  [program]
  (run-test-program program))