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
      (cond
        (day5/stop? opcode-info)
        (do
          ;(println (str "run-test-program: STOP (index = " index ")"))
          updated-program)
        :else
        (let [[latest-index latest-program] (run-op opcode-info a updated-program index)]
          (recur latest-index latest-program))))))

(defn amplifier-test-fn
  [amplifier-setting input program]
  (fn [] (let [a (atom {:inputs [input amplifier-setting]})]
           ;(println (str "amplifier-test-fn: a = " @a " amplifier-setting " amplifier-setting " input = " input))
           (run-test-program program a)
           ;(println (str "amplifier-test-fn: a = " @a))
           (:output @a))))

(defn test-phase-settings
  [program]
  (->> (for [s part1-amplifier-settings]
         (let [test-chain-result (-> (reduce
                                       (fn [output amplifier-setting]
                                         ((amplifier-test-fn amplifier-setting output program)))
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
;; Most of the amplifiers are connected as they were before; amplifier A's output is connected to amplifier B's input, and so on.
;; However, the output from amplifier E is now connected into amplifier A's input. This creates the feedback loop: the signal
;; will be sent through the amplifiers many times.
;;
;; In feedback loop mode, the amplifiers need totally different phase settings: integers from 5 to 9, again each used exactly once.
;; These settings will cause the Amplifier Controller Software to repeatedly take input and produce output many times before halting.
;; Provide each amplifier its phase setting at its first input instruction; all further input/output instructions are for signals.
;;
;; Don't restart the Amplifier Controller Software on any amplifier during this process.
;; Each one should continue receiving and sending signals until it halts.
;;
;; All signals sent or received in this process will be between pairs of amplifiers except the very first signal and the very last signal.
;; To start the process, a 0 signal is sent to amplifier A's input exactly once.
;;
;; Eventually, the software on the amplifiers will halt after they have processed the final loop.
;; When this happens, the last output signal from amplifier E is sent to the thrusters.
;; Your job is to find the largest output signal that can be sent to the thrusters using the new phase settings and feedback loop
;; arrangement.
;;

;
; Questions for Part 2:
;
; "Don't restart the Amplifier Controller Software on any amplifier during this process."
; - Start by assuming that means for a given phase-setting sequence
; TODO: Figure out if that's for a given phase-setting sequence, or across sequences
;
; What should the amplifier program do after it processes the 'output' op?
; - Start by assuming that we save off the instruction index and program as-is, and resume next time around loop
; TODO: Figure out whether to leave the instruction index and program as-is, or to run to completion
;
; Notes for Part 2:
;
; Have a global atom for all state.
;
;

(def amplifier-ids [:A :B :C :D :E])

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

(defn output?
  [opcode]
  (= (get test-opcodes opcode)
     output))

(defn init-amplifiers
  [s p]
  (let [configs (->> (map vector amplifier-ids s)
                     (map #(vector (first %) {:program p :instruction-index 0 :inputs (list (second %))}))
                     (into {}))]
    (assoc-in configs [:A :inputs] (->> (get-in configs [:A :inputs])
                                        (cons 0)))))

(defn run-feedback-op
  [opcode-info updated-program index state-store]
  (throw (Exception. "Implement run-feedback-op")))

(defn run-amplifier-program
  [state-store amplifier-id]
  (let [current-state @state-store
        p (get current-state :program)
        i (get current-state :instruction-index)]
    (loop [index i
           updated-program p]
      (let [opcode-input (last (take (+ index 1) updated-program))
            opcode-info (day5/parse-opcode opcode-input)]
        (cond

          (day5/stop? opcode-info)
          {:amplifier-id amplifier-id :program updated-program :instruction-index index :last-op :stop}

          (output? opcode-info)
          (let [{:keys [_ num-params param-modes]} opcode-info
                mode (first param-modes)
                params (take num-params (drop (+ index 1) updated-program))
                val (day5/param-val mode updated-program (first params))]
            {:amplifier-id amplifier-id :program updated-program :instruction-index index :output val})

          :else
          (let [[latest-index latest-program] (run-feedback-op opcode-info updated-program index state-store)]
            (recur latest-index latest-program)))))))

(defn run-amplifier-chain
  [state-store]
  ; Run each amplifier's program
  ; If stop is encountered before amplifier E, throw exception
  ; Otherwise if output cons onto input for next amplifier
  ; Save amplifier program state to the store
  ; Return the result from the last amplifier program run
  (throw (Exception. "Implement run-amplifier-chain"))
  )

(defn test-phase-setting-sequence
  [phase-settings program]
  (let [state-store (init-amplifiers phase-settings program)]
    ; Run amplifier chain
    ; If the chain is halted, return the output from E (read from the state-store)
    ; Otherwise, cons E's output onto A's input, save to store, and re-run the amplifier chain
    @state-store))







