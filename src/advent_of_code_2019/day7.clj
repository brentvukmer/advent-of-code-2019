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
   :phase-setting-sequence [1,0,4,3,2]
   :program                [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                            1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]})

;
; NOTES:
; - read-and-save needs to pull from atom before doing read-line
; - output needs to write to atom
; - discover-amplifier-settings needs to:
;    - generate the combinations of phase settings
;    - for each of the combinations, set up a chain of test-amplifier-settings function calls
;    - inside test-amplifier-settings, create an atom that stores phase setting and input signal
;


;;
;; https://adventofcode.com/2019/day/7#part2
;;