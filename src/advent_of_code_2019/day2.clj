(ns advent-of-code-2019.day2)

;;
;; https://adventofcode.com/2019/day/2
;;

;;
;; PART 1
;;

(def program (read-string (str "[" (slurp (clojure.java.io/resource "day2")) "]")))

(def opcodes
  {1  +
   2  *
   99 :stop})
(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn collect-updates
  [program]
  (loop [index 0
         updates []
         updated-program program]
    (let [opcode (last (take (+ index 1) updated-program))
          op (get opcodes opcode)]
      (if (= op :stop)
        {:input-program  program
         :updates        updates
         :output-program updated-program}
        (let [[p1 p2 p3] (take 3 (drop (+ index 1) updated-program))
              result (op (get updated-program p1) (get updated-program p2))]
          (recur (+ index 4)
                 (conj updates [p3 result])
                 (assoc updated-program p3 result)))))))

(defn run-program
  ([program noun verb]
   (let [restored-state (-> program
                            (assoc 1 noun)
                            (assoc 2 verb))]
     (collect-updates restored-state)))
  ([program]
   (run-program program 12 2)))

(defn part1
  ([program]
   (first (:output-program (run-program program))))
  ([program noun verb]
   (first (:output-program (run-program program noun verb)))))

;;
;; PART 2
;;

(defn find-noun-verb-matching-result
  [program result]
  (let [noun-verb-pairs (mapcat (fn [x] (map #(vector x %) (range 0 100))) (range 0 100))
        all-results
        (map #(hash-map :result (part1 program (first %) (second %)) :noun-verb-pair %) noun-verb-pairs)]
    (-> (filter #(= result (:result %)) all-results)
        first)))

(defn part2
  []
  (let [match (find-noun-verb-matching-result program 19690720)
        {:keys [noun-verb-pair]} match
        [noun verb] noun-verb-pair]
    (+ (* 100 noun) verb)))
