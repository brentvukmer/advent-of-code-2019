(ns advent-of-code-2019.day4)

;;
;; https://adventofcode.com/2019/day/4
;;

;;
;; PART 1
;;

(defn- digits
  [num]
  (->> (str num)
       (map str)
       (map read-string)))

(defn- at-least-one-pair?
  [digit-pairs]
  (-> (some #(= 1 (count (set %))) digit-pairs)
      true?))

(defn- never-decrease?
  [digit-pairs]
  (let [some-decrease (some #(< (second %) (first %)) digit-pairs)]
    (not some-decrease)))

(defn valid-password?
  [num]
  (let [digit-pairs (partition 2 1 (digits num))]
    (and
      (at-least-one-pair? digit-pairs)
      (never-decrease? digit-pairs))))

(def input (let [tokens (-> (clojure.java.io/resource "day4")
                            (slurp)
                            (clojure.string/split #"-"))]
             (map read-string tokens)))

(defn part1
  [[start end]]
  (let [r (range start (inc end))]
    (-> (filter valid-password? r)
        count)))

;;
;; PART 2
;;

(defn- counter-store
  [d]
  (->> (-> (set d)
           (interleave (repeat 0)))
       (apply hash-map)))

(defn- digit-counts
  [num]
  (let [d (digits num)
        store (counter-store d)]
    (reduce (fn [accum digit]
              (assoc accum digit (inc (get accum digit)))) store d)))

(defn- no-larger-matching-group?
  [num]
  (=
    (->> (digit-counts num)
         vals
         set
         (some #{2}))
    2))

(defn part2
  [[start end]]
  (let [r (range start (inc end))]
    (-> (filter #(and
                   (valid-password? %)
                   (no-larger-matching-group? %)) r)
        count)))