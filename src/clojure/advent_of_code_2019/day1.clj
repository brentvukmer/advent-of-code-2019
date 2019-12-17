(ns advent-of-code-2019.day1)

(def inputs (read-string
              (str "[" (slurp (clojure.java.io/resource "day1")) "]")))

(def test-inputs (read-string
                   (str "[" (slurp (clojure.java.io/resource "day1-test")) "]")))

;;
;; PART 1
;;

(defn fuel-for-mass
  [mass]
  (-> (Math/floorDiv mass 3)
      (- 2)))

(defn fuel-sum
  ([ms f]
   (apply + (map f ms)))
  ([ms]
   (fuel-sum ms fuel-for-mass)))

;;
;; PART 2
;;

(defn fuel-for-fuel
  [mass]
  (loop [m mass
         accum 0]
    (let [f (fuel-for-mass m)]
      (if (or
            (neg? f)
            (zero? f))
        accum
        (recur f
               (+ accum f))))))


(defn total-module-fuel
  [mass]
  (let [module-fuel (fuel-for-mass mass)]
    (+ module-fuel (fuel-for-fuel module-fuel))))

(defn total-fuel-sum
  [ms]
  (fuel-sum ms total-module-fuel))