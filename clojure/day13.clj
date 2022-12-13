(ns day13
  (:require [clojure.core :as core] [clojure.string :as str]))

(def input "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

;; (def input (slurp "./day13.input"))

(def parsed (map #(map load-string (str/split-lines %1)) (str/split input #"\n\n")))

(defn ensure-vec [x] (if (sequential? x) x [x]))

(defn in-order? [left right]
  ;; (prn "checking" left right)
  (cond (and (int? left) (int? right))
        (cond (< left right)
              true
              (> left right)
              false
              :else
              :continue)
        (and (sequential? left) (sequential? right))
        (cond (and (empty? left) (empty? right))
              :continue
              (empty? left)
              true
              (empty? right)
              false
              :else
              (let [next-in-order (in-order? (first left) (first right))]
                (if (not= next-in-order :continue)
                  next-in-order
                  (recur (rest left) (rest right)))))
        :else
        (recur (ensure-vec left) (ensure-vec right))))

; sum of indices of pairs which are in order (1-based)
(prn (reduce +
             (map-indexed
              (fn [index is-in-order] (if is-in-order (+ 1 index) 0))
              (map #(apply in-order? %1) parsed))))

(defn in-order-comparator [left right]
  (let [in-order (in-order? left right)]
    (cond (true? in-order)
          -1
          (false? in-order)
          1
          :else
          0)))

(def two [[2]])
(def six [[6]])
(def sorted (sort in-order-comparator (concat (apply concat parsed) [two six])))

; product of indices of two and six markers
(prn (* (+ 1 (.indexOf sorted two)) (+ 1 (.indexOf sorted six))))
