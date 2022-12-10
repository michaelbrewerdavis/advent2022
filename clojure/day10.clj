(ns day10
  (:require [clojure.core :as core] [clojure.string :as str]))


(require '("fs" :as fs))
(def input (str (fs/readFileSync "./clojure/day10.input")))

(defn parse-string-or-nil [s] (if (not (nil? s)) (parse-long s) nil))

(def parsed
  (let [lines
        (str/split-lines input)
        split-to-string
        (map #(str/split %1 #" ") lines)
        split-to-int
        (map #(do [(first %1) (parse-string-or-nil (second %1))]) split-to-string)]
    split-to-int))


(defn apply-instruction [register instr i]
  (let [[op arg] instr]
    ;; (prn "apply-1" i register op arg)
    (if (= "noop" op)
      [register]
      [(+ register arg) register])))

(apply-instruction 3 ["noop" nil] 3)

(defn apply-instructions
  ([instructions] (apply-instructions instructions 1 [1] 1))
  ([instructions register history index]
   (if (empty? instructions)
     (reverse (flatten history))
     (let [[current & rest] instructions
           next-cycles (apply-instruction register current index)
           next-register (first next-cycles)]
       (recur rest next-register (cons next-cycles history) (+ index (count next-cycles)))))))

(def history (apply-instructions parsed))

(defn at-cycle [n] (nth history (- n 1)))
(def positions [20 60 100 140 180 220])

(reduce + (mapv * positions (map at-cycle positions)))

(defn draw-pixel [x register]
  (if (>= 1 (Math/abs (- register x)))
    "X"
    "."))

(map (partial reduce +) (partition 40 (mapv #(draw-pixel %1 %2) (flatten (repeat 6 (range 40))) history)))
