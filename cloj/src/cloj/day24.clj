(ns cloj.day24
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")

;; (def input (slurp "./day24.input"))

(defn parse-pt [pt]
  (cond (= pt \#)
        \#
        (= pt \.)
        []
        :else
        [pt]))

(def world (map vec (map #(map parse-pt %1) (str/split-lines input))))

(defn blank-row [row]
  (vec (map #(if (= \# %1)
               %1
               []) row)))

(defn blank-world [world]
  (vec (map blank-row world)))

(blank-world world)

(map-indexed #(do [%1 %2]) [1 2 3])

(defn get-world [world row_num col_num]
  (if (or (< row_num 0) (< col_num 0)
          (>= row_num (count world))
          (>= col_num (count (first world))))
    \#
    (nth (nth world row_num) col_num)))

(defn set-world [world row_num col_num val]
  ;; (prn "setting")
  ;; (prn world row_num col_num val)
  ;; (prn (nth (nth world row_num) col_num))
  ;; (prn           (nth world row_num))
  (assoc world
         row_num
         (assoc
          (nth world row_num)
          col_num
          (cons val (get-world world row_num col_num)))))

(defn next-pos [world blizz row_num col_num]
  (let [height (count world)
        width (count (first world))]
    (cond (= blizz \>)
          (if (= col_num (- width 2))
            [row_num 1]
            [row_num (+ col_num 1)])
          (= blizz \<)
          (if (= col_num 1)
            [row_num (- width 2)]
            [row_num (- col_num 1)])
          (= blizz \^)
          (if (= row_num 1)
            [(- height 2) col_num]
            [(- row_num 1) col_num])
          (= blizz \v)
          (if (= row_num (- height 2))
            [1 col_num]
            [(+ row_num 1) col_num]))))

(defn iterate-row [new-world [row_num row]]
  ;; (prn row_num row)
  (letfn [(iterate-cell [world [col_num cell]]
            (letfn [(iterate-blizzard [world blizz]
                      ;; (prn "blizz" row_num col_num blizz)
                      (let [[r c] (next-pos world blizz row_num col_num)]
                        (set-world world r c blizz)))]
              (if (= \# cell)
                world
                (reduce iterate-blizzard world cell))))]
    (reduce iterate-cell new-world (map-indexed vector row))))

(defn iterate-world [world]
  (let [new-world (blank-world world)]
    (reduce iterate-row new-world (map-indexed vector world))))

(def world-at
  (memoize (fn [n]
             (if (= n 0)
               world
               (iterate-world (world-at (- n 1)))))))

(world-at 18)

(def initial-point [0 1])
(def final-point
  [(- (count world) 1)
   (- (count (first world)) 2)])

(defn possible-moves [pt]
  (set (map #(mapv + pt %1)
            [[0 0]
             [0 1]
             [1 0]
             [-1 0]
             [0 -1]])))

(defn valid-move-at? [[row_num col_num] world]
  ;; (prn row_num col_num (get-world world row_num col_num))
  (let [value (get-world world row_num col_num)]
    (and (vector? value) (empty? value))))

(possible-moves [1 4])

(defn iterate-moves [world starting-points]
  (let [possible-moves (apply set/union (map possible-moves starting-points))
        valid-moves (set (filter #(valid-move-at? %1 world) possible-moves))]
    valid-moves))


(defn iterate-scenario [[world moves]]
  (let [next-world (iterate-world world)
        next-moves (iterate-moves next-world moves)]
    [next-world next-moves]))


(defn move-scenario [initial-world initial-point final-point]
  (first (keep-indexed
          #(if (contains? (second %2) final-point) [%1 %2] nil)
          (take 1000 (iterate iterate-scenario [initial-world [initial-point]])))))

(def pass1 (move-scenario world initial-point final-point))
(def pass2 (move-scenario
            (first (second pass1))
            final-point
            initial-point))
(def pass3 (move-scenario
            (first (second pass2))
            initial-point
            final-point))

(prn (first pass1))
(prn (first pass2))
(prn (first pass3))
;; (map second (take 20 (iterate iterate-scenario [world [initial-point]])))

(+ (first pass1) (first pass2) (first pass3))
