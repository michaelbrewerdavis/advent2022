(ns day5
  (:require [clojure.core :as core] [clojure.string :as str]))

;; (ns mbd.adventofcode.day5
;;   (:require [clojure.string :as str]))

(def input "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def input (slurp "./day5.input"))

(def instruction-groups (split-with
                         (fn [x] (not (= x "")))
                         (str/split-lines input)))

(defn zip [x y]
  (if (and (empty? x) (empty? y))
    ()
    (cons [(first x) (first y)] (zip (rest x) (rest y)))))

(defn find-char [s]
  (if (empty? s)
    nil
    (if (re-matches #"[A-Za-z]+" (str (first s)))
      (str (first s))
      (find-char (rest s)))))

(defn add-char [column char]
  (if (nil? char)
    column
    (cons char column)))

(defn parse-initial-state [lines]
  (let [[spec & elements] (reverse lines)
        num-cols (count (str/split (str/trim spec) #"\s+"))
        cols (repeat num-cols ())]
    (letfn [(add-row [c row]
              (let [els (map find-char (partition 4 4 " " row))]
                (map (fn [x] (add-char (first x) (second x))) (zip c els))))]
      (vec (reduce add-row cols elements)))))

(defn parse-move-instructions [lines]
  (letfn [(get-matches [line]
            (map (fn [x] (Integer/parseInt x)) (rest (re-find (re-matcher #"move (\d+) from (\d+) to (\d+)" line)))))]
    (map get-matches lines)))

(def initial-state (parse-initial-state (first instruction-groups)))
(def move-instructions (parse-move-instructions (rest (second instruction-groups))))

(defn move-element [state from to]
  (let [from-col (nth state (- from 1))
        to-col (nth state (- to 1))
        moved (first from-col)]
    (assoc (vec state) (- from 1) (rest from-col) (- to 1) (cons moved to-col))))


(defn apply-instruction [state instruction]
  (let [[amount source dest] instruction]
    (if (= 0 amount)
      state
      (apply-instruction (move-element state source dest) [(- amount 1) source dest]))))

(defn apply-instructions [state instructions]
  (reduce apply-instruction state instructions))
(prn initial-state)
(prn move-instructions)

(def final-state (apply-instructions initial-state move-instructions))
(prn (str/join (map first final-state)))

(defn apply-instruction-multiple-crates [state instruction]
  (let [[amount from to] instruction
        moved-elements (take amount (nth state (- from 1)))
        from-leftovers (drop amount (nth state (- from 1)))
        to-added (concat moved-elements (nth state (- to 1)))]
    (assoc state (- from 1) from-leftovers (- to 1) to-added)))

(def final-state-multiple-crates (reduce apply-instruction-multiple-crates initial-state move-instructions))
(prn (str/join (map first final-state-multiple-crates)))
