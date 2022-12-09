(ns day8
  (:require [clojure.core :as core] [clojure.string :as str]))

(def input "30373
25512
65332
33549
35390")

;; (def input (slurp "./day8.input"))

(def tree-map
  (map (fn [line]
         (map parse-long (str/split line #"")))
       (str/split-lines input)))

(defn make-visibility-map [tree-map]
  (map (fn [row] (map (constantly 0) row)) tree-map))

(map (constantly 0) ["a" "b"])

(defn rotate [x] (map reverse (apply mapv vector x)))

(defn update-row-visibility [tree-row visibility-row min-height]
  ;; (prn tree-row visibility-row min-height)
  (let [current (first tree-row)
        current-visible (first visibility-row)]
    (cond (empty? tree-row)
          []
          (> current min-height)
          (cons (max 1 current-visible) (update-row-visibility (rest tree-row) (rest visibility-row) current))
          :else
          (cons current-visible (update-row-visibility (rest tree-row) (rest visibility-row) min-height)))))



(defn update-map-visibility [tree-map visibility-map]
  (let [zeros (repeat (count tree-map) -1)]
    (mapv update-row-visibility tree-map visibility-map zeros)))



(defn generate-map-visibility [tree-map visibility-map]
  (let [initial-state {:tree tree-map :vis visibility-map}
        four (repeat 4 0)]
    (letfn [(dostep [{tree :tree vis :vis} i]
              {:tree (rotate tree)
               :vis (rotate (update-map-visibility tree vis))})]
      (reduce dostep initial-state four))))

(def result (generate-map-visibility tree-map (make-visibility-map tree-map)))

(prn (reduce + (map (fn [r] (reduce + r)) (get result :vis))))

(defn viewing-distance [row max]
  (cond (empty? row)
        0
        (>= (first row) max)
        1
        :else
        (+ 1 (viewing-distance (rest row)  max))))

(defn visibility-score [tree-map x y]
  (let [row (nth tree-map y)
        row-components (split-at x row)
        before-row (first row-components)
        tree (first (second row-components))
        after-row (rest (second row-components))
        column (map #(nth %1 x) tree-map)
        col-components (split-at  y column)
        before-col (first col-components)
        after-col (rest (second col-components))]
    (let [scores (map #(viewing-distance %1  tree)
                      [(reverse before-row)
                       after-row
                       (reverse before-col)
                       after-col])]
      (reduce * scores))))


(visibility-score tree-map 1 2)

(def scores (map (fn [y] (map #(visibility-score tree-map %1 y) (range (count tree-map)))) (range (count tree-map))))
(def max-score (apply max (flatten scores)))
(prn max-score)
