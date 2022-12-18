(ns day17
  (:require [clojure.core :as core] [clojure.string :as str]))

;; (def wind ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def wind (slurp "./day17.input"))

(def wind-iter (cycle wind))

(def minus [[0 0] [1 0] [2 0] [3 0]])
(def plus [[1 0] [0 1] [1 1] [2 1] [1 2]])
(def ell [[0 0] [1 0] [2 0] [2 1] [2 2]])
(def bar [[0 0] [0 1] [0 2] [0 3]])
(def square [[0 0] [0 1] [1 0] [1 1]])

(def shape-iter (cycle [minus plus ell bar square]))

(def initial-world {:rows [] :wind wind-iter :shapes shape-iter :current-shape nil :current-pos nil})

(defn print-world [{:keys [rows wind current-shape current-pos]}]
  (prn)
  (prn "world...")
  (doall (map (comp prn str/join) (reverse rows)))
  (prn "shape..." current-pos)
  (prn current-shape)
  (prn "next wind" (first wind))
  (prn))

(defn pos-for-new [world-height]
  [2, (+ 3  world-height)])

(defn point-okay [rows point]
  (let [[x y] point]
    (not (or (< x 0)
             (> x 6)
             (< y 0)
             (and (< y (count rows))
                  (= "#" (nth (nth rows y) x)))))))

(defn can-move? [rows delta pos shape]
  (let [new-pos (mapv + pos delta)
        new-shape (map #(mapv + new-pos %1) shape)]
    (every? #(point-okay rows %1) new-shape)))

(defn move-for-wind [rows wind pos shape]
  (let [delta (if (= wind \<) [-1 0] [1 0])]
    (if (can-move? rows delta pos shape)
      (mapv + pos delta)
      pos)))

(defn lock-in-shape [rows shape pos]
  (let [final-shape (map #(mapv + %1 pos) shape)
        max-y (apply max (map second final-shape))
        full-length-rows
        (vec (concat rows (repeat (- (+ 1 max-y) (count rows)) ["-" "-" "-" "-" "-" "-" "-"])))]
    (reduce (fn [r [x y]]
              (assoc r y (assoc (nth r y) x "#"))) full-length-rows final-shape)))

(defn move [world]
  (let [{:keys [rows wind shapes current-shape current-pos]} world
        new-shape? (nil? current-shape)
        shape (if new-shape? (first shapes) current-shape)
        next-shapes (if new-shape? (next shapes) shapes)
        next-wind (next wind)
        wind (first wind)
        initial-pos (if new-shape? (pos-for-new (count rows)) current-pos)
        wind-pos (move-for-wind rows wind initial-pos shape)]
    (if (can-move? rows [0 -1] wind-pos shape)
      (recur {:rows rows :wind next-wind :shapes next-shapes :current-shape shape :current-pos (mapv + [0 -1] wind-pos)})

      (do  {:rows (lock-in-shape rows shape wind-pos)
            :wind next-wind
            :shapes next-shapes
            :current-shape nil
            :current-pos nil}))))

(defn world-at [n] (last (take (+ n 1) (iterate move initial-world))))

(defn world-height [x] (count (get (world-at x) :rows)))

(prn (world-height 2022))

(defn estimated-world-height [x]
  (+ (world-height (+ 718 (mod (- x 718) 1755)))
     (* 2747 (quot (- x 718)  1755))))

(prn (estimated-world-height 1000000000000))
