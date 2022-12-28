(ns cloj.day22
  (:require [clojure.string :as str]))

(def input "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

;; (def input (slurp "day22.input"))

(def parsed
  (let [[map-input directions]  (str/split  input #"\n\n")]
    [(str/split-lines map-input) directions]))

(def initial-map (first parsed))
(def instructions (map (fn [[_ _ n d]]
                         (if (nil? n) d (parse-long n))) (re-seq #"((\d+)|([RL])+)" (second parsed))))

(def initial-dir [1 0])

(defn get-in-map [[x y]]
  ;;(prn "gip" x y)
  (if (or (< y 0)
          (< x 0)
          (>= y (count initial-map))
          (>= x (count (nth initial-map y))))
    nil
    (nth (nth initial-map y) x)))

(defn find-map-pos [pt dir]
  ;;(prn "find-map-pos" pt dir)
  (if (= \space (get-in-map pt))
    (recur (mapv + pt dir) dir)
    pt))


(def initial-pos (find-map-pos [0 0] [1 0]))

instructions

(defn find-other-side [[x y] [dx dy]]
  (cond (= dx 1)
        (find-map-pos [0 y] [dx dy])
        (= dy 1)
        (find-map-pos [x 0] [dx dy])
        (= dx -1)
        (find-map-pos [(dec (count (nth initial-map y))) y] [dx dy])
        :else
        (find-map-pos [x (dec (count initial-map))] [dx dy])))

(find-other-side [0 11] [0 -1])
(get-in-map [11 0])
(defn move-position [[pt dir] n]
  ;;(prn "mp" pt dir n)
  (let [new-pt (mapv + dir pt)
        new-value (get-in-map new-pt)]
    ;;(prn new-pt new-value)
    (cond (= 0 n)
          [pt dir]
          (= \# new-value)
          [pt dir]
          (or (nil? new-value) (= \space new-value))
          (recur [(find-other-side new-pt dir) dir] (- n 1))
          :else
          (recur [new-pt dir] (- n 1)))))

(defn rotate-directionx [[pt delta] b]
  ;;(prn "rd" pt delta b)
  [pt delta])

(defn rotate-direction [[pt [dx dy]] rotation]
  ;;(prn "rd" pt dx dy rotation)
  (if (= rotation "R")
    [pt [(- dy) dx]]
    [pt [dy (- dx)]]))

(defn follow-instructions [[position direction] instructions]
  ;;(prn "top" position direction instructions)
  (if  (empty? instructions)
    [position direction]
    (let [current (first instructions)]
      ;;(prn "down here" current)
      (if (number? current)
        (do ;;(prn "hi") 
          (recur (move-position [position direction] current)
                 (rest instructions)))
        (do ;;(prn "bye" [position direction] (rotate-direction [position direction] current))
          (recur (rotate-direction [position direction] current)
                 (rest instructions)))))))


(get-in-map [5 5])

(def foo (follow-instructions [initial-pos initial-dir] instructions))


(prn foo (+ (* 1000 (+ 1 ((comp second first) foo)))
            (* 4 (+ 1 (ffirst foo)))))


(def square-size (int (Math/sqrt (/ (count (filter #(not (= \space %1)) (flatten (map vec initial-map)))) 6))))

square-size

(def faces (let [rows (partition  square-size initial-map)]
             (map (fn [row]
                    (let [num-cols (count (partition square-size (first row)))
                          cols (map #(partition square-size %1) row)]
                      (map (fn [n]

                             (map #(nth %1 n) cols)) (range num-cols)))) rows)))

(def sides [:top :bottom :front :back :left :right])

(defn get-face [row col] (nth (nth faces row) col))

(defn assign-faces [assigned leftover]
  (map (fn [row] (map (fn [col] [row col]) (nth faces row))) faces))

;;   /  A  B                   /  f  /            /  f  /
;;   /  C  /                  |   a | b          |  d' | e
;;   E  D  /                  |     |/           |     |
;;   F                            c                 c


A to right => B -- no change
A down => C -- no change
A left => E   --
