(ns cloj.day23
  (:require [clojure.string :as str]))

(def input ".....
..##.
..#..
.....
..##.
.....")

(def input "..............
..............
.......#......
.....###.#....
...#...#.#....
....#...##....
...#.###......
...##.#.##....
....#..#......
..............
..............
..............
")

(def input (slurp "./day23.input"))

(def lines (str/split-lines input))
(defn all-pairs [sq1 sq2] (for [i sq1 j sq2] [i j]))

(def world (reduce
            (fn [elves [col row]]
              (if (= \# (nth (nth lines row) col))
                (assoc elves [col row] {})
                elves))
            {}
            (all-pairs
             (range (count (first lines)))
             (range (count lines)))))

(def rules [{:dir [0 -1] :checks [[0 -1] [1 -1] [-1 -1]]}
            {:dir [0 1] :checks [[0 1] [1 1] [-1 1]]}
            {:dir [-1 0] :checks [[-1 -1] [-1 1] [-1 0]]}
            {:dir [1 0] :checks [[1 -1] [1 1] [1 0]]}])

(def all-directions (filter #(not= [0 0] %1) (all-pairs [-1 0 1] [-1 0 1])))

all-directions

(defn add-points [a b]
  (mapv + a b))

(defn apply-rule [pt world rule]
  (let [{:keys [checks dir]} rule]
    (if (empty? (filter #(not (nil? (get world (add-points %1 pt)))) checks))
      (add-points pt dir)
      nil)))

(defn propose-move [pt world rules]
  (if (every? nil? (map #(get world (add-points pt %1)) all-directions))
    (do
      ;; (prn "okay with" pt
      ;;        (not (some? (map #(get world (add-points pt %1)) all-directions)))
      ;;        (map #(get world (add-points pt %1)) all-directions))
      pt)
    (first (filter some? (map #(apply-rule pt world %1) rules)))))

(defn do-round [[world rules _ n]]
  (prn n)
  (let [future-moves (map #(future [%1 (propose-move %1 world rules)]) (keys world))
        moves (map deref future-moves)
        collect-moves (reduce #(assoc %1 %2 (+ 1 (get %1 %2 0))) {} (map second moves))
        ;; moves (reduce #(assoc %1 %2 (propose-move %2 world rules)) {} (keys world))
        ;; _ (prn "m" moves)
        ;; collect-moves (reduce #(assoc %1 %2 (+ 1 (get %1 %2 0))) {} (vals moves))
        ;; _ (prn "c" collect-moves)
        applied-moves
        (map #(if (> (get collect-moves (second %1)) 1)
                [(first %1) (first %1)]
                [(first %1) (second %1)])
             moves)
        ;; _ (prn "a" applied-moves)
        new-world
        (reduce #(assoc %1 %2 {}) {} (map second applied-moves))
        ;; _ (prn "w" new-world)
        n-moves (count (filter #(not= (first %1) (second %1)) applied-moves))
        _ (prn "first?" n-moves)
        done (= 0 n-moves)
        ;; _ (prn "d" done)
        ]
    ;; (prn "DONE?" done)
    [new-world (concat (rest rules) [(first rules)]) done (+ 1 n)]))

(def after-10 (last (take 11 (iterate do-round [world rules false 0]))))

after-10
(prn "after 10" (let [x-span (reduce #(do
                                        [(min (first %1) (first %2))
                                         (max (second %1) (first %2))])
                                     [##Inf ##-Inf] (keys (first after-10)))

                      y-span
                      (reduce #(do [(min (first %1) (second %2))
                                    (max (second %1) (second %2))])
                              [##Inf ##-Inf] (keys (first after-10)))
                      width (+ 1 (- (second x-span) (first x-span)))
                      height (+ 1 (- (second y-span) (first y-span)))
                      area (* width height)
                      empty-space (- area (count world))]
                  empty-space))

(last (first (drop-while #(not (second (next %1))) (iterate do-round [world rules false 0]))))

(count world)

(count lines)

(defn get-bounds [world]
  (let [x-span (reduce #(do
                          [(min (first %1) (first %2))
                           (max (second %1) (first %2))])
                       [##Inf ##-Inf] (keys world))

        y-span
        (reduce #(do [(min (first %1) (second %2))
                      (max (second %1) (second %2))])
                [##Inf ##-Inf] (keys world))]
    [x-span y-span]))

(defn print-world [world]
  (let [[[x0 x1] [y0 y1]] (get-bounds world)
        row (vec (map (constantly ".") (range x0 (+ 1 x1))))
        rows (vec (map (constantly row) (range y0 (+ 1 y1))))
        _ (prn (type row) (type rows))
        filled
        (reduce (fn [w [x y]]
                  (doall (assoc w (- y y0) (doall (assoc (get w (- y y0)) (- x x0) "#"))))) rows (keys world))]
    (map #(str/join %1) filled)))

(print-world world)

world
(range -4 4)
