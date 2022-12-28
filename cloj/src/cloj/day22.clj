(ns cloj.day22
  (:require [clojure.set :as set]
            [clojure.string :as str]))

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

(def input (slurp "day22.input"))

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

(defn print-map-with-point [pt]
  ;; (prn)
  ;; (prn pt)
  ;; (doall (map prn (assoc initial-map (second pt)
  ;;                        (str/join (assoc (vec (nth initial-map (second pt))) (first pt) "O")))))
  nil)

(defn find-other-side [[x y] [dx dy]]
  (cond (= dx 1)
        [(find-map-pos [0 y] [dx dy]) [dx dy]]
        (= dy 1)
        [(find-map-pos [x 0] [dx dy]) [dx dy]]
        (= dx -1)
        [(find-map-pos [(dec (count (nth initial-map y))) y] [dx dy]) [dx dy]]
        :else
        [(find-map-pos [x (dec (count initial-map))] [dx dy]) [dx dy]]))

(defn move-position [[pt dir] n find-other-side]
  ;; (prn "mp" pt dir n)
  (let [new-pt (mapv + dir pt)
        new-value (get-in-map new-pt)]
    ;;(prn new-pt new-value)
    (cond (= 0 n)
          [pt dir]
          (= \# new-value)
          [pt dir]
          (or (nil? new-value) (= \space new-value))
          (let [[wrapped-pt wrapped-dir] (find-other-side new-pt dir)
                wrapped-pt-value (get-in-map wrapped-pt)]
            (if (= \# wrapped-pt-value)
              [pt dir]
              (recur [wrapped-pt wrapped-dir] (- n 1) find-other-side)))
          :else
          (recur [new-pt dir] (- n 1) find-other-side))))

(defn rotate-direction [[pt [dx dy]] rotation]
  ;;(prn "rd" pt dx dy rotation)
  (if (= rotation "R")
    [pt [(- dy) dx]]
    [pt [dy (- dx)]]))

(defn follow-instructions [[position direction] instructions find-other-side]
  ;; (prn "top" position direction (first instructions))
  (print-map-with-point position)
  (if  (empty? instructions)
    [position direction]
    (let [current (first instructions)]
      ;;(prn "down here" current)
      (if (number? current)
        (do ;;(prn "hi") 
          (recur (move-position [position direction] current find-other-side)
                 (rest instructions)
                 find-other-side))
        (do ;;(prn "bye" [position direction] (rotate-direction [position direction] current))
          (recur (rotate-direction [position direction] current)
                 (rest instructions)
                 find-other-side))))))

;; (def foo (follow-instructions [initial-pos initial-dir] instructions find-other-side))

;; (prn foo (+ (* 1000 (+ 1 ((comp second first) foo)))
;;             (* 4 (+ 1 (ffirst foo)))))

(def square-size (int (Math/sqrt (/ (count (filter #(not (= \space %1)) (flatten (map vec initial-map)))) 6))))

(def faces (let [rows (partition  square-size initial-map)]
             (map (fn [row]
                    (let [num-cols (count (partition square-size (first row)))
                          cols (map #(partition square-size %1) row)]
                      (map (fn [n]

                             (map #(nth %1 n) cols)) (range num-cols)))) rows)))

(def c2 [[[]
          []
          [[0 0 0] [1 0 0]  [0 1 0]]]
         [[[3 0 0] [-1 0 0] [0 0 1]]
          [[0 0 0] [0 1 0] [0 0 1]]
          [[0 3 0] [1 0 0] [0 0 1]]]
         [[]
          []
          [[0 3 3] [1 0 0] [0 -1 0]]
          [[3 3 3] [0 0 -1] [0 -1 0]]]])

(def max-index (- square-size 1))

(def c2 [[[]
          [[0 0 0] [1 0 0] [0 1 0]] ;; xy
          [[max-index 0 0] [0 0 1] [0 1 0]]] ;; yz
         [[]
          [[0 max-index 0] [1 0 0] [0 0 1]]] ;; xz
         [[[0 max-index 0] [0 0 1] [0 -1 0]] ;; yz
          [[0 max-index max-index] [1 0 0] [0 -1 0]]] ;; xy
         [[[0 0 0] [0 0 1] [1 0 0]]]]) ;; xz

(def indexed-c2 (apply concat (keep-indexed (fn [n v] (map #(do [[(first %1) n] (second %1)]) (keep-indexed #(do [%1 %2]) v))) c2)))

indexed-c2
(defn between-points? [[x0 y0 z0] [x1 y1 z1] [xc yc zc]]
  ;; (prn "check" [x0 x1 xc] [y0 y1 yc] [z0 z1 zc])
  (and (>= xc (min x0 x1))
       (<= xc (max x0 x1))
       (>= yc (min y0 y1))
       (<= yc (max y0 y1))
       (>= zc (min z0 z1))
       (<= zc (max z0 z1))))

(defn in-plane? [[origin dx dy] pt]
  ;; (prn "ip" origin dx dy pt)
  (between-points?
   origin
   (mapv +
         origin
         (mapv * (repeat (- square-size 1)) dx)
         (mapv * (repeat (- square-size 1)) dy))
   pt))


(defn find-plane-index [plane]
  (ffirst (filter #(= (second %1) plane) indexed-c2)))

(find-plane-index [[0 3 0] [1 0 0] [0 0 1]])

(defn get-dim [[dx dy dz]]
  (cond (not= 0 dx)
        :x
        (not= 0 dy)
        :y
        :else
        :z))

(defn divide-dir [[x0 y0 z0] [x1 y1 z1]]
  (cond (not= x1 0)
        (/ x0 x1)
        (not= y1 0)
        (/ y0 y1)
        (not= z1 0)
        (/ z0 z1)))

(defn unit-vector [dir]
  (cond (= dir :x)
        [1 0 0]
        (= dir :y)
        [0 1 0]
        :else
        [0 0 1]))

(defn project-dir [pt dir]
  (mapv * (unit-vector dir) pt))

(defn get-plane-dims [[x dx dy]]
  ;; (prn "gpd" x dx dy)
  #{(get-dim dx) (get-dim dy)})

(defn get-new-plane-info [[origin dx dy] coords current-dir new-dir]
  (let [delta (mapv - coords origin)
        new-coords [(divide-dir delta dx)
                    (divide-dir delta dy)]
        new-delta (if (= (get-dim dx) new-dir) dx dy)
        new-sign (if (= (project-dir coords new-dir) (project-dir origin new-dir)) 1 -1)
        new-magnitude (if (= (get-dim dx)  new-dir) [new-sign 0] [0 new-sign])]

    [new-coords new-magnitude]))


(defn find-opposite-on-cube [[next-x next-y] [dx dy]]
  ;; (prn "opp" next-x next-y dx dy)
  (let [x (- next-x dx)
        y (- next-y dy)
        side-index-x (quot x square-size)
        side-index-y (quot y square-size)
        local-plane (nth (nth c2 side-index-y) side-index-x)
        local-index-x (mod x square-size)
        local-index-y (mod y square-size)
        [origin x-spec y-spec] local-plane
        current-dir (if (not= dx 0) x-spec y-spec)
        cube-coords (mapv + origin
                          (map (partial * local-index-x) x-spec)
                          (map (partial * local-index-y) y-spec))
        possible-planes (filter #(in-plane? %1 cube-coords) (filter (comp not empty?) (apply concat c2)))
        new-plane (first (filter #(not (contains? (get-plane-dims %1) (get-dim current-dir))) possible-planes))
        ;; _ (prn "hhh" local-plane cube-coords current-dir possible-planes new-plane)
        new-direction (first (set/difference (get-plane-dims new-plane) (get-plane-dims local-plane)))
        [new-plane-x new-plane-y] (find-plane-index new-plane)
        [new-local-coords new-delta] (get-new-plane-info new-plane cube-coords current-dir new-direction)
        new-coords (mapv + new-local-coords
                         [(* new-plane-x square-size) (* new-plane-y square-size)])
        ;; _ (prn cube-coords current-dir new-plane new-plane-x new-plane-y new-coords new-direction)
        ]

    ;; (prn "moving across edge" [x y] [dx dy])
    (print-map-with-point [x y])
    ;; (prn "after" new-coords new-delta)
    (print-map-with-point new-coords)
    [new-coords new-delta]))


(map get-plane-dims [[[3 0 0] [-1 0 0] [0 0 1]] [[0 3 3] [1 0 0] [0 -1 0]]])

(def foo2 (follow-instructions [initial-pos initial-dir] instructions find-opposite-on-cube))

(prn foo2 (+ (* 1000 (+ 1 ((comp second first) foo2)))
             (* 4 (+ 1 (ffirst foo2)))))

;; (filter #(in-plane? %1 [49 2 0]) (filter (comp not empty?) (apply concat c2)))
