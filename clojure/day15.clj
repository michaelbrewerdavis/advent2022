(ns day15
  (:require [clojure.core :as core] [clojure.string :as str]))

;; (def input (slurp "./day15.sample"))
;; (def target-line 10)
;; (def max-coord 20)

(def input (slurp "./day15.input"))
(def target-line 2000000)
(def max-coord 4000000)

(defn parse-line [line]
  (let [[_ sxt syt bxt byt] (re-find #"Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" line)
        [sx sy bx by] (map parse-long [sxt syt bxt byt])]
    {:sensor [sx sy] :beacon [bx by]}))

(def parsed (map parse-line (str/split-lines input)))

(defn count-beacons [line parsed]
  (let [beacons (map #(get %1 :beacon) parsed)]
    (count ((comp dedupe sort) (filter #(= line (second %1)) beacons)))))


(defn find-excluded [line sensor beacon]
  (let [[sx sy] sensor
        [bx by] beacon

        distance (+ (abs (- sx bx)) (abs (- sy by)))
        distance-to-line (abs (- sy line))
        remaining-distance (- distance distance-to-line)]
    (if (< remaining-distance 0)
      nil
      [(- sx remaining-distance) (+ sx remaining-distance)])))

;; (find-excluded 10 [8 7] [2 10])

(defn sorted-intervals-overlap? [left right]
  (let [[_l0 l1] left
        [r0 _r1] right]
    ;; (prn "overlap?" left right)
    (<= r0 l1)))

(defn sorted-intervals-merge [left right]
  (let [[l0 l1] left
        [_r0 r1] right]
    [l0 (max l1 r1)]))

(defn merge-intervals [intervals]
  (reduce (fn [current next]
            (let [last-span (first current)
                  other-spans (rest current)]
              ;; (prn "merging" last-span next)
              (cond (nil? last-span)
                    [next]
                    (sorted-intervals-overlap? last-span next)
                    (cons (sorted-intervals-merge last-span next) other-spans)
                    :else
                    (cons next current))))
          []
          (sort intervals)))

(defn blocked-intervals [parsed target-line]
  (merge-intervals (remove nil? (map #(find-excluded target-line (get %1 :sensor) (get %1 :beacon)) parsed))))

(defn interval-length [x] (+ 1 (abs (- (first x) (second x)))))

(def blocked-count (reduce + (map interval-length (merge-intervals (blocked-intervals parsed target-line)))))

(prn (- blocked-count (count-beacons target-line parsed)))


(defn interval-covered-by? [[l0 l1] [r0 r1]]
  (and (>= l0 r0) (<= l1 r1)))

(defn remove-interval [[l0 l1] [r0 r1]]
  ;; (prn l0 l1 r0 r1)
  (if (< l0 r0)
    [l0 (- r0 1)]
    [(+ r1 1) l1]))

(defn remove-intervals [target to-remove]
  ;; (prn "removing" target to-remove)
  (cond (nil? target) target
        (empty? to-remove) target
        :else
        (let [candidate (first to-remove)]
          (cond (interval-covered-by? target candidate)
                nil
                (apply sorted-intervals-overlap? (sort [target candidate]))
                (recur (remove-interval target candidate) (rest to-remove))
                :else
                (recur target (rest to-remove))))))

(def possible
  (filter #((comp not nil? second) %1)
          (map-indexed  #(do [%1
                              (remove-intervals [0 max-coord] (blocked-intervals parsed %2))])
                        (range (+ 1 max-coord)))))

(let [[found_y [found_x _found_x]] (first possible)]
  (+ found_y (* 4000000 found_x)))
