(ns day12
  (:require [clojure.core :as core] [clojure.string :as str]))

(def input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

;; (def input (slurp "./day12.input"))

(def raw-map (map vec (str/split-lines input)))

(def ALPHA "abcdefghijklmnopqrstuvwxyz")

(defn height-from-raw [x]
  (let [height
        (cond (= \S x)
              0
              (= \E x)
              25
              :else
              (str/index-of ALPHA x))]
    height))

(def height-map
  (apply vector (map #(apply vector (map height-from-raw %1)) raw-map)))

(defn find-in-map [m v]
  (second
   (first

    (filter #(do (= (first %1) v))
            (reduce concat (map-indexed
                            (fn [y row]

                              (map-indexed #(do [%2 [%1 y]]) row))
                            m))))))

(defn find-all-in-map [m v]
  (map second

       (filter #(do (= (first %1) v))
               (reduce concat (map-indexed
                               (fn [y row]

                                 (map-indexed #(do [%2 [%1 y]]) row))
                               m)))))

(def initial-index (find-in-map raw-map \S))

(map prn height-map)

(def initial-access-map
  (apply vector (map (fn [row] (apply vector (map #(if (= \E %1) 0 nil) row))) raw-map)))


(defn get-from [from [x y]]
  (get (get from y) x))

(defn can-access [source dest]
  ;; (prn "ca" source dest)
  (let [source-height (get-from height-map source)
        dest-height (get-from height-map dest)]
    ;; (prn "ca2" source dest source-height dest-height)
    (if (nil? dest-height)
      false
      (<= dest-height (+ 1 source-height)))))

(defn compute-access [access source dest]
  (let [dest-access (get-from access dest)
        can-reach (can-access source dest)]
    (cond (nil? dest-access)
          nil
          can-reach
          (+ 1 dest-access)
          :else
          nil)))

(defn check-access [access [x y]]
  ;; (prn "checking" [x y])
  (let [current-access (get-from access [x y])
        above-access (compute-access access [x y] [x (- y 1)])
        below-access (compute-access access [x y] [x (+ y 1)])
        left-access (compute-access access [x y] [(- x 1) y])
        right-access (compute-access access [x y] [(+ x 1) y])
        possible-access (filter int? [above-access below-access left-access right-access])
        least-access (if (empty? possible-access) nil (apply min possible-access))]
    ;; (prn [x y] current-access above-access left-access below-access right-access least-access)

    (cond (int? current-access)
          current-access
          (int? least-access)
          least-access
          :else
          nil)))

(defn iterate-access [access]
  (apply vector (map-indexed
                 (fn [y row]
                   (apply vector (map-indexed
                                  (fn [x _access] (check-access access [x y])) row)))
                 access)))

(defn find-min-steps [access possible-points]
  (let [possible-values (map #(get-from access %1) possible-points)]
    (if (every? nil? possible-values)
      (recur (iterate-access access) possible-points)
      (first (filter (comp not nil?) possible-values)))))


(prn (find-min-steps initial-access-map [initial-index]))

(def zeros (find-all-in-map height-map 0))

(prn (find-min-steps initial-access-map zeros))
