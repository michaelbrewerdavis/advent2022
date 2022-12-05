(require '[clojure.string :as str])
(require '[clojure.core :as core])

;; (def input "2-4,6-8
;; 2-3,4-5
;; 5-7,7-9
;; 2-8,3-7
;; 6-6,4-6
;; 2-6,4-8")

(def input (slurp "./day4.input"))

(defn parse-range [s]
  (let [p (str/split s #"-")]
    (map (fn [x] (Integer/parseInt x)) [(first p) (second p)])))

(defn parse-pair [s]
  (let [p (str/split s #",")]
    (map parse-range p)))

(def lines (str/split-lines input))

(def groups (map parse-pair lines))

(defn contains? [a b]
  (and (<= (first a) (first b))
       (>= (second a) (second b))))

(defn either-contains? [a b]
  (or (contains? a b)
      (contains? b a)))

(def containing (filter (fn [g] (either-contains? (first g) (second g)))  groups))

(prn (count containing))

(defn overlaps? [a b]
  (let [[a1 a2] a
        [b1 b2] b]
    (if (< b1 a1)
      (<= a1 b2)
      (<= b1 a2))))

(def overlapping (filter (fn [g] (overlaps? (first g) (second g))) groups))

(prn (count overlapping))
