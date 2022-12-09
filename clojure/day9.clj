(ns day9
  (:require [clojure.core :as core] [clojure.string :as str]))


;; (def input "R 4
;; U 4
;; L 3
;; D 1
;; R 4
;; D 1
;; L 5
;; R 2")

;; (def input "R 5
;; U 8
;; L 8
;; D 3
;; R 17
;; D 10
;; L 25
;; U 20")

(def input (slurp "./day9.input"))

(def rope-length 2)

;; (def make-int int)
;; (defn abs [x] (if (< x 0) (- x) x))
(def make-int parse-long)

(def parsed (map #(do [(first %1) (make-int (second %1))]) (map #(str/split %1 #"\s") (str/split-lines input))))

(defn sign [x] (cond (< x 0) -1 (= x 0) 0 :else 1))

(defn move-one-tail [head tail]
  (let [[hx hy] head
        [tx ty] tail]
    ;; (prn "one-tail" head tail)
    (cond (= hx tx)
          (if (> (abs (- hy ty)) 1)
            [tx (/ (+ hy ty) 2)]
            [tx ty])
          (= hy ty)
          (if (> (abs (- hx tx)) 1)
            [(/ (+ hx tx) 2) ty]
            [tx ty])
          (and (> 2 (abs (- hx tx))) (> 2 (abs (- hy ty))))
          [tx ty]
          :else
          [(+ tx (sign (- hx tx)))
           (+ ty (sign (- hy ty)))])))

(defn fix-rope [head tail]
  (reductions move-one-tail head tail))

(defn move-rope-once [[head & tail] dir]
  (let [[hx hy] head
        new-hx (cond (= dir "L")
                     (- hx 1)
                     (= dir "R")
                     (+ hx 1)
                     :else
                     hx)
        new-hy (cond (= dir "U")
                     (+ hy 1)
                     (= dir "D")
                     (- hy 1)
                     :else
                     hy)
        new-head [new-hx new-hy]
        new-rope (fix-rope new-head tail)]
    ;; (prn head tail dir new-head new-rope)
    new-rope))

(defn move-rope [rope [dir n]]
  ;; (prn dir n rope)
  (if (= n 0)
    rope
    (recur (move-rope-once rope dir) [dir (- n 1)])))

(defn splat-instructions
  ([instr] (splat-instructions instr []))
  ([instr acc]
  ;;  (prn (empty? instr) (count acc))
   (if (empty? instr)
     (reverse acc)
     (let [[next-instr next-n] (first instr)]
       (if (= 0 next-n)
         (recur (rest instr) acc)
         (recur (cons [next-instr (- next-n 1)] (rest instr))
                (cons [next-instr 1] acc)))))))

(def splatted (splat-instructions parsed))

(def origin (repeat rope-length [0 0]))

(def all-points (reductions #(move-rope %1 %2) origin splatted))

(count (set (map #(nth %1 (- rope-length 1)) all-points)))
