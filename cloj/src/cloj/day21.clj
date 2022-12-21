(ns cloj.day21 (:require [clojure.core :as core] [clojure.string :as str]))

(def input "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

;; (def input (slurp "day21.input"))

(re-seq #"([a-z]+): ([a-z0-9]+)( (.) ([a-z]+))?", input)

(def monkeys (reduce
              (fn [monkeys [_ key left _ op right]]
                ;; (prn key left op right)
                (assoc monkeys key
                       (if (nil? op)
                         {:type "number" :value (parse-long left)}
                         {:type "op" :op op :left left :right right})))
              {}
              (re-seq #"([a-z]+): ([a-z0-9]+)( (.) ([a-z]+))?", input)))


(def monkey-names (set (keys monkeys)))

(defn sort-monkeys [stack remaining sorted]
  ;; (prn stack)
  (cond (and (empty? stack) (empty? remaining))
        sorted
        (empty? stack)
        (recur [(first remaining)] (disj remaining (first remaining)) sorted)
        :else
        (let [top (first stack)
              top-value (get monkeys top)
              {:keys [type left right]} top-value]
          (cond (= type "number")
                (recur (rest stack) remaining (conj sorted top))
                (contains? remaining left)
                (recur (cons left stack) (disj remaining left) sorted)
                (contains? remaining right)
                (recur (cons right stack) (disj remaining right) sorted)
                :else
                (recur (rest stack) remaining (conj sorted top))))))


(def sorted-monkeys (sort-monkeys [] monkey-names []))

(defn get-op [op]
  (cond (= op "/")
        /
        (= op "+")
        +
        (= op "-")
        -
        :else
        *))

(defn apply-monkeys [previous-values monkey-name]
  (let [monkey (get monkeys monkey-name)
        {:keys [type value op left right]} monkey]
    ;; (prn monkey-name value op left right)
    (if (= type "number")
      (assoc previous-values monkey-name value)
      (assoc previous-values monkey-name
             ((get-op op) (get previous-values left) (get previous-values right))))))

(def monkey-values (reduce apply-monkeys {} sorted-monkeys))
;; (sort-monkeys "root" (set (keys monkeys)) [])
(prn (get monkey-values "root"))

(def augmented-monkeys
  (assoc monkeys
         "humn" {:type "human"}
         "root" (assoc (get monkeys "root") :op "=")))

(defn apply-monkeys-2 [previous-values monkey-name]
  (let [monkey (get augmented-monkeys monkey-name)
        ;; _ (prn "monkey" monkey)
        {:keys [type value op left right]} monkey]
    ;; (prn monkey-name value op left right)
    (cond (= type "number")
          (assoc previous-values monkey-name value)
          (= type "human")
          (assoc previous-values monkey-name ["human"])
          :else
          (let [left-value (get previous-values left)
                right-value (get previous-values right)
                ;; _ (prn left-value right-value)
                new-value (cond (or (not (number? left-value)) (not (number? right-value)))
                                [op left-value right-value]
                                :else
                                ((get-op op) (get previous-values left) (get previous-values right)))
                ;; _ (prn new-value)
                ]
            (assoc previous-values monkey-name new-value)))))

(def monkey-values-2 (reduce apply-monkeys-2 {} sorted-monkeys))

(def root-value (get monkey-values-2 "root"))

(defn invert-ops [input acc]
  ;; (prn input acc)
  (let [[op left right] input
        [n h] (if (number? left) [left  right] [right left])]
    ;; (prn op left right n h)
    (cond (= op "human")
          acc
          (= op "=")
          (recur h n)
          (= op "+")
          (recur h (- acc n))
          (= op "*")
          (recur h (/ acc n))
          (= op "-")
          (if (number? left)
            (recur h (- n acc))
            (recur h (+ acc n)))
          (= op "/")
          (if (number? left)
            (recur h (/ n acc))
            (recur h (* acc n))))))


(invert-ops ["-" ["human"] 3] 0)

(invert-ops ["=" ["/" ["+" 4 ["*" 2 ["-" ["human"] 3]]] 4] 150] 0)

(prn (invert-ops root-value 0))
