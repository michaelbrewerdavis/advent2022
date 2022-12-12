(ns day11
  (:require [clojure.core :as core] [clojure.string :as str]))

;; (defn bigint [x] (js/BigInt x))

(def input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * ol
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(def factor 96577)

;; (def input "Monkey 0:
;;   Starting items: 63, 84, 80, 83, 84, 53, 88, 72
;;   Operation: new = old * 11
;;   Test: divisible by 13
;;     If true: throw to monkey 4
;;     If false: throw to monkey 7

;; Monkey 1:
;;   Starting items: 67, 56, 92, 88, 84
;;   Operation: new = old + 4
;;   Test: divisible by 11
;;     If true: throw to monkey 5
;;     If false: throw to monkey 3

;; Monkey 2:
;;   Starting items: 52
;;   Operation: new = old * old
;;   Test: divisible by 2
;;     If true: throw to monkey 3
;;     If false: throw to monkey 1

;; Monkey 3:
;;   Starting items: 59, 53, 60, 92, 69, 72
;;   Operation: new = old + 2
;;   Test: divisible by 5
;;     If true: throw to monkey 5
;;     If false: throw to monkey 6

;; Monkey 4:
;;   Starting items: 61, 52, 55, 61
;;   Operation: new = old + 3
;;   Test: divisible by 7
;;     If true: throw to monkey 7
;;     If false: throw to monkey 2

;; Monkey 5:
;;   Starting items: 79, 53
;;   Operation: new = old + 1
;;   Test: divisible by 3
;;     If true: throw to monkey 0
;;     If false: throw to monkey 6

;; Monkey 6:
;;   Starting items: 59, 86, 67, 95, 92, 77, 91
;;   Operation: new = old + 5
;;   Test: divisible by 19
;;     If true: throw to monkey 4
;;     If false: throw to monkey 0

;; Monkey 7:
;;   Starting items: 58, 83, 89
;;   Operation: new = old * 19
;;   Test: divisible by 17
;;     If true: throw to monkey 2
;;     If false: throw to monkey 1")

;; (def factor 9699690)

(def expr #"Starting items: ([0-9, ]+)
\s*Operation: new = (.*)
\s*Test: divisible by (\d+)
\s*If true: throw to monkey (\d+)
\s*If false: throw to monkey (\d+)")

(defn read-op [input]
  (let [[x op y] (str/split input #" ")
        resolve-arg (fn [arg old]  (if (= arg "old") old (bigint arg)))
        resolved-op (if (= op "*") * +)]
    (fn [old]
      (mod
       (resolved-op (resolve-arg x old) (resolve-arg y old))
       factor))))


(defn read-monkey [match]
  (let [[_ items operation test on-true on-false] match]
    {:items
     (map bigint (str/split items #", "))
     :takes
     0
     :optext
     operation
     :op
     (read-op operation)
     :test
     (bigint test)
     :on-true
     (parse-long on-true)
     :on-false
     (parse-long on-false)}))

(def initial-monkeys
  (vec (map (comp read-monkey (partial re-find expr))
            (str/split input #"\n\n"))))

(defn take-item [monkeys i]
  (let [current-monkey (get monkeys i)
        current-items (get current-monkey :items)
        item (first current-items)
        new-items (rest current-items)
        new-current-monkey
        (assoc current-monkey
               :items new-items
               :takes (+ 1 (get current-monkey :takes)))
        new-monkeys (assoc monkeys i new-current-monkey)]
    [new-monkeys item]))

(defn give-item [monkeys i item]
  (let [current-monkey (get monkeys i)
        current-items (get current-monkey :items)
        new-items (concat current-items [item])
        new-current-monkey
        (assoc current-monkey :items new-items)
        new-monkeys (assoc monkeys i new-current-monkey)]
    new-monkeys))

(defn do-item [monkeys i]
  (let [current-monkey (get monkeys i)
        current-op (get current-monkey :op)
        [new-monkeys item] (take-item monkeys i)
        worried-item (current-op item)
        ;; relieved-item (quot worried-item 3)
        relieved-item worried-item
        divisible (= (bigint 0) (mod relieved-item (get current-monkey :test)))
        give-to-monkey (if divisible (get current-monkey :on-true) (get current-monkey :on-false))]
    ;; (prn "item" item worried-item relieved-item divisible give-to-monkey)
    (give-item new-monkeys give-to-monkey relieved-item)))

(defn do-turn [monkeys i]
  (let [current-monkey (nth monkeys i)]
    (if (empty? (get current-monkey :items))
      monkeys
      (recur (do-item monkeys i) i))))


(defn do-round [monkeys]
  (reduce do-turn monkeys (range (count monkeys))))


(def after-rounds (reduce (fn [m n]
                            (if (= 0 (mod n 10))
                              (prn n)
                              nil)
                            (do-round m)) initial-monkeys (range 10000)))
(prn after-rounds)

(def inspections (sort > (map #(get %1 :takes) after-rounds)))
(def monkey-business (reduce * (take 2 inspections)))
(prn monkey-business)
