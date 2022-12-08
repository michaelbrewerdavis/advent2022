(require '[clojure.string :as str])
(require '[clojure.core :as core])

;; (def input "vJrwpWtwJgWrhcsFMMfFFhFp
;; jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
;; PmmdzqPrVvPwwTWBwg
;; wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
;; ttgJtRGJQctTZtZT
;; CrZsJsPPZsGzwwsLwLmpwMDw")

(def input (slurp "day3.input"))

(def lines (map str/trim (str/split-lines input)))

(defn split-in-half [s]
  (let [len (count s)]
    [(subs s 0 (/ len 2))
     (subs s (/ len 2))]))

(split-in-half "abcd")

(def packs (map split-in-half lines))

(defn str-includes [str char]
  (not (nil? (str/index-of str char))))

(defn find-dup [pack]
  (let [a (first pack) b (second pack)]
    (letfn [(matches-b [char]
              (not (nil? (str/index-of b char))))]
      (first (filter matches-b (.toCharArray a))))))


(def dups (map find-dup packs))

(def code-for-a (.codePointAt "a" 0))
(def code-for-A (.codePointAt "A" 0))


(defn score [char]
  (let [char-code (int char)]
    (if (>= char-code code-for-a)
      (+ 1 (- char-code code-for-a))
      (+ 27 (- char-code code-for-A)))))


(def total-score (reduce + (map score dups)))
(prn total-score)


(def groups (partition 3 lines))

(def group (first groups))

(defn common-char [group]
  (let [g1 (first group) g2 (second group) g3 (last group)]
    (first
     (filter
      (fn [c]
        (and
         (str-includes g2 c)
         (str-includes g3 c)))
      (.toCharArray g1)))))

(def total-score-2 (reduce + (map score (map common-char groups))))
(prn total-score-2)
