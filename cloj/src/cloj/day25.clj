(ns cloj.day25
  (:require [clojure.string :as str]))

(def input "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

;; (def input (slurp "./day25.input"))

(def parsed (str/split-lines input))

(defn parse-digit [d]
  (cond (= d "2")
        2
        (= d "1")
        1
        (= d "0")
        0
        (= d "-")
        -1
        (= d "=")
        -2))

(defn from-snafu
  ([digits] (from-snafu (str/split digits #"") 0))
  ([digits acc]
  ;;  (prn digits acc)
   (if (empty? digits)
     acc
     (let [d (parse-digit (first digits))
           new-acc (+ (* (bigint 5) acc) d)]
       (recur (rest digits) new-acc)))))

(defn to-snafu
  ([num] (to-snafu num []))
  ([num acc]
   (if (= 0 num)
     (if (empty? acc)
       "0"
       (str/join acc))
     (let [mmod (mod num 5)
           ddiv (quot num 5)]
    ;; (prn num mmod ddiv)
       (cond
         (>= 2 mmod)
         (to-snafu ddiv (cons mmod acc))
         (= 3 mmod)
         (to-snafu (+ ddiv 1) (cons "=" acc))
         :else
         (to-snafu (+ ddiv 1) (cons "-" acc)))))))

(map from-snafu parsed)

(to-snafu (reduce + (map from-snafu parsed)))
