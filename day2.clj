(require '[clojure.string :as str])


;; (def input "A Y
;;     B X
;;     C Z")

(def input (slurp "day2.input"))

(defn get-lines [input] (map str/trim (str/split-lines input)))

(defn get-moves [line] (str/split line #" "))

(defn beats [abc]
  (if (= abc "A")
    "Y"
    (if (= abc "B")
      "Z"
      "X")))

(defn ties [abc]
  (if (= abc "A")
    "X"
    (if (= abc "B")
      "Y"
      "Z")))

(defn loses [abc]
  (if (= abc "A")
    "Z"
    (if (= abc "B")
      "X"
      "Y")))

(defn get-win-score [move]
  (let [them (first move) me (second move)]
    (cond
      (= me (beats them)) 6
      (= me (ties them)) 3
      :else 0)))

(defn get-move-score [move]
  (let [me (second move)]
    (if (= me "X")
      1
      (if (= me "Y")
        2
        3))))

(defn get-score [x] (+ (get-move-score x) (get-win-score x)))

(def moves (map get-moves (get-lines input)))

(def scores (map get-score moves))

(prn (reduce + scores))

(defn get-my-move [move]
  (let [them (first move) outcome (second move)]
    (cond
      (= outcome "X") (loses them)
      (= outcome "Y") (ties them)
      :else (beats them))))

(defn outcome-to-move [input]
  [(first input) (get-my-move input)])

(def real-moves (map outcome-to-move moves))

(def real-scores (map get-score real-moves))

(prn (reduce + real-scores))
