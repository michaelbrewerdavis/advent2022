(require 'clojure.string)

(def input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

;; (def input "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(def input (slurp "./day6.input"))

(def cs (clojure.string/split input #""))
(defn diff4 [a b c d] (not (or (= a b) (= a c) (= a d) (= b c) (= b d) (= c d))))


(defn get-index [start-index list] (if (apply diff4 (take 4 list)) (+ start-index 4) (get-index (+ 1 start-index) (rest list))))

(prn (get-index 0 cs))

(defn all-diff? [list]
  (= (count list) (count (set list))))

(defn get-index-n [start-index list n]
  (if (all-diff? (take n list)) (+ start-index n) (get-index-n (+ 1 start-index) (rest list) n)))

(prn (get-index-n 0 cs 14))
