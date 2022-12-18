(ns day16
  (:require [clojure.core :as core] [clojure.string :as str]))

(def input (slurp "./day16.sample"))

;; (def input (slurp "./day16.input"))

(def start-node "AA")
(def time-limit 30)
;; (def time-limit 26)

(defn parse-line [line]
  (let [[_ valve rate-text dest-text] (re-find #"Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)" line)
        rate (parse-long rate-text)
        dest (str/split dest-text #", ")]
    {:id valve :rate rate :dest dest}))

(def parsed (map parse-line (str/split-lines input)))

(def world (reduce #(assoc %1 (get %2 :id) %2) {} parsed))

(def iterate-path
  (memoize (fn [time loc  open-valves]
            ;;  (prn time loc open-valves flow-rate total-flow)
             (if (= time time-limit)
               0
               (let [go-through-paths (map #(iterate-path (+ time 1) %1 open-valves) (get-in world [loc :dest]))
                     flow-rate (get-in world [loc :rate])
                     time-left (- time-limit time)
                     added-flow (* flow-rate (- time-left 1))]
                 (if (or (= flow-rate 0) (contains? open-valves loc))
                   (apply max go-through-paths)
                   (apply max (cons
                               (+ added-flow (iterate-path (+ time 1) loc (conj open-valves loc)))
                               go-through-paths))))))))

(defn all-pairs [v1 v2]
  (apply concat (map (fn [x] (map #(do [x %1]) v2)) v1)))

(def iterate-path-with-elephant
  (memoize (fn [time [me elephant] open-valves]
             (prn [me elephant] time  open-valves)
             (if (= time time-limit)
               0
               (let [my-dests (get-in world [me :dest])
                     elephant-dests (get-in world [elephant :dest])
                     my-flow-rate (if (contains? open-valves me) 0 (get-in world [me :rate]))
                     eleph-flow-rate (if (contains? open-valves elephant) 0 (get-in world [elephant :rate]))
                     time-left (- time-limit time)
                     my-added-flow (* my-flow-rate (- time-left 1))
                     eleph-added-flow (* eleph-flow-rate (- time-left 1))
                     go-through-paths (map #(iterate-path-with-elephant (+ time 1) %1 open-valves) (all-pairs my-dests elephant-dests))]
                 (cond (and (= my-flow-rate 0) (= eleph-flow-rate 0))
                       (apply max go-through-paths)
                       (= my-flow-rate 0)
                       (apply max (concat
                                   (map #(+ eleph-added-flow (iterate-path-with-elephant (+ time 1) [%1 elephant] (conj open-valves elephant))) my-dests)
                                   go-through-paths))
                       (= eleph-flow-rate 0)
                       (apply max (concat
                                   (map #(+ my-added-flow (iterate-path-with-elephant (+ time 1) [me %1] (conj open-valves me))) elephant-dests)
                                   go-through-paths))
                       (= me elephant)
                       (apply max (concat
                                   (map #(+ my-added-flow (iterate-path-with-elephant (+ time 1) [me %1] (conj open-valves me))) elephant-dests)
                                   go-through-paths))
                       :else
                       (apply max (cons
                                   (+ my-added-flow eleph-added-flow (iterate-path-with-elephant (+ time 1) [me elephant] (conj open-valves me elephant)))
                                   go-through-paths))))))))

;; (iterate-path-with-elephant 0 [start-node start-node] (set []))

(defn map-values [m f]
  (into {} (for [[k v] m] [k (f v k)])))


;; (def paths-from-x
;;   (memoize (fn [x n]
;;              (if (= n 0)
;;                []
;;                (let [next-steps (get-in world [x :dest])
;;                      base (zipmap next-steps (map vector next-steps))
;;                      subnodes (zipmap next-steps (map #(paths-from-x %1 (- n 1)) next-steps))
;;                      subpaths (map-values subnodes #(cons %2 %1))]
;;                  (prn "xx" base "subnodes" subnodes "subpaths" subpaths)
;;                  (apply concat
;;                         subpaths))))))

;; (paths-from-x start-node 2)

(defn merge-things [left right]
  ;; (prn left right)
  (if (<= (count left) (count right))
    left
    right))

(defn pcontains? [a b]
  ;; (prn "contains" a b (some #(= b %1) a))
  (some #(= b %1) a))

(def paths-from-x
  (memoize (fn [x subpath]
            ;;  (prn x n subpath)
             (let [next-steps (get-in world [x :dest])
                   interesting-next-steps (filter #(not (pcontains? subpath %1)) next-steps)]
                ;;  (prn "recurring" x subpath next-steps interesting-next-steps)
               (apply (partial merge-with merge-things)
                      {x subpath}
                      (map #(paths-from-x %1 (conj subpath %1)) interesting-next-steps))))))

(defn find-good-paths [loc open-valves]
  (let [all-paths (paths-from-x loc [])]
    (into {} (filter (fn [[k v]]
                       (and
                        (not (= k loc))
                        (not (pcontains? open-valves k))
                        (> (get-in world [k :rate]) 0))) all-paths))))


(defn travel [time loc open-valves active-path]
  ;; (prn time loc open-valves active-path)
  (let [new-paths (find-good-paths loc open-valves)
        possible-contribution (* (- time-limit (+ time 1)) (get-in world [loc :rate]))]
    (cond (= time time-limit)
          0
          (first active-path)
          (recur (+ time 1) (first active-path) open-valves (rest active-path))

          (and (not (pcontains? open-valves loc)) (> (get-in world [loc :rate]) 0))
          (let [foo (+ possible-contribution (travel (+ time 1) loc (conj open-valves loc) nil))
                bar (apply max (cons 0 (map #(travel time loc open-valves %1) (map second new-paths))))]
            (max foo bar))
          :else
          (let [foo (apply max (cons 0 (map #(travel time loc open-valves %1) (map second new-paths))))]
            foo))))

(travel 0 "AA" #{} nil)
