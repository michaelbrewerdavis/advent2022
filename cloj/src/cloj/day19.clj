(ns cloj.day19 (:require [clojure.core :as core] [clojure.string :as str]))

(def input "Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.")

(def matcher
  (re-seq
   #"Blueprint (\d+): Each ore robot costs (\d+) ore.\s+Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
   (str/replace input #"(?m)\s+" " ")))

(str/replace input #"(?m)\s+" " ")

(def blueprints
  (map  (fn [[_full id ore-ore clay-ore obs-ore obs-clay geode-ore geode-obs]]
          [[(parse-long ore-ore) 0 0 0]
           [(parse-long clay-ore) 0 0 0]
           [(parse-long obs-ore) (parse-long obs-clay) 0 0]
           [(parse-long geode-ore) 0 (parse-long geode-obs) 0]])
        matcher))

(def b1 (first blueprints))

(def initial-state [[1 0 0 0]   ; robots
                    [0 0 0 0]]) ; supplies

;; (defn can-build? [blueprint state n]
;;   (let [[_ supplies] state
;;         cost (nth blueprint n)]
;;     (every? true? (mapv #(>= %1 %2) supplies cost))))

(defn nth-1 [n]
  (assoc [0 0 0 0] n 1))

;; (defn build [blueprint state n]
;;   (let [[robots supplies] state
;;         cost (nth blueprint n)
;;         new-supplies (mapv - supplies cost)
;;         new-robots (mapv + robots (nth-1 n))]
;;     [new-robots new-supplies]))

;; (defn do-minute [blueprint state]
;;   (let [[robots supplies] state
;;         _ (prn "before" robots supplies)
;;         build-robot? (can-build? blueprint state 0)
;;         [next-robots post-purchase-supplies] (if build-robot? (build blueprint state 0) state)
;;         next-supplies (mapv + robots post-purchase-supplies)
;;         _ (prn "after" build-robot? post-purchase-supplies next-robots next-supplies)]
;;     [next-robots
;;      next-supplies]))

;; (defn get-next-states [blueprint state]
;;   (let [[old-robots _old-supplies] state]
;;     (map #(do [(first %1) (mapv + old-robots (second %1))])
;;          (filter (comp not nil?)
;;                  (cons state
;;                        (map (fn [n]
;;                               (if (can-build? blueprint state n)
;;                                 (build blueprint state n)
;;                                 nil))
;;                             (range 4)))))))

;; (defn get-geodes [state]
;;   (last (second state)))

;; (defn zero-geodes [state]
;;   [(first state) (assoc (second state) 3 0)])

;; (zero-geodes [[1 2 3 4] [1 2 3 4]])
;; (def find-max
;;   (memoize
;;    (fn [blueprint state n]
;;     ;;  (prn "checking" blueprint state n)
;;      (if (= n 0)
;;        (get-geodes state)
;;        (let [next-states (get-next-states blueprint state)
;;              next-state-geodes (map get-geodes next-states)
;;              zeroed-states (map zero-geodes next-states)
;;              new-values (map #(find-max blueprint %1 (- n 1)) zeroed-states)
;;              fixed-values (mapv + new-values next-state-geodes)]
;;          (apply max
;;                 fixed-values))))))

;; (prn "trying")
;; ;; (find-max b1 initial-state 24)

;; ;; (doall (get-next-states b1  [[3 0 0 0] [1 0 0 0]]))
;; ;; ;; (take 10 (iterate #(do-minute b1 %1) initial-state))

;; ;; (first blueprints)




(defn get-possible-next-robots [state]
  (let [[[_ore clay obsidian _geode]] state]
    (cond (pos? obsidian)
          (range 4)
          (pos? clay)
          (range 3)
          :else
          (range 2))))

;; (defn get-time-to-next-robot [blueprint state n]
;;   (let [requirement (nth blueprint n)
;;         [delta current] state
;;         times-per-resource  (map (comp int #(Math/ceil %1))  (mapv / (mapv - requirement current) (map #(max 1 %1) delta)))]
;;     (max 1 (apply max times-per-resource))))

;; (get-time-to-next-robot [[1 2 3 4] [4 2 1 1] [1 1 1 1] [2 2 2 2]]
;;                         [[1 1 1 1] [2 1 2 1]]
;;                         2)

;; (defn filtered-next-robots [blueprint state time]
;;   (filter #(<= (get-time-to-next-robot blueprint state %1) time) (get-possible-next-robots state)))

;; (filtered-next-robots [[1 2 3 4] [4 2 1 1] [1 1 1 1] [2 2 2 2]]
;;                       [[1 1 1 1] [2 1 2 1]]
;;                       0)

;; (defn advance-state [state time]
;;   (let [[robots supplies] state]
;;     [robots (mapv + (map #(* time %1)  robots) supplies)]))

;; (advance-state  [[1 1 1 1] [2 1 2 1]] 3)

;; (defn purchase-robot [blueprint state n]
;;   (let [requirement (nth blueprint n)
;;         [robots supplies] state]
;;     [(mapv + robots (nth-1 n))
;;      (mapv - supplies requirement)]))

;; (defn find-max-2 [blueprint state time]
;;   ;; (prn)
;;   ;; (println "hello" blueprint state time)
;;   (if (= 0 time)
;;     (do
;;       ;; (prn blueprint state)
;;       (get-geodes state))
;;     (let [candidate-next-robots (filtered-next-robots blueprint state time)
;;           times-to-next-robot (map #(get-time-to-next-robot blueprint state %1) candidate-next-robots)
;;           advanced-states (map #(advance-state state %1) times-to-next-robot)
;;           states-after-purchase (map (partial purchase-robot blueprint) advanced-states candidate-next-robots)
;;           advanced-times (map #(- time %1) times-to-next-robot)
;;           ;; _ (prn "as" advanced-states "cn" candidate-next-robots "sp" states-after-purchase)
;;           maxes (map #(find-max-2 blueprint %1 %2) states-after-purchase advanced-times)
;;           ;; _ (prn "s" state)
;;           ;; _ (prn "nr" candidate-next-robots times-to-next-robot)
;;           ;; _ (prn "as" advanced-states)
;;           ;; _ (prn "sp" states-after-purchase advanced-times)
;;           ]
;;       ;; (prn "goodbye" candidate-next-robots advanced-times maxes)
;;       ;; (prn)
;;       (if (empty? maxes)
;;         (recur blueprint (advance-state state time) 0)
;;         (apply max maxes)))))

;; ;; (find-max-2 b1 initial-state 10)
;; 12
;; (get-possible-next-robots initial-state)



;; (defn paths [blueprint state current_path acc])

(defn get-time-to-next-robot [blueprint state n]
  ;; (prn "time-to-next" blueprint state n)
  (let [requirement (nth blueprint n)
        [delta current] state
        time-per-resource
        (map (fn [needed generated have]
               (cond (or (>= have needed) (>= generated needed))
                     0
                     (= 0 generated)
                     ##Inf
                     :else
                     (int (Math/ceil (/ (- needed have) generated))))) requirement delta current)]
    (max 1 (apply max time-per-resource))))

;; next paths

;; for a given subpath
;;   - check whether each robot is a possible next path
;;   - iterate on each possible next-path

(def time-limit 18)

(defn advance-state [state steps new-robot]

  (let [[robots supplies] state
        delta (mapv * robots (repeat steps))
        new-supplies (mapv + delta supplies)
        new-robots (mapv + (nth-1 new-robot) robots)]
    ;; (prn robots supplies delta new-supplies new-robots)
    [new-robots new-supplies]))

(defn do-stuff [blueprint partial-scenarios paths]
  (if (empty? partial-scenarios)
    (second paths)
    (let [[time current-path current-state] (first partial-scenarios)
          more-scenarios (rest partial-scenarios)
          time-remaining (- time-limit time)
          ;; _foo (prn time-remaining)
          times-to-build (map #(get-time-to-next-robot blueprint current-state %1) (range 4))
          next-steps (remove #(< time-remaining (nth times-to-build %1)) (range 4))
          next-times (map #(+ time (nth times-to-build %1)) next-steps)
          next-paths (map #(cons %1 current-path) (map #(do [%1 %2]) next-steps next-times))
          next-states (map #(advance-state current-state (nth times-to-build %1) %1) next-steps)
          next-scenarios (map #(do [%1 %2 %3]) next-times next-paths next-states)]
      (if (empty? next-steps)
        (do
          ;; (prn "adding path" current-path)
          (if (> (nth (second current-state) 3) (first paths))
            (recur blueprint more-scenarios [(nth (second current-state) 3) current-path])
            (recur blueprint more-scenarios paths)))
        (recur blueprint (concat next-scenarios more-scenarios) paths)))))


(count (do-stuff b1 [[0 [] initial-state]] [0 []]))
