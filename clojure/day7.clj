(ns day7
  (:require [clojure.core :as core] [clojure.string :as str]))

(def input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(def input (slurp "./day7.input"))

(defn is-command [line] (str/starts-with? line "$"))


(defn change-path [path dir]
  (cond (= dir "/")
        ["/"]
        (= dir "..")
        (rest path)
        :else
        (cons dir path)))

(defn add-ls-entry-to-state [state ls-entry]
  (if (nil? ls-entry)
    state
    (let [[value key] (str/split ls-entry #" ")]
      (assoc state key (if (= "dir" value) (get state key {})
                           (parse-long value))))))



(defn tp [label x]
  ;; (prn label x) 
  x)

(defn add-ls-to-state [state path output]
  (if (empty? path)
    (tp "reduce" (reduce add-ls-entry-to-state state output))
    (let [next-dir (last path)
          inner-state (add-ls-to-state (get state next-dir {}) (drop-last path) output)]
      (tp "assoc" (assoc state next-dir inner-state)))))


(defn read-instruction [lines state path]
  (let [current-line (first lines)]

    (if (= current-line nil)
      state
      (let [[output rest-of-lines] (split-with (comp not is-command) (rest lines))
            [_dollar command arg] (str/split current-line #" ")]
        (cond (= command "cd")
              (read-instruction rest-of-lines state (change-path path arg))
              (= command "ls")
              (let [new-state (tp "new-state" (add-ls-to-state state path output))]
                (tp "post-state" (read-instruction (tp "rest" rest-of-lines) new-state (tp "p" path))))
              :else
              (read-instruction rest-of-lines state path))))))

(def initial-state (read-instruction (str/split-lines input) {} ()))

;; returns [size [[dir size] [dir size] [dir size]]]
(defn list-directory-sizes [state path]
  (let [subdirs (filter (comp not number? second) (seq state))
        subdir-results
        (map (fn [[k v]]
               (list-directory-sizes v (cons k path))) subdirs)
        subdir-sizes (map first subdir-results)
        subdir-lists (map second subdir-results)
        files (filter (comp number? second) (seq state))
        shallow-size (reduce + (map second files))
        included-size (reduce + subdir-sizes)
        total-size (+ shallow-size included-size)
        joined-subdir-lists (cons [path total-size] (apply concat subdir-lists))]
    [total-size joined-subdir-lists]))


(def result (list-directory-sizes initial-state ()))

;; size of dirs less than 100000
(reduce + (filter (partial > 100000) (map second (second result))))

(let [fs-space 70000000
      needed-space 30000000
      all-dir-sizes (map second (second result))
      max-dir-size (apply max all-dir-sizes)
      free-space (- fs-space max-dir-size)
      min-delete-size (- needed-space free-space)
      delete-size (apply min (filter (partial <= min-delete-size) all-dir-sizes))]
  delete-size)
