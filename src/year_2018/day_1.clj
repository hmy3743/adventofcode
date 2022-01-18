;; re-seq v
;; reduce v
;; reductions v

(ns year-2018.day-1
  (:require [clojure.string :as str]))

(defn solve-part-1
  "solve part 1 problem"
  [integers]
  (apply + integers))

(defn parse-input
  "parse input string into integer list"
  [input]
  (->> (re-seq #"[+-]\d+" input)
       ;(->> (str/split-lines input)
       (map #(Integer/parseInt %))))

(defn main-part-1
  "main function to call with stdio for part 1"
  [input]
  (-> input
      (parse-input)
      (solve-part-1)))

(defn first-dup-accumulate
  "find the first duplicate accumulate value"
  [integers acc acc-set]
  (if (acc-set acc)
    acc
    (recur
      (rest integers)
      (+ acc (first integers))
      (conj acc-set acc))))

(defn solve-part-2
  "solve part 2 problem"
  [integers]
  (-> integers
      cycle
      (first-dup-accumulate 0 #{})))

(defn first-duplicate-element
  [element-set elements]
  (let [[head] elements]
    (if (element-set head)
      head
      (recur
        (conj element-set head)
        (rest elements)))))

(defn solve-part-2-with-reductions
  [integers]
  (->> integers
       cycle
       (reductions +)
       (first-duplicate-element #{})))

(defn reduce-until-accumulate-duplicate
  [status element]
  (let [[acc-set acc] status next-acc (+ acc element)]
    (if (acc-set acc)
      (reduced acc)
      [(conj acc-set acc) next-acc])))

(defn solve-part-2-with-reduce
  [integers]
  (->> integers
       cycle
       (reduce reduce-until-accumulate-duplicate [#{} 0])))

(defn main-part-2
  "main function to call with stdio for part 2"
  ([input]
   (-> input
       parse-input
       solve-part-2))
  ([input solve-func]
   (-> input
       parse-input
       solve-func)))

(comment
  (main-part-1 (slurp "resources/year_2018/day_1.in"))
  (main-part-2 (slurp "resources/year_2018/day_1.in"))
  (main-part-2 (slurp "resources/year_2018/day_1.in") solve-part-2-with-reductions)
  (main-part-2 (slurp "resources/year_2018/day_1.in") solve-part-2-with-reduce))
