(ns year-2018.day-5
  (:require [clojure.string :as string]))

(defn reaction?
  [c d]
  (and c
       d
       (not= c d)
       (= (string/upper-case c) (string/upper-case d))))

(defn process
  [string]
  (->> string
       (reduce (fn [vec ch] (if (reaction? (last vec) ch) (pop vec) (conj vec ch))) [])
       (apply str)))

(comment
  (process "dabAcCaCBAcCcaDA"))

(def input (string/trim-newline (slurp "resources/year_2018/day_5.in")))

(def unitfy (juxt string/upper-case string/lower-case))

(defn string-without-unit
  [ch s]
  (let [[u v] (unitfy ch)
        pattern (str u \| v)
        reg (re-pattern pattern)]
    (string/replace s reg "")))

(comment
  (-> input
      process
      count)
  (->> (range (int \a) (inc (int \z)))
       (map char)
       (map (fn [c] {:ch c :s input}))
       (map (fn [{:keys [ch s]}] (string-without-unit ch s)))
       (map process)
       (map count)
       (apply min)))