(ns year-2018.day-6
  (:require [clojure.string :as string]))

(def input (string/trim-newline (slurp "resources/year_2018/day_6.in")))

(defn parse-input
  [input]
  (->> input
       string/split-lines
       (map #(string/split % #",\s"))
       (map (fn [[a b]] [(Integer/parseInt a) (Integer/parseInt b)]))))

(defn min-max-locations
  [locations]
  (let [mr (->> locations
                (map first)
                (apply min))
        mc (->> locations
                (map second)
                (apply min))
        Mr (->> locations
                (map first)
                (apply max))
        Mc (->> locations
                (map second)
                (apply max))]
    {:min-row mr :min-col mc :max-row Mr :max-col Mc}))

(defn generate-field
  ([locations dr dc]
   (let [{mr :min-row mc :min-col Mr :max-row Mc :max-col} (min-max-locations locations)]
     (for [r (range (- mr dr) (+ (inc Mr) dr))
           c (range (- mc dc) (+ (inc Mc) dc))]
       [r c])))
  ([location]
   (generate-field location 0 0)))

(defn generate-lim
  [locations]
  (let [{mr :min-row mc :min-col Mr :max-row Mc :max-col} (min-max-locations locations)]
    (concat (for [c (range mc (inc Mc))] [mr c])
            (for [c (range mc (inc Mc))] [Mr c])
            (for [r (range mr (inc Mc))] [r mc])
            (for [r (range mr (inc Mr))] [r Mc]))))

(defn l1-norm
  [v1 v2]
  (->> (map vector v1 v2)
       (map (fn [[u v]] (Math/abs (- u v))))
       (apply +)))

(defn closest-location
  [u locations]
  (let [{:keys [min-idx min-value]}
        (->> locations
             (map-indexed vector)
             (map (fn [[idx location]] [idx (l1-norm u location)]))
             (reduce (fn [{:keys [min-idx min-value] :as state} [idx norm]]
                       (cond (< min-value norm) state
                             (= min-value norm) {:min-idx (conj min-idx idx) :min-value min-value}
                             (< norm min-value) {:min-idx [idx] :min-value norm}))
                     {:min-idx [] :min-value ##Inf}))]
    (if (= 1 (count min-idx)) {:min-idx (first min-idx) :distance min-value} {:distance min-value})))

(defn solve-part-1
  [locations]
  (let [lim (generate-lim locations)
        lim-closests (map #(closest-location % locations) lim)
        inf-idx (->> lim-closests
                     (filter :min-idx)
                     (reduce #(conj %1 (%2 :min-idx)) '())
                     (apply hash-set))
        field (generate-field locations)
        field-closest (map #(closest-location % locations) field)
        fin-closest (->> field-closest
                         (filter :min-idx)
                         (filter (fn [{min-idx :min-idx}] (not (inf-idx min-idx)))))
        fin-counts (->> fin-closest
                        (map :min-idx)
                        frequencies)]
    (->> fin-counts
         vals
         (apply max))))

(let [limit 10000]
  (defn solve-part-2
    [locations]
    (let [margin (quot limit (count locations))
          field (generate-field locations margin margin)
          distances (map (fn [location] (map #(l1-norm location %) locations)) field)
          distance-sum (map #(apply + %) distances)
          safe (filter #(< % limit) distance-sum)]
      (count safe))))

(comment
  (parse-input input)
  (-> input
      parse-input
      generate-field
      (as-> candidates (take 10 candidates)))
  (l1-norm [1 2] [2 3])
  (closest-location-index [0 0] '([1 2] [2 3] [-1 -1] [4 4]))
  (generate-lim '([1 2] [2 3] [-1 -1] [4 4]))
  (solve-part-1 '([-1 -1] [1 1] [1 -1] [-1 1] [0 0]))
  (-> input
      parse-input
      solve-part-1)
  (-> input
      parse-input
      solve-part-2))