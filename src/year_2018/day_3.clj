(ns year-2018.day-3)

(defn parse-line
  [line]
  [(second (re-find #"#(\d+)\s" line))
   (mapv #(Integer/parseInt %) (rest (re-find #"@\s(\d+),(\d+):" line)))
   (mapv #(Integer/parseInt %) (rest (re-find #":\s(\d+)x(\d+)$" line)))
   ])

(comment
  (= ["1" [1 3] [4 4]] (parse-line "#1 @ 1,3: 4x4")))

(defn parse-input
  [input]
  (->> input
       clojure.string/split-lines
       (map parse-line)))

(comment
  (parse-input "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n"))

(defn empty-field
  [row-number col-number]
  (vec (repeat row-number (vec (repeat col-number nil)))))

(comment
  (empty-field 3 4))

(defn calc-field-size
  [queries]
  (->> queries
       (map #(subvec % 1))
       (map (fn [[[a b] [c d]]] [(+ a c) (+ b d)]))
       (reduce (fn [[a b] [c d]] [(max a c) (max b d)]))))

(comment
  (calc-field-size '(["1" [1 3] [4 4]] ["2" [3 1] [4 4]] ["3" [5 5] [2 2]])))

(defn set-id-on-field
  [id locations field]
  (if (seq locations)
    (let [[[r c]] locations]
      (->> (update-in field [r c] (fn [v] (case v nil id id id \X)))
           (recur id (rest locations))))
    field))

(comment
  (set-id-on-field
    "1"
    '([1 3] [1 4] [1 5] [1 6] [2 3] [2 4] [2 5] [2 6] [3 3] [3 4] [3 5] [3 6] [4 3] [4 4] [4 5] [4 6])
    (empty-field 7 7)))

(defn process-query
  [[id [r c] [rs cs]] field]
  (let [locations (->> (range r (+ r rs))
                       (mapcat (fn [r] (map (fn [c] [r c]) (range c (+ c cs))))))]
    (set-id-on-field id locations field)))

(comment
  (process-query ["1" [1 3] [4 4]] (empty-field 7 7)))

(defn process-queries
  [queries field]
  (if (seq queries)
    (->> field
         (process-query (first queries))
         (recur (rest queries)))
    field))

(comment
  (process-queries '(["1" [1 3] [4 4]] ["2" [3 1] [4 4]] ["3" [5 5] [2 2]]) (empty-field 7 7)))

(defn solve-part-1
  [queries]
  (->> queries
       calc-field-size
       (apply empty-field)
       (process-queries queries)
       flatten
       (filter #(= \X %))
       count))

(comment
  (solve-part-1 '(["1" [1 3] [4 4]] ["2" [3 1] [4 4]] ["3" [5 5] [2 2]])))

(defn main-part-1
  [input]
  (-> input
      parse-input
      solve-part-1))

(comment
  (main-part-1 (slurp "resources/year_2018/day_3.in")))

(defn overlap?
  [[id [r c] [rs cs]] field]
  (let [locations (->> (range r (+ r rs))
                       (mapcat (fn [r] (map (fn [c] [r c]) (range c (+ c cs))))))]
    (->> locations
         (map (fn [loc] (get-in field loc)))
         (filter #(= \X %))
         count
         (not= 0))))

(def sample-field [[nil nil nil nil nil nil nil]
                   [nil nil nil "1" "1" "1" "1"]
                   [nil nil nil "1" "1" "1" "1"]
                   [nil "2" "2" \X \X "1" "1"]
                   [nil "2" "2" \X \X "1" "1"]
                   [nil "2" "2" "2" "2" "3" "3"]
                   [nil "2" "2" "2" "2" "3" "3"]])

(comment
  (overlap? ["1" [1 3] [4 4]] sample-field))

(defn solve-part-2
  [queries]
  (def field
    (->> queries
         calc-field-size
         (apply empty-field)
         (process-queries queries)))
  (->> queries
       (map (fn [query] [(overlap? query field) (first query)]))
       (filter #(not (first %)))
       (map second)))

(comment
  (solve-part-2 '(["1" [1 3] [4 4]] ["2" [3 1] [4 4]] ["3" [5 5] [2 2]])))

(defn main-part-2
  [input]
  (-> input
      parse-input
      solve-part-2))

(comment
  (main-part-2 (slurp "resources/year_2018/day_3.in")))