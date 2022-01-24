(ns year-2018.day-4)

(defn time-tuple-to-minute-unix
  [[y m d h M]]
  (quot (.getTime (java.sql.Timestamp/valueOf (java.time.LocalDateTime/of y m d h M))) 60000))

(defn time-tuple-to-minute
  [time-tuple]
  (- (time-tuple-to-minute-unix time-tuple) (time-tuple-to-minute-unix [1 1 1 0 0])))

(comment
  (time-tuple-to-minute [1518 1 1 0 1]))

(defn time-diff
  [time-tuple1 time-tuple2]
  (- (time-tuple-to-minute time-tuple1) (time-tuple-to-minute time-tuple2)))

(defn parse-input
  [input]
  (->> input
       clojure.string/split-lines
       (map (fn [line]
              [
               (rest (re-find #"\[(\d+)-(\d+)-(\d+)\s(\d+):(\d+)\]" line))
               (second (re-find #"\]\s(.+)$" line))]))
       (map (fn [[timestamp message]]
              [(mapv #(Integer/parseInt %) timestamp) message]))
       (map (fn [[timestamp message]]
              [timestamp (case message
                           "falls asleep" :sleep
                           "wakes up" :awake
                           (Integer/parseInt (second (re-find #"#(\d+)\s" message))))]))
       (sort-by first)))

(comment
  (parse-input (slurp "resources/year_2018/day_4.in")))



(defn find-most-sleeping
  [shift-table]
  (let [[_ _ sleep-seconds]
        (reduce (fn [[id sleep-begin sleep-map] [time action]]
                  (case action
                    :sleep [id time sleep-map]
                    :awake [id nil (update sleep-map id (fnil #(+ % (time-diff time sleep-begin)) 0))]
                    [action nil sleep-map])) [nil nil nil] shift-table)]
    (->> sleep-seconds
         lazy-seq
         (apply max-key second)
         first)))

(comment
  (find-most-sleeping [[[1518 11 21 0 2] 2273]
                       [[1518 11 21 0 7] :sleep]
                       [[1518 11 21 0 19] :awake]
                       [[1518 11 21 0 48] :sleep]
                       [[1518 11 21 0 53] :awake]
                       [[1518 11 22 0 4] 3319]
                       [[1518 11 22 0 9] :sleep]
                       [[1518 11 22 0 44] :awake]]))

(defn calc-minute-count
  [m until]
  (-> until
      time-tuple-to-minute
      (+ (- 59 m))
      (quot 60)))
(def peek (fn [x] (println x ) x))

(comment
  (calc-minute-count 0 [1 #_#_#_1 1 1 0]))

(defn calc-minutes-between
  [begin-tuple end-tuple]
  (reduce (fn [counts m] (conj counts (- (calc-minute-count m end-tuple) (calc-minute-count m begin-tuple)))) [] (range 0 60)))

(comment
  (calc-minutes-between [1518 11 21 0 7] [1518 11 21 1 7]))

(defn sleep-by-minute
  [shift-table]
  (->> shift-table
       (reduce (fn [[id sleep-begin minute-count-vec] [time-tuple action]]
                 (case action
                   :sleep [id time-tuple minute-count-vec]
                   :awake [id nil (mapv + minute-count-vec (calc-minutes-between sleep-begin time-tuple))]
                   [action nil minute-count-vec])) [nil nil (vec (repeat 60 0))])
       (#(nth % 2))))

(comment
  (sleep-by-minute [[[1518 11 21 0 2] 2273]
                    [[1518 11 21 0 7] :sleep]
                    [[1518 11 21 0 19] :awake]
                    [[1518 11 21 0 48] :sleep]
                    [[1518 11 21 0 53] :awake]
                    [[1518 11 22 0 4] 3319]
                    [[1518 11 22 0 9] :sleep]
                    [[1518 11 22 0 44] :awake]]))

(defn filter-shift-table
  [target shift-table]
  (->> shift-table
       (reduce (fn [[current-id with-id] [_ action :as shift]]
                 (case action
                   :sleep [current-id (conj with-id [current-id shift])]
                   :awake [current-id (conj with-id [current-id shift])]
                   [action (conj with-id [action shift])])) [nil nil])
       second
       (filter #(= target (first %)))
       (map second)
       reverse))

(comment
  (filter-shift-table 3319 '([[1518 11 21 0 2] 2273]
                            [[1518 11 21 0 7] :sleep]
                            [[1518 11 21 0 19] :awake]
                            [[1518 11 21 0 48] :sleep]
                            [[1518 11 21 0 53] :awake]
                            [[1518 11 22 0 4] 3319]
                            [[1518 11 22 0 9] :sleep]
                             {:time [1518 11 22 0 44] :action :awake})))

(defn solve-part-1
  [shift-table]
  (let [champion (find-most-sleeping shift-table)]
    (->> shift-table
         (filter-shift-table champion)
         sleep-by-minute
         (reduce-kv (fn [[mosti mostv] k v]
                      (if (< mostv v) [k v] [mosti mostv])) [nil -1])
         first
         (* champion))))

(comment
  (solve-part-1 [[[1518 11 21 0 2] 2273]
                 [[1518 11 21 0 7] :sleep]
                 [[1518 11 21 0 19] :awake]
                 [[1518 11 21 0 48] :sleep]
                 [[1518 11 21 0 53] :awake]
                 [[1518 11 22 0 4] 3319]
                 [[1518 11 22 0 9] :sleep]
                 [[1518 11 22 0 44] :awake]]))

(defn main-part-1
  [input]
  (-> input
      parse-input
      solve-part-1))

(comment
  (main-part-1 (slurp "resources/year_2018/day_4.in")))

(defn solve-part-2
  [shift-table]
  (->> (into #{} (map second shift-table))
       (filter number?)
       (map #(vector % (filter-shift-table % shift-table)))
       (map (fn [[id table]] [id (sleep-by-minute table)]))
       (map (fn [[id numbers]] [id (map-indexed #(vector %1 %2) numbers)]))
       (map (fn [[id numbers]] [id (apply max-key second numbers)]))
       (apply max-key (fn [[_ [_ v]]] v))
       (apply (fn [id [m _]] (* id m)))))
       ;))

(comment
  (solve-part-2 [[[1518 11 21 0 2] 2273]
                 [[1518 11 21 0 7] :sleep]
                 [[1518 11 21 0 19] :awake]
                 [[1518 11 21 0 48] :sleep]
                 [[1518 11 21 0 53] :awake]
                 [[1518 11 22 0 4] 3319]
                 [[1518 11 22 0 9] :sleep]
                 [[1518 11 22 0 44] :awake]]))

(defn main-part-2
  [input]
  (-> input
      parse-input
      solve-part-2))

(comment
  (main-part-2 (slurp "resources/year_2018/day_4.in")))

(defn for-test
  []
  (let [_ (println 11)] (println 12)))

(for-test)