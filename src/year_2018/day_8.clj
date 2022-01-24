(ns year-2018.day-8)

(defn parse-input
  [input]
  (->> input
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(comment
  (take 5 (parse-input (slurp "resources/year_2018/day_8.in"))))

(defn generate-tree
  [data]
  (let [[children-number metadata-number] data
        [next-data children] (reduce (fn [[data children] _]
                                       (let [[ndata child] (generate-tree data)]
                                         [ndata (conj children child)])) [(nthrest data 2) []] (range children-number))
        ret-data (nthrest next-data metadata-number)
        metadata (take metadata-number next-data)]
    [ret-data {:children children :metadata metadata}]))

(comment
  (generate-tree '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2)))

(defn reduce-tree
  [f status tree]
  (let [next-status (f status tree)]
    (reduce (fn [status child] (reduce-tree f status child)) next-status (tree :children))))

(comment
  (reduce-tree
    (fn [status {metadata :metadata}] (+ status (apply + metadata)))
    0
    (second (generate-tree '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2)))))

(defn main-part-1
  [input]
  (->> input
       parse-input
       generate-tree
       second
       (reduce-tree (fn [status {metadata :metadata}] (+ status (apply + metadata))) 0)))

(comment
  (-> "resources/year_2018/day_8.in"
      slurp
      main-part-1))

(defn node-value
  [{:keys [children metadata]}]
  (let [number-of-children (count children)]
    (if (= number-of-children 0)
      (apply + metadata)
      (reduce (fn [s m]
                (let [idx (dec m)]
                  (if (and (<= 0 idx) (< idx number-of-children)) (+ s (node-value (nth children idx))) s))) 0 metadata))))

(comment
  (node-value '{:children [{:children [], :metadata (10 11 12)} {:children [{:children [], :metadata (99)}], :metadata (2)}],
               :metadata (1 1 2)}))

(defn main-part-2
  [input]
  (->> input
       parse-input
       generate-tree
       second
       node-value))

(comment
  (-> "resources/year_2018/day_8.in"
      slurp
      main-part-2))