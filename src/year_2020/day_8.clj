(ns year-2020.day-8
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]))

(defn inspect
  [x]
  (println x)
  x)

(def input (-> "resources/year_2020/day_8.in"
               slurp
               string/trim-newline))

(defn parse-input
  [input]
  (->> input
       (re-seq #"((?:nop)|(?:jmp)|(?:acc))\s([+-]\d+)\n")
       (map rest)
       (mapv (fn [[op v]] [(keyword op) (Integer/parseInt v)]))))

(defn run-op
  [[op v] index acc]
  (case op
    :nop [(inc index) acc]
    :jmp [(+ v index) acc]
    :acc [(inc index) (+ v acc)]))

(defn process
  [{:keys [aborted? program index history-set acc] :as state}]
  (let [index-valid? (<= 0 index (dec (count program)))
        operate-twice? (contains? history-set index)]
    (match [aborted? index-valid? operate-twice?]
      [true _ _] state
      [_ false _] (assoc state :aborted? true)
      [_ _ true] (assoc state :aborted? true)
      :else (let [[next-index next-acc] (run-op (nth program index) index acc)]
              (merge state {:index next-index
                            :acc next-acc
                            :history-set (conj history-set index)})))))

(defn initial-state
  [program]
  {:aborted? false
   :program program
   :index 0
   :history-set #{}
   :acc 0})

(defn process-all
  [state]
  (->> state
       (iterate process)
       (take-while #(not (:aborted? %)))
       last))

(defn solve-part-1
  [program]
  (->> program
       initial-state
       process-all
       :acc))

(defn generate-candidate-programs
  [program]
  (->> program
       (map-indexed vector)
       (filter (fn [[_ [op _]]] (#{:jmp :nop} op)))
       (map first)
       (map (fn [i] (assoc program i [({:jmp :nop :nop :jmp} (first (nth program i)))
                                      (second (nth program i))])))))

(defn solve-part-2
  [program]
  (->> program
       generate-candidate-programs
       (pmap initial-state)
       (pmap process-all)
       (filter (fn [{index :index}] (or (< index 0) (<= (count program) index))))
       (map :acc)))

(comment
  (parse-input input)
  (process {:aborted? false
            :program [[:acc 1] [:jmp 3] [:acc 1] [:acc 1] [:jmp 2] [:jmp -2] [:jmp -6]]
            :index 0
            :history-set #{}
            :acc 0})
  (process-all {:aborted? false
                :program [[:acc 1] [:jmp 3] [:acc 1] [:acc 1] [:jmp 2] [:jmp -2] [:jmp -6]]
                :index 0
                :history-set #{}
                :acc 0})
  (-> input
      parse-input
      solve-part-1)
  (generate-candidate-programs [[:acc 1] [:jmp 3] [:acc 1] [:acc 1] [:jmp 2] [:jmp -2] [:jmp -6]])
  (-> input
      parse-input
      solve-part-2)
  )
