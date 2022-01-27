(ns year-2018.day-7
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]))

(def input (string/trim-newline (slurp "resources/year_2018/day_7.in")))
(def time-taken (zipmap (map (comp str char) (range 65 91)) (range 61 96)))

(defn inspect
  [x]
  (println x)
  x)

(defn parse-input
  [input]
  (->> input
       string/split-lines
       (map (fn [line]
              [(second (re-find #"Step\s([A-Z])\smust" line))
               (second (re-find #"step\s([A-Z])\scan" line))]))))

(defn generate-indegree
  [orders]
  (let [node-set (->> orders
                      (apply concat)
                      set)]
    (->> orders
         (map second)
         frequencies
         (merge (zipmap node-set (repeat 0))))))

(defn generate-dag
  [order]
  (->> order
       (reduce (fn [dag [u v]] (assoc dag u (conj (dag u '()) v))) {})))

(defn time-progress
  [{:keys [entry-nodes now worker-number in-progress] :as state}]
  (let [empty-entry? (empty? entry-nodes)
        all-working? (= worker-number (count in-progress))
        fastest-complete-time (->> in-progress
                                   (map :end-time)
                                   (apply min ##Inf))
        next-now (match [empty-entry? all-working?]
                   [true _] fastest-complete-time
                   [_ true] fastest-complete-time
                   :else now)]
    (assoc state :now next-now)))

(defn assign-tasks
  [{:keys [now entry-nodes worker-number in-progress] :as state}]
  (let [idle-worker-number (- worker-number (count in-progress))
        ordered-task-candidate (sort entry-nodes)
        todos (take idle-worker-number ordered-task-candidate)
        progress (->> todos
                      (map #(hash-map :task % :end-time (+ now (time-taken %)))))
        next-in-progress (concat in-progress progress)
        todo-set (apply hash-set todos)
        next-entry-nodes (remove #(todo-set %) entry-nodes)]
    (merge state {:in-progress next-in-progress :entry-nodes next-entry-nodes})))

(defn resolve-task-dependency
  [dag indegree task]
  (->> (dag task)
       (reduce (fn [[ent ind] t]
                 (let [next-ind (update ind t dec)
                       next-ent (if (= 0 (next-ind t)) (conj ent t) ent)]
                   [next-ent next-ind])) ['() indegree])))

(defn complete-tasks
  [{:keys [now dag indegree entry-nodes in-progress] :as state}]
  (let [completed-progress (filter #(= now (:end-time %)) in-progress)
        uncompleted-progress (filter #(< now (:end-time %)) in-progress)
        completed-tasks (map #(:task %) completed-progress)
        [next-entry next-indegree] (reduce (fn [[ent ind] task]
                                             (let [[next-ent next-ind] (resolve-task-dependency dag ind task)]
                                               [(concat ent next-ent) next-ind]))
                                           [entry-nodes indegree]
                                           completed-tasks)]
    (merge state {:indegree next-indegree :entry-nodes next-entry :in-progress uncompleted-progress})))

(letfn [(select-by [f & items] (reduce (fn [u v] (if (f u v) u v)) items))]
  (defn next-node
    [{:keys [dag indegree entry-nodes]}]
    (let [cur (apply select-by (fn [u v] (<= (compare u v) 0)) entry-nodes)
          [next-entry-nodes next-indegree] (->> cur
                                                dag
                                                (reduce (fn [[ent ind] v]
                                                          (let [n (dec (ind v))]
                                                            [(if (= n 0) (conj ent v) ent) (assoc ind v n)]))
                                                        [entry-nodes indegree]))]
      [cur {:dag dag :indegree next-indegree :entry-nodes (remove #(= cur %) next-entry-nodes)}])))

(letfn [(iterate-by ([exit? next-with f args]
                     (iterate-by exit? next-with [] f args))
          ([exit? next-with prefix f args]
           (let [result (apply f args)]
             (if (exit? result)
               (conj prefix result)
               (recur exit? next-with (conj prefix result) f (next-with result))))))]

  (defn solve-part-1
    [orders]
    (let [indegree (generate-indegree orders)
          dag (generate-dag orders)
          entry-nodes (->> indegree
                           (filter #(= 0 (second %)))
                           (map first))]
      (->> (iterate-by
            (fn [[_ {entry-nodes :entry-nodes}]] (empty? entry-nodes))
            rest
            next-node
            [{:dag dag :indegree indegree :entry-nodes entry-nodes}])
           (map first)
           (apply str)))))

(defn work
  [{:keys [entry-nodes in-progress] :as state}]
  (match [(empty? entry-nodes) (empty? in-progress)]
    [true true] state
    :else (-> state
              time-progress
              complete-tasks
              assign-tasks
              recur)))

(defn solve-part-2
 [orders]
 (let [dag (generate-dag orders)
       indegree (generate-indegree orders)
       entry-nodes (->> indegree
                        (filter #(= 0 (second %)))
                        (map first))
       {end-time :now} (work {
                              :dag dag
                              :indegree indegree
                              :entry-nodes entry-nodes
                              :now 0
                              :worker-number 5
                              :in-progress []
       })]
   end-time))

(comment
  (parse-input input)
  (generate-indegree '(["A" "B"] ["C" "B"]))
  (generate-dag '(["A" "B"] ["C" "B"]))
  (next-node {:dag {"A" '("B"), "C" '("B")}
              :indegree {"C" 0, "B" 2, "A" 0}
              :entry-nodes ["A" "C"]})
  (solve-part-1 '(["A" "B"] ["C" "B"]))
  (-> input
      parse-input
      solve-part-1)
  (assign-tasks {:entry-nodes ["A" "C" "B"]
                 :now 0
                 :worker-number 3
                 :in-progress [{:task "D" :end-time 1}]})
  (complete-tasks {:dag {"A" '("C") "B" '("C")}
                   :indegree {"A" 0 "B" 0 "C" 2}
                   :entry-nodes []
                   :now 1
                   :worker-number 2
                   :in-progress [{:task "A" :end-time 1} {:task "B" :end-time 1}]})
  (work {:dag {"A" '("B"), "C" '("B")}
         :indegree {"C" 0, "B" 2, "A" 0}
         :entry-nodes ["A" "C"]
         :now 0
         :worker-number 2
         :in-progress []})
  (-> input
      parse-input
      solve-part-2))