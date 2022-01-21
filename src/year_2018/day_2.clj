(ns year-2018.day-2)

(defn parse-input
  [input]
  (re-seq #"[^ \r\n]+" input))

(defn char-counter
  [ch-map ch]
  (let [number (ch-map ch)]
    (if (some? number)
      (conj ch-map [ch (inc number)])
      (conj ch-map [ch 1]))))

(defn count-char
  [string]
  (reduce char-counter {} string))

(comment
  (count-char "abcda")
  (map count-char ["abcd" "abcda" "abccc"]))

(defn counts-reducer
  [status _ number]
  (let [[two three] status]
    (case number
      2 [(inc two) three]
      3 [two (inc three)]
      status)))

(comment
  (counts-reducer [0 0] \a 2)
  (counts-reducer [1 1] \c 3))

(defn extract-two-three
  [counts]
  (let [[two three] (reduce-kv counts-reducer [0 0] counts)]
    (cond
      (and (< 0 two) (< 0 three)) [1 1]
      (< 0 two) [1 0]
      (< 0 three) [0 1]
      :else [0 0])))

(comment
  (extract-two-three {\z 1 \a 2 \c 3 \d 4 \e 2}))

(defn solve-part-1
  [strings]
  (->> strings
       ;(map count-char)
       (map frequencies)
       (map extract-two-three)
       (apply map +)
       (apply *)))

(comment
  (solve-part-1 ["abcdaa" "aaefgh" "abab"]))

(defn main-part-1
  "Entry function for part 1 problem"
  [input]
  (-> input
      parse-input
      solve-part-1))

(defn equal-count
  [u v]
  (->> (map = u v)
       (filter identity)
       count))

(comment
  (equal-count "abc" "bcd")
  (equal-count "abcd" "abcc"))

(defmulti pairing-inner (fn [u v] (or (empty? u) (empty? v))))
(defmethod pairing-inner true [_ _] '())
(defmethod pairing-inner :default
  [u v]
  (lazy-cat (map (fn [a b] [a b]) u v) (pairing-inner u (rest v))))

(defn pairing
  [items]
  (pairing-inner items (rest items)))

(comment
  (pairing '(1 2 3 4)))

(defn one-by-one
  [[x y]]
  (map #(vector %1 %2) x y))

(defn solve-part-2
  [strings]
  (->> strings
       pairing
       (some (fn [[u v]] (when (= (dec (count u)) (equal-count u v)) [u v])))
       ;(apply map (fn [c d] [c d]))
       one-by-one
       (filter (fn [[c d]] (= c d)))
       (map first)
       (apply str)))

(comment
((fn [[x y]] (map #(vector %1 %2) x y)) ["abcd" "abcc"])
(apply map (fn [c d] [c d]) ["abcd" "abcc"])
(map (fn [[a b]] (vector a b)) ["abcd" "abcc"]))

(defn main-part-2
  "Entry function for part 2 problem"
  [input]
  (-> input
      parse-input
      solve-part-2))

(comment
  (solve-part-2 ["abc" "bcd" "cde" "def" "azc"]))

(comment
  (main-part-1 (slurp "resources/year_2018/day_2.in"))
  (main-part-2 (slurp "resources/year_2018/day_2.in")))

(comment
  (defn inspect
    [x]
    (println x)
    x)
  (->> [1 2 3]
      inspect
      (map inc)
      inspect))