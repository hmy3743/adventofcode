(ns year-2020.day-4
  (:require [clojure.spec.alpha :as spec]))

(defn inspect
  [arg]
  (println arg)
  arg)

(defn parse-input-into-chunk
  [input]
  (->> input
       (re-seq #"(?:.+\n)+\n")
       (map clojure.string/trim-newline)))

(defn parse-able
  [value]
  (try (Integer/parseInt value) value))

(defn parse-passport
  [passport-string]
  (->> passport-string
       (re-seq #"([^:\s]+):([^:\s]+)")
       (map #(subvec % 1))
       (into {})
       (#(select-keys % ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"]))
       (#(zipmap (map keyword (keys %)) (vals %)))))

(defn parse-input-into-passport
  [input]
  (->> input
       parse-input-into-chunk
       (map parse-passport)))

(defn passport-field-valid?
  [fields passport]
  (every? passport fields))

(defn solve-part-1
  [passports]
  (->> passports
       (filter #(passport-field-valid? [:byr :iyr :eyr :hgt :hcl :ecl :pid] %))
       count))


(defn main-part-1
  [input]
  (-> input
      parse-input-into-passport
      solve-part-1))


(defn parse-int
  [string]
  (try
    (Integer/parseInt string)
    (catch Exception _ nil)))

(defn int-in?
  [bottom top integer]
  (true? (some-> integer
                parse-int
                (#(and (<= bottom %) (< % top))))))

(defn valid-hgt?
  [hgt]
  (if hgt (let [bottoms {"in" 59 "cm" 150}
                tops {"in" 77 "cm" 194}
                [_ val unit] (re-matches #"(\d+)(cm|in)" hgt)]
            (and
              (contains? #{"cm" "in"} unit)
              (int-in? (bottoms unit) (tops unit) val)))
          false))

(spec/def :passport/parsed-int
  (spec/conformer #(try (Integer/parseInt %) (catch Exception _ ::spec/invalid))))

(spec/def :passport/byr
  (spec/and :passport/parsed-int (spec/int-in 1920 2003)))

(spec/def :passport/iyr
  (spec/and :passport/parsed-int (spec/int-in 2010 2021)))
(spec/def :passport/eyr
  (spec/and :passport/parsed-int (spec/int-in 2020 2031)))
(spec/def :passport/hgt
  (spec/and (spec/conformer #(re-matches #"(\d+)(cm|in)" %))
            (spec/conformer (fn [[_ u v]] (if (or (nil? u) (nil? v)) ::spec/invalid {:value (Integer/parseInt u) :unit v})))
            (spec/conformer (fn [{:keys [value unit]}] (if (spec/valid? (spec/int-in ({"in" 59 "cm" 150} unit) ({"in" 77 "cm" 194} unit)) value) true ::spec/invalid)))))
(spec/def :passport/hcl
  (spec/and string? #(re-matches #"#[0-9a-f]{6}" %)))
(spec/def :passport/ecl
  #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(spec/def :passport/pid
  (spec/and string? #(re-matches #"\d{9}" %)))

(spec/def :passport/all
  (spec/keys :req-un [:passport/byr :passport/iyr :passport/eyr :passport/hgt :passport/hcl :passport/ecl :passport/pid]))

(defn solve-part-2-with-spec
  [passports]
  (->> passports
       (filter #(spec/valid? :passport/all %))
       count))

(defn main-part-2
  [input solve-func]
   (-> input
       parse-input-into-passport
       solve-func))

(comment
  (parse-input-into-chunk
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n"
    )
  (parse-passport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm")
  (parse-input-into-passport
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n"
    )
  (passport-field-valid?
    [:byr :iyr :eyr :hgt :hcl :ecl :pid]
    {:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}
    )
  (passport-field-valid?
    [:byr :iyr :eyr :hgt :hcl :ecl :pid]
    {:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"}
    )
  (solve-part-1 '({:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}
                  {:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"})
                )
  (main-part-1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n")
  (-> "resources/year_2020/day_4.in"
      slurp
      main-part-1)
  (-> "resources/year_2020/day_4.in"
      slurp
      (main-part-2 solve-part-2-with-spec))
  (spec/explain :passport/all {:byr "1992"}))
