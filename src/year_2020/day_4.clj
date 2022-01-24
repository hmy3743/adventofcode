(ns year-2020.day-4)

(defn inspect
  [arg]
  (println arg)
  arg)

(defn parse-input-into-chunk
  [input]
  (->> input
       (re-seq #"(?:.+\n)+\n")
       (map clojure.string/trim-newline)))

(comment
  (parse-input-into-chunk
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n"
    ))

(defn parse-passport
  [passport-string]
  (->> passport-string
       (re-seq #"([^:\s]+):([^:\s]+)")
       (map #(subvec % 1))
       (into {})))

(comment
  (parse-passport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"))

(defn parse-input-into-passport
  [input]
  (->> input
       parse-input-into-chunk
       (map parse-passport)))

(comment
  (parse-input-into-passport
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n"
    ))

(defn passport-field-valid?
  [fields passport]
  (every? passport fields))

(comment
  (passport-field-valid?
    ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
    {"ecl" "gry", "pid" "860033327", "eyr" "2020", "hcl" "#fffffd", "byr" "1937", "iyr" "2017", "cid" "147", "hgt" "183cm"}
    )
  (passport-field-valid?
    ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
    {"iyr" "2013", "ecl" "amb", "cid" "350", "eyr" "2023", "pid" "028048884", "hcl" "#cfa07d", "byr" "1929"}
    ))

(defn solve-part-1
  [passports]
  (->> passports
       (filter #(passport-field-valid? ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"] %))
       count))

(comment
  (solve-part-1 '({"ecl" "gry", "pid" "860033327", "eyr" "2020", "hcl" "#fffffd", "byr" "1937", "iyr" "2017", "cid" "147", "hgt" "183cm"}
                  {"iyr" "2013", "ecl" "amb", "cid" "350", "eyr" "2023", "pid" "028048884", "hcl" "#cfa07d", "byr" "1929"})
                ))

(defn main-part-1
  [input]
  (-> input
      parse-input-into-passport
      solve-part-1))

(comment
  (main-part-1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n")
  (-> "resources/year_2020/day_4.in"
      slurp
      main-part-1))

(defn parse-int
  [string]
  (try
    (Integer/parseInt string)
    (catch Exception _ nil)))

(defn int-in?
  [bottom top integer]
  (case (some-> integer
                parse-int
                (#(and (<= bottom %) (< % top))))
    true true
    false))

(defn valid-byr?
  [{byr "byr"}]
  (int-in? 1920 2003 byr))

(comment
  (valid-byr? {"ecl" "gry", "pid" "860033327", "eyr" "2020", "hcl" "#fffffd", "byr" "1937", "iyr" "2017", "cid" "147", "hgt" "183cm"}
              ))

(comment
  (valid-byr? {"byr" "1992"})
  (valid-byr? {"byr" "1919"})
  (valid-byr? {"byr" "2003"})
  (valid-byr? {}))

(defn valid-iyr?
  [{iyr "iyr"}]
  (int-in? 2010 2021 iyr))

(defn valid-eyr?
  [{eyr "eyr"}]
  (int-in? 2020 2031 eyr))

(defn valid-hgt?
  [{hgt "hgt"}]
  (if hgt (let [bottoms {"in" 59 "cm" 150}
                tops {"in" 77 "cm" 194}
                [_ val unit] (re-matches #"(\d+)(cm|in)" hgt)]
            (and
              (.contains #{"cm" "in"} unit)
              (int-in? (bottoms unit) (tops unit) val)))
          false))

(comment
  (valid-hgt? {})
  (valid-hgt? {"hgt" "1.8m"})
  (map #(valid-hgt? %) '({"hgt" "58in"} {"hgt" "59in"} {"hgt" "76in"} {"hgt" "77in"} {"hgt" "149cm"} {"hgt" "150cm"} {"hgt" "193cm"} {"hgt" "194cm"})))

(defn valid-hcl?
  [{hcl "hcl"}]
  (some? (when hcl (re-matches #"#[0-9a-f]{6}" hcl))))

(comment
  (valid-hcl? {})
  (map valid-hcl? '({"hcl" "#123123"} {"hcl" "123123"} {"hcl" "#FFFFFF"})))

(defn valid-ecl?
  [{ecl "ecl"}]
  (.contains #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl))

(comment
  (map valid-ecl? '({"ecl" "amb"} {"ecl" "blu"} {"ecl" "brn"} {"ecl" "gry"} {"ecl" "grn"} {"ecl" "hzl"} {"ecl" "oth"} {"ecl" "png"})))

(defn valid-pid?
  [{pid "pid"}]
  (some? (when pid (re-matches #"\d{9}" pid))))

(comment
  (valid-pid? {})
  (map valid-pid? '({"pid" "123123123"} {"pid" "023123123"} {"pid" "23123123"})))

(defn solve-part-2
  [passports]
  (->> passports
       (filter #(and
                  (valid-byr? %)
                  (valid-iyr? %)
                  (valid-eyr? %)
                  (valid-hgt? %)
                  (valid-hcl? %)
                  (valid-ecl? %)
                  (valid-pid? %)))
       count))

(defn main-part-2
  [input]
  (-> input
      parse-input-into-passport
      solve-part-2))

(comment
  (-> "resources/year_2020/day_4.in"
      slurp
      main-part-2))