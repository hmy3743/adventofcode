(ns year-2020.day-4
  (:require [clojure.test :refer :all]
            [year-2020.day-4 :refer :all]))

(deftest happy-path
  (testing (is (= '("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"
                     "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929")
                  (parse-input-into-chunk
                    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n"))))
  (testing (is (= {"ecl" "gry", "pid" "860033327", "eyr" "2020", "hcl" "#fffffd", "byr" "1937", "iyr" "2017", "cid" "147", "hgt" "183cm"}
                  (parse-passport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"))))
  (testing (is (= '({"ecl" "gry", "pid" "860033327", "eyr" "2020", "hcl" "#fffffd", "byr" "1937", "iyr" "2017", "cid" "147", "hgt" "183cm"}
                    {"iyr" "2013", "ecl" "amb", "cid" "350", "eyr" "2023", "pid" "028048884", "hcl" "#cfa07d", "byr" "1929"})
                  (parse-input-into-passport
                    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n"))))
  (testing (is (= true
                  (passport-field-valid?
                    ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
                    {"ecl" "gry", "pid" "860033327", "eyr" "2020", "hcl" "#fffffd", "byr" "1937", "iyr" "2017", "cid" "147", "hgt" "183cm"}
                    ))))
  (testing (is (= false
                  (passport-field-valid?
                    ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
                    {"iyr" "2013", "ecl" "amb", "cid" "350", "eyr" "2023", "pid" "028048884", "hcl" "#cfa07d", "byr" "1929"}
                    ))))
  (testing (is (= 1 (main-part-1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n"))))
  (testing (is (= 190 (-> "resources/year_2020/day_4.in"
                          slurp
                          main-part-1))))
  (testing (is (= true (valid-byr? {"ecl" "gry", "pid" "860033327", "eyr" "2020", "hcl" "#fffffd", "byr" "1937", "iyr" "2017", "cid" "147", "hgt" "183cm"}))))
  (testing (is (= '(true false false false) (map valid-byr? '({"byr" "1992"} {"byr" "1919"} {"byr" "2003"} {})))))
  (testing (is (= '(false false false true true false false true true false) (map #(valid-hgt? %) '({} {"hgt" "1.8m"} {"hgt" "58in"} {"hgt" "59in"} {"hgt" "76in"} {"hgt" "77in"} {"hgt" "149cm"} {"hgt" "150cm"} {"hgt" "193cm"} {"hgt" "194cm"})))))
  (testing (is (= '(false true false false) (map valid-hcl? '({} {"hcl" "#123123"} {"hcl" "123123"} {"hcl" "#FFFFFF"})))))
  (testing (is (= '(false true true true true true true true false) (map valid-ecl? '({} {"ecl" "amb"} {"ecl" "blu"} {"ecl" "brn"} {"ecl" "gry"} {"ecl" "grn"} {"ecl" "hzl"} {"ecl" "oth"} {"ecl" "png"})))))
  (testing (is (= '(false true true false) (map valid-pid? '({} {"pid" "123123123"} {"pid" "023123123"} {"pid" "23123123"})))))
  (testing (is (= 121 (-> "resources/year_2020/day_4.in"
                          slurp
                          main-part-2)))))
(comment
  (run-tests))