(ns year-2018.day-1
  (:require [clojure.test :refer :all]))

(deftest happypath
  (testing (is 15 (year-2018.day-1/main-part-1
                              "1
                              2
                              3
                              4
                              5
                              ")))
  (testing (is 1000 (year-2018.day-1/main-part-2
                   "1000
                   -999
                   ")))
  (testing (is 459 (main-part-1 (slurp "resources/year_2018/day_1.in"))))
  (testing (is 65474 (main-part-2 (slurp "resources/year_2018/day_1.in")))))

(comment
  (run-tests))