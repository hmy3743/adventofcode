(ns year-2018.day-3-test
  (:require [clojure.test :refer :all] [year-2018.day-3 :refer :all]))

(deftest happypath
  (testing (is (= ["1" [1 3] [4 4]] (parse-line "#1 @ 1,3: 4x4"))))
  (testing (is (= '(["1" [1 3] [4 4]] ["2" [3 1] [4 4]] ["3" [5 5] [2 2]]) (parse-input "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n"))))
  (testing (is (= [[nil nil nil nil] [nil nil nil nil] [nil nil nil nil]] (empty-field 3 4))))
  (testing (is (= [7 7] (calc-field-size '(["1" [1 3] [4 4]] ["2" [3 1] [4 4]] ["3" [5 5] [2 2]])))))
  (testing (is (=
             [[nil nil nil nil nil nil nil]
              [nil nil nil "1" "1" "1" "1"]
              [nil nil nil "1" "1" "1" "1"]
              [nil nil nil "1" "1" "1" "1"]
              [nil nil nil "1" "1" "1" "1"]
              [nil nil nil nil nil nil nil]
              [nil nil nil nil nil nil nil]]
             (set-id-on-field
               "1"
               '([1 3] [1 4] [1 5] [1 6] [2 3] [2 4] [2 5] [2 6] [3 3] [3 4] [3 5] [3 6] [4 3] [4 4] [4 5] [4 6])
               (empty-field 7 7))
             )))
  (testing (is (=
             [[nil nil nil nil nil nil nil]
              [nil nil nil "1" "1" "1" "1"]
              [nil nil nil "1" "1" "1" "1"]
              [nil nil nil "1" "1" "1" "1"]
              [nil nil nil "1" "1" "1" "1"]
              [nil nil nil nil nil nil nil]
              [nil nil nil nil nil nil nil]]
             (process-query ["1" [1 3] [4 4]] (empty-field 7 7)))))
  (testing (is (=
             [[nil nil nil nil nil nil nil]
              [nil nil nil "1" "1" "1" "1"]
              [nil nil nil "1" "1" "1" "1"]
              [nil "2" "2" \X \X "1" "1"]
              [nil "2" "2" \X \X "1" "1"]
              [nil "2" "2" "2" "2" "3" "3"]
              [nil "2" "2" "2" "2" "3" "3"]]
             (process-queries '(["1" [1 3] [4 4]] ["2" [3 1] [4 4]] ["3" [5 5] [2 2]]) (empty-field 7 7)))))
  (testing (is (= 4 (solve-part-1 '(["1" [1 3] [4 4]] ["2" [3 1] [4 4]] ["3" [5 5] [2 2]])))))
  (testing (is (= 118322 (main-part-1 (slurp "resources/year_2018/day_3.in")))))
  (testing (is (= true (overlap? ["1" [1 3] [4 4]] sample-field))))
  (testing (is (= '("3") (solve-part-2 '(["1" [1 3] [4 4]] ["2" [3 1] [4 4]] ["3" [5 5] [2 2]])))))
  (testing (is (= '("1178") (main-part-2 (slurp "resources/year_2018/day_3.in"))))))

(comment
  (run-tests))