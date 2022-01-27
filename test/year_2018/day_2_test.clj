(ns year-2018.day-2-test
  (:require [clojure.test :refer :all] [year-2018.day-2 :refer :all]))

(deftest happypath
  (testing (is (= {\a 2, \b 1, \c 1, \d 1} (count-char "abcda"))))
  (testing (is (= '({\a 1, \b 1, \c 1, \d 1} {\a 2, \b 1, \c 1, \d 1} {\a 1, \b 1, \c 3}) (map count-char ["abcd" "abcda" "abccc"]))))
  (testing (is (= [1 0] (counts-reducer [0 0] \a 2))))
  (testing (is (= [1 2] (counts-reducer [1 1] \c 3))))
  (testing (is (= [1 1] (extract-two-three {\z 1 \a 2 \c 3 \d 4 \e 2}))))
  (testing (is (= 2 (solve-part-1 ["abcdaa" "aaefgh" "abab"]))))
  (testing (is (= 0 (equal-count "abc" "bcd"))))
  (testing (is (= 3 (equal-count "abcd" "abcc"))))
  (testing (is (= "ac" (solve-part-2 ["abc" "bcd" "cde" "def" "azc"]))))
  (testing (is (= 6000 (main-part-1 (slurp "resources/year_2018/day_2.in")))))
  (testing (is (= "pbykrmjmizwhxlqnasfgtycdv" (main-part-2 (slurp "resources/year_2018/day_2.in"))))))

(comment
  (run-tests))