(ns year-2018.day-4
  (:require [clojure.test :refer :all] [year-2018.day-4 :refer :all]))

(deftest happypath
  (testing (is (= 797880961 (time-tuple-to-minute [1518 1 1 0 1]))))
  (testing (is (= 3319 (find-most-sleeping [[[1518 11 21 0 2] 2273]
                                      [[1518 11 21 0 7] :sleep]
                                      [[1518 11 21 0 19] :awake]
                                      [[1518 11 21 0 48] :sleep]
                                      [[1518 11 21 0 53] :awake]
                                      [[1518 11 22 0 4] 3319]
                                      [[1518 11 22 0 9] :sleep]
                                      [[1518 11 22 0 44] :awake]]))))
  (testing (is (= 1 (calc-minute-count 0 [1 1 1 1 0]))))
  (testing (is (= (repeat 60 1) (calc-minutes-between [1518 11 21 0 7] [1518 11 21 1 7]))))
  (testing (is (= [0 0 0 0 0 0 0 1 1 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0]
               (sleep-by-minute [[[1518 11 21 0 2] 2273]
                                   [[1518 11 21 0 7] :sleep]
                                   [[1518 11 21 0 19] :awake]
                                   [[1518 11 21 0 48] :sleep]
                                   [[1518 11 21 0 53] :awake]
                                   [[1518 11 22 0 4] 3319]
                                   [[1518 11 22 0 9] :sleep]
                                   [[1518 11 22 0 44] :awake]]))))
  (testing (is (= '([[1518 11 22 0 4] 3319] [[1518 11 22 0 9] :sleep] [[1518 11 22 0 44] :awake])
               (filter-shift-table 3319 '([[1518 11 21 0 2] 2273]
                                          [[1518 11 21 0 7] :sleep]
                                          [[1518 11 21 0 19] :awake]
                                          [[1518 11 21 0 48] :sleep]
                                          [[1518 11 21 0 53] :awake]
                                          [[1518 11 22 0 4] 3319]
                                          [[1518 11 22 0 9] :sleep]
                                          [[1518 11 22 0 44] :awake])))))
  (testing (is (= 29871 (solve-part-1 [[[1518 11 21 0 2] 2273]
                                [[1518 11 21 0 7] :sleep]
                                [[1518 11 21 0 19] :awake]
                                [[1518 11 21 0 48] :sleep]
                                [[1518 11 21 0 53] :awake]
                                [[1518 11 22 0 4] 3319]
                                [[1518 11 22 0 9] :sleep]
                                [[1518 11 22 0 44] :awake]]))))
  (testing (is (= 8950 (main-part-1 (slurp "resources/year_2018/day_4.in")))))
  (testing (is (= 118196 (solve-part-2 [[[1518 11 21 0 2] 2273]
                                [[1518 11 21 0 7] :sleep]
                                [[1518 11 21 0 19] :awake]
                                [[1518 11 21 0 48] :sleep]
                                [[1518 11 21 0 53] :awake]
                                [[1518 11 22 0 4] 3319]
                                [[1518 11 22 0 9] :sleep]
                                [[1518 11 22 0 44] :awake]]))))
  (testing (is (= 78452 (main-part-2 (slurp "resources/year_2018/day_4.in"))))))

(comment
  (run-tests))