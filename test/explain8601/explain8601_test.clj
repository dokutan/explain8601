(ns explain8601.explain8601-test
  (:require [clojure.test :refer :all]
            [explain8601.parser :as parser]
            [explain8601.transformer :as transformer]
            [explain8601.invalidator :as invalidator]))

(deftest explain8601-test
  (let [explain8601 (fn [expression]
                      (-> expression
                          parser/parse-all-8601-1
                          parser/parse-all-8601-2
                          parser/parse-all-8601-3
                          transformer/transform-8601
                          (transformer/descriptions->description expression)))
        test-cases {"" "'' does not appear to be a valid expression.\n"
                    "25:00:00" "'25:00:00' does not appear to be a valid expression.\n"
                    "12:30.5" "'12:30.5' represents 30,5 minutes past 12 hours.\n"
                    "P3W" "'P3W' represents a duration of 3 weeks.\n"
                    "-P1W" "'-P1W' represents a duration of 1 week in the reverse direction.\n"
                    "-1234S2" "'-1234S2' represents a year between -1200 and -1299, estimated to be -1234.\n"
                    "2022-123" "'2022-123' represents the 123rd day of the year 2022.\n"
                    "2022-W12-X" "'2022-W12-X' represents an unspecified day of the 12th week of the year 2022.\n"
                    "01:02+03" "'01:02+03' represents 2 minutes past 1 hours (3 hours ahead of UTC).\n"
                    "R3/P1Y/2022" "'R3/P1Y/2022' represents an interval lasting for a duration of 1 year, ending the year 2022, repeating 3 times.\n"}]
    (doall (map
            (fn [s]
              (testing s (is (=
                              (explain8601 s)
                              (test-cases s)))))
            (keys test-cases)))))

(deftest invalid-string?-test
  (testing "1234-12-12TT12:34"
    (is (true? (invalidator/invalid-string? "1234-12-12TT12:34"))))

  (testing "1234-12-12T12:34"
    (is (false? (invalidator/invalid-string? "1234-12-12T12:34")))))

(deftest invalid-tree?-test
  (let [valid [[{:second "00"}]
               [{:second "XX"}]
               [{:second "6X"}]
               [{:second "60"}]
               [{:minute "00"}]
               [{:minute "59"}]
               [{:minute "XX"}]
               [{:minute "5X"}]
               [{:hour "00"}]
               [{:hour "24"}]
               [{:hour "XX"}]
               [{:hour "2X"}]
               [{:week "01"}]
               [{:week "53"}]
               [{:week "XX"}]
               [{:week "5X"}]
               [{:day-of-year "001"}]
               [{:day-of-year "XXX"}]
               [{:day-of-year "366"}]
               [{:day-of-year "36X"}]]
        invalid [[{:second "61"}]
                 [{:second "7X"}]
                 [{:minute "60"}]
                 [{:minute "6X"}]
                 [{:hour "25"}]
                 [{:hour "3X"}]
                 [{:week "00"}]
                 [{:week "54"}]
                 [{:week "6X"}]
                 [{:day-of-year "000"}]
                 [{:day-of-year "367"}]
                 [{:day-of-year "37X"}]
                 [{:hour "00"} {:week "00"}]]]

    (doall
     (for [testcase valid]
       (testing (str testcase)
         (is (false? (invalidator/invalid-tree? testcase))))))

    (doall
     (for [testcase invalid]
       (testing (str testcase)
         (is (true? (invalidator/invalid-tree? testcase))))))))
