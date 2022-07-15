(ns explain8601.parser-test
  (:require [clojure.test :refer :all]
            [explain8601.parser :as parser]))

(deftest parser-test
  (let [test-cases {"" '()
                    "00:00" '([:representation [:time [:hour "0" "0"] ":" [:minute "0" "0"]]])
                    "12:34,5" '([:representation [:time [:hour "1" "2"] ":" [:minute "3" "4"] [:fraction "," "5"]]])
                    "1234" '([:representation [:date [:calendar-date [:year "1" "2" "3" "4"]]]]
                             [:representation [:time [:hour "1" "2"] [:minute "3" "4"]]])
                    "2022-01-02" '([:representation [:date [:calendar-date [:year "2" "0" "2" "2"] "-" [:month "0" "1"] "-" [:day "0" "2"]]]])
                    "2022-100" '([:representation [:date [:ordinal-date [:year "2" "0" "2" "2"] "-" [:day-of-year "1" "0" "0"]]]])
                    "2022100" '([:representation [:date [:ordinal-date [:year "2" "0" "2" "2"] [:day-of-year "1" "0" "0"]]]])
                    "2022-W01-7" '([:representation [:date [:week-date [:year "2" "0" "2" "2"] "-" "W" [:week "0" "1"] "-" [:weekday "7"]]]])
                    "2022W017" '([:representation [:date [:week-date [:year "2" "0" "2" "2"] "W" [:week "0" "1"] [:weekday "7"]]]])
                    "2022-01-02T03:04" '([:representation [:date-time [:year "2" "0" "2" "2"] "-" [:month "0" "1"] "-" [:day "0" "2"]
                                                           "T" [:time [:hour "0" "3"] ":" [:minute "0" "4"]]]])
                    "PT01H" '([:representation [:duration "P" "T" [:hours "0" "1"] "H"]])
                    "P0001-02" '([:representation [:duration "P" [:calendar-date [:year "0" "0" "0" "1"] "-" [:month "0" "2"]]]])
                    "2022-01/02" '([:representation [:interval [:year "2" "0" "2" "2"] "-" [:month "0" "1"] [:solidus "/"] [:calendar-date-tail-month [:month "0" "2"]]]])
                    "R3/2022/P1Y" '([:representation [:repeating-interval "R" [:repetitions "3"] [:solidus "/"]
                                                      [:interval [:date [:calendar-date [:year "2" "0" "2" "2"]]] [:solidus "/"] [:duration "P" [:years "1"] "Y"]]]])}]
    (doall (map
            (fn [s]
              (testing s (is (=
                              (parser/parse-all-8601 s)
                              (test-cases s)))))
            (keys test-cases)))))