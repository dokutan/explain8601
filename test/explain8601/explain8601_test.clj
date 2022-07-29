(ns explain8601.explain8601-test
  (:require [clojure.test :refer :all]
            [explain8601.parser :as parser]
            [explain8601.transformer :as transformer]))

(deftest explain8601-test
  (let [explain8601 (fn [expression]
                      (-> expression
                          parser/parse-all-8601-1
                          parser/parse-all-8601-2
                          parser/parse-all-8601-3
                          transformer/transform-8601
                          (transformer/descriptions->description expression)))
        test-cases {"" "'' does not appear to be a valid expression.\n"
                    "P3W" "'P3W' represents a duration of 3 weeks.\n"
                    "-P1W" "'-P1W' represents a duration of 1 week in the reverse direction.\n"}]
    (doall (map
            (fn [s]
              (testing s (is (=
                              (explain8601 s)
                              (test-cases s)))))
            (keys test-cases)))))