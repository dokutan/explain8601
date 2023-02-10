(ns explain8601.invalidator
  (:require [clojure.string :as string]))

(defn invalid-string?
  "Check if `s` is an invalid expression"
  [s]
  (string/includes? s "TT"))

(defn invalid-tree?
  "Check if `tree` is an invalid parse tree produced by `parser/parse-all-8601-3`"
  [tree]
  (let [numberx->int ;; convert a str that can contain 'X' to an int
        (fn [x] (-> x
                    (string/replace "X" "0")
                    (Integer/parseInt)))

        checks
        {:second (fn [s] (not (<= 0 (numberx->int s) 60)))
         :minute (fn [s] (not (<= 0 (numberx->int s) 59)))
         :hour (fn [s] (not (<= 0 (numberx->int s) 24)))
         :day-of-year (fn [s] (not (or
                                    (<= 1 (numberx->int s) 366)
                                    (= s "XXX"))))
         :week (fn [s] (not (or
                             (<= 1 (numberx->int s) 53)
                             (= s "XX"))))}

        nodes
        (->> tree
             flatten
             (filter map?))]

    (->>
     (map
      #(for [[node-key node-value] %1
             [check-key check-fn] checks]
         (if (= check-key node-key)
           (check-fn node-value)
           false))
      nodes)
     flatten
     (reduce #(or %1 %2)))))
