(ns explain8601.invalidator
  (:require [clojure.string :as string]))

(defn invalid-string?
  "Check if `s` is an invalid expression"
  [s]
  (string/includes? s "TT"))

(defn numberx->float
  "convert a str that can contain 'X' to a float"
  [x]
  (-> x
      (string/replace "X" "0")
      (string/replace "," ".")
      (Float/parseFloat)))

(defn invalid-tree-1?
  "Check if `tree` is an invalid parse tree produced by `parser/parse-all-8601-3`, part 1"
  [tree]
  (let [checks
        {:second (fn [s] (not (<= 0 (numberx->float s) 60)))
         :minute (fn [s] (not (<= 0 (numberx->float s) 59)))
         :hour (fn [s] (not (<= 0 (numberx->float s) 24)))
         :day-of-year (fn [s] (not (or
                                    (<= 1 (numberx->float s) 366)
                                    (= s "XXX"))))
         :week (fn [s] (not (or
                             (<= 1 (numberx->float s) 53)
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

(defn invalid-tree-2?
  "Check if `tree` is an invalid parse tree produced by `parser/parse-all-8601-3`, part 2"
  [tree]
  (let [components (->> tree
                        flatten
                        (filter map?)
                        (apply merge))]
    (if (= (:hour components) "24")
      (not
       (and
        (zero? (numberx->float (:minute components "0")))
        (zero? (numberx->float (:second components "0")))))
      false)))

(defn invalid-tree?
  "Check if `tree` is an invalid parse tree produced by `parser/parse-all-8601-3`"
  [tree]
  (or (invalid-tree-1? tree)
      (invalid-tree-2? tree)))
