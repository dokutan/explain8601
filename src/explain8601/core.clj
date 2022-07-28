(ns explain8601.core
  (:require [explain8601.parser :as parser]
            [explain8601.transformer :as transformer]
            [zprint.core :as zp]
            [instaparse.core :as insta])
  (:gen-class))

(defn -main
  "The main function"
  [& args]
  (let [expression (nth args 0)
        parse-tree-1 (parser/parse-all-8601-1 expression)
        parse-tree-2 (parser/parse-all-8601-2 parse-tree-1)
        parse-tree-3 (parser/parse-all-8601-3 parse-tree-2)
        description (transformer/transform-8601 parse-tree-3)]
    (println expression)
    (println "↓")
    (zp/czprint parse-tree-1)
    (println "↓")
    (zp/czprint parse-tree-2)
    (println "↓")
    (zp/czprint parse-tree-3)
    (println "↓")
    (println description)))
