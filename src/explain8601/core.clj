(ns explain8601.core
  (:require [explain8601.parser :as parser]
            [explain8601.transformer :as transformer]
            [zprint.core :as zp])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (zp/czprint (transformer/transform-8601 (parser/parse-all-8601 (nth args 0)))))
