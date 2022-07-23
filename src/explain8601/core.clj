(ns explain8601.core
  (:require [explain8601.parser :as parser]
            [zprint.core :as zp])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (zp/czprint (parser/parse-all-8601 (nth args 0))))
