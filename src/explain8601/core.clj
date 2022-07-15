(ns explain8601.core
  (:require [explain8601.parser :as parser])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (parser/parse-all-8601 (nth args 0))))
