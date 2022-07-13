(ns explain8601.core
  (:require [instaparse.core :as insta])
  (:gen-class))

(insta/set-default-input-format! :abnf)

(def parser
  (insta/parser
   (clojure.java.io/resource "8601.abnf")
   ;:output-format :enlive
   :input-format :abnf))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (parser (nth args 0))))
