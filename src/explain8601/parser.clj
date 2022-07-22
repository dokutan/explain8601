(ns explain8601.parser
  (:require [instaparse.core :as insta]))

(def parse-8601
  (insta/parser
   (clojure.java.io/resource "8601.abnf")
   :output-format :hiccup ; :hiccup or :enlive
   :input-format :abnf))

(defn parse-all-8601
  [string]
  (insta/transform
   {:DIGIT (fn [d] d)}
   (insta/parses parse-8601 string)))