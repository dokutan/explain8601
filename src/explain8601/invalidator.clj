(ns explain8601.invalidator
  (:require [clojure.string :as string]))

(defn invalid-string?
  "Check if `s` is an invalid expression"
  [s]
  (string/includes? s "TT"))