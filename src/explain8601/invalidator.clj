(ns explain8601.invalidator)

(defn invalid-string?
  "Check if `s` is an invalid expression"
  [s]
  (.contains s "TT"))