(ns explain8601.core
  (:require [explain8601.parser :as parser]
            [explain8601.transformer :as transformer]
            [zprint.core :as zp]
            [clojure.tools.cli :as cli]
            [clojure.string :as string])
  (:gen-class))

(def ^:private usage
  "explain8601 [opts] -e expression")

(def ^:private cli-options
  [["-e" "--expression EXPRESSION" "the expression that should be explained"]
   ["-d" "--debug" "show intermediate results for debugging"]
   ["-h" "--help" "show this help message"]])

(defn -main
  "The main function"
  [& args]
  (let [opts (cli/parse-opts args cli-options)]
    (cond
      (:errors opts)
      (println (string/join "\n" (:errors opts)))

      (or (:help (:options opts))
          (not (:expression (:options opts))))
      (println (str usage "\n" (:summary opts)))

      :else
      (let [expression (:expression (:options opts))
            parse-tree-1 (parser/parse-all-8601-1 expression)
            parse-tree-2 (parser/parse-all-8601-2 parse-tree-1)
            parse-tree-3 (parser/parse-all-8601-3 parse-tree-2)
            descriptions (transformer/transform-8601 parse-tree-3)]
        (when (:debug (:options opts))
          (zp/czprint expression)
          (println "↓")
          (zp/czprint parse-tree-1)
          (println "↓")
          (zp/czprint parse-tree-2)
          (println "↓")
          (zp/czprint parse-tree-3)
          (println "↓"))
        (zp/czprint descriptions)))))
