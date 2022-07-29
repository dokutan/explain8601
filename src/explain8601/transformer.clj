(ns explain8601.transformer
  (:require [instaparse.core :as insta]
            [clojure.string :as string]))

(defn- join-and
  "Join items using ', ' and ' and '"
  [items]
  (let [start (reverse (rest (reverse items)))
        end (last items)]
    (if (empty? start)
      (str end)
      (str
       (string/join ", " start)
       " and "
       end))))

(defn- make-singular
  "Remove trailing 's' from `str` if `i` = 1"
  [str i]
  (if (> (Float/parseFloat i) 1)
    str
    (subs str 0 (dec (count str)))))

(defn- transform-qualifier
  "Describe the `qualifiers`"
  [qualifiers]
  (cond
    (and (contains? qualifiers :uncertain)
         (contains? qualifiers :approximate))
    " (uncertain and approximate)"
    
    (contains? qualifiers :uncertain)
    " (uncertain)"
    
    (contains? qualifiers :approximate)
    " (approximate)"
    
    :else
    ""))

(defn- transform-duration-component
  "Describes the component `c` of a duration"
  [c]
  (reduce-kv
   (fn [_ k v]
     (.replace (str v " " (make-singular (str k) v) (transform-qualifier (get c :qualifier))) ":" ""))
   ""
   (dissoc c :qualifier)))

(defn- transform-date-year
  "Tranform a :date-year node"
  [props]
  (if (get props :min)
    (format
     "a year between %s and %s, estimated to be %s"
     (get props :min)
     (get props :max)
     (get props :year))
    (format "the year %s" (get props :year))))

(defn- transform-duration
  "Tranform a :duration node"
  [props]
  (format
   "a duration of %s%s"
   (join-and
    (mapv transform-duration-component (get props :components)))
   (if (get props :reverse)
     " in the reverse direction"
     "")))

(defn transform-8601
  [tree]
  (insta/transform
   {:date-year transform-date-year
    :duration transform-duration}
   tree))