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
  (if (or (> (Float/parseFloat i) 1)
          (< (Float/parseFloat i) -1))
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

(defn- transform-grouping
  "Transform a :grouping value"
  [grouping]
  (case grouping
    "21" "spring"
    "22" "summer"
    "23" "autumn"
    "24" "winter"
    "25" "spring (northern hemisphere)"
    "26" "summer (northern hemisphere)"
    "27" "autumn (northern hemisphere)"
    "28" "winter (northern hemisphere)"
    "29" "spring (southern hemisphere)"
    "30" "summer (southern hemisphere)"
    "31" "autumn (southern hemisphere)"
    "32" "winter (southern hemisphere)"
    "33" "first quarter"
    "34" "second quarter"
    "35" "third quarter"
    "36" "fourth quarter"
    "37" "first quadrimester"
    "38" "second quadrimester"
    "39" "third quadrimester"
    "40" "first semester"
    "41" "second semester"
    (str "grouping " grouping)))

(defn- transform-calendar-date
  "Transform a :calendar-year node"
  [& props]
  (cond
    (:century (first props))
    (format
     "the century from %s to %s%s"
     (:start (first props))
     (:end (first props))
     (transform-qualifier (:qualifier (first props))))

    (:decade (first props))
    (format
     "the decade from %s to %s%s"
     (:start (first props))
     (:end (first props))
     (transform-qualifier (:qualifier (first props))))

    (and (:year (first props))
         (:grouping (second props)))
    (format
     "%s%s of %s%s"
     (transform-grouping (:grouping (second props)))
     (transform-qualifier (:qualifier (second props)))
     (:year (first props))
     (transform-qualifier (:qualifier (first props))))


    :else
    (vec (conj props :calendar-date))))

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
    :calendar-date transform-calendar-date
    :duration transform-duration}
   tree))

(defn descriptions->description
  "Convert a sequence of descriptions in to a single string"
  [descriptions expression]
  (case (count descriptions)
    0
    (format
     "'%s' does not appear to be a valid expression.\n"
     expression)
    1
    (format
     "'%s' represents %s.\n"
     expression
     (first descriptions))
    (format
     "'%s' is ambiguous and could represent one of the following:\n%s"
     expression
     (string/join (map (fn [s] (str " • " s "\n")) descriptions)))))