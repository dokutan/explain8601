(ns explain8601.transformer
  (:require [instaparse.core :as insta]
            [clojure.string :as string]))

(declare transform-8601)

(defn- find-map-with
  "Finds the first map in `s` that has key `k`"
  [s k]
  (first
   (filter
    (fn [m]
      (and (map? m)
           (get m k)))
    s)))

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

(defn- singular
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
     (.replace (str v " " (singular (str k) v) (transform-qualifier (get c :qualifier))) ":" ""))
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

(defn- grouping->name
  "Convert a :grouping value to its description"
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

(defn- weekday->name
  "Convert the number of a weekday to its name"
  [weekday]
  (case weekday
    "1" "monday"
    "2" "tuesday"
    "3" "wednesday"
    "4" "thursday"
    "5" "friday"
    "6" "saturday"
    "7" "sunday"
    "X" "an unspecified day"
    "X*" "an unspecified day"
    (str "day " weekday)))

(defn- month->name
  "Convert the number of a month to its name"
  [month]
  (case month
    "1" "january"
    "2" "february"
    "3" "march"
    "4" "april"
    "5" "may"
    "6" "june"
    "7" "july"
    "8" "august"
    "9" "september"
    "10" "october"
    "11" "november"
    "12" "december"
    "XX" "an unspecified month"
    "X*" "an unspecified month"
    (str "month " month)))

(defn- ordinal
  "Return the ordinal version of `i`"
  [i]
  (condp (fn [x y] (string/ends-with? y x)) i
    "11" (str i "th")
    "12" (str i "th")
    "13" (str i "th")
    "1" (str i "st")
    "2" (str i "nd")
    "3" (str i "rd")
    (str i "th")))

(defn- transform-calendar-date
  "Transform a :calendar-date node"
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
     (grouping->name (:grouping (second props)))
     (transform-qualifier (:qualifier (second props)))
     (:year (first props))
     (transform-qualifier (:qualifier (first props))))


    :else
    (let [year (find-map-with props :year)
          month (find-map-with props :month)
          day (find-map-with props :day)]
      (str
       (if day
         (str "the " (ordinal (:day day)) " day" (transform-qualifier (:qualifier day)) " of ")
         "")
       (if month
         (str (month->name (:month month)) (transform-qualifier (:qualifier month)) " of ")
         "") 
       "the year " (:year year) (transform-qualifier (:qualifier year))))))

(defn- transform-week-date
  "Transform a :week-date node"
  [& props]
  (let [year (find-map-with props :year)
        week (find-map-with props :week)
        weekday (find-map-with props :weekday)]
    (str
     (if weekday
       (str (weekday->name (:weekday weekday)) (transform-qualifier (:qualifier weekday)) " of ")
       "")
     (if week
       (str "the " (ordinal (:week week)) " week" (transform-qualifier (:qualifier week)) " of ")
       "")
     "the year " (:year year) (transform-qualifier (:qualifier year)))))

(defn- transform-ordinal-date
  "Transform a :ordinal-date node"
  [& props]
  (let [year (find-map-with props :year) 
        day (find-map-with props :day-of-year)]
    (str
     (if day
       (str "the " (ordinal (:day-of-year day)) " day" (transform-qualifier (:qualifier day)) " of ")
       "")
     "the year " (:year year) (transform-qualifier (:qualifier year)))))

(defn- transform-time
  "Transform a :time node"
  [& props]
  (let [hour (find-map-with props :hour)
        minute (find-map-with props :minute)
        second (find-map-with props :second)]
    (str
     (join-and
      (filter
       string?
       [(when minute
          (str (:minute minute) (singular " minutes" (:minute minute)) (transform-qualifier (:qualifier minute))))
        (when second
          (str (:second second) (singular " seconds" (:second second)) (transform-qualifier (:qualifier second))))]))
     (if (or minute second) " past " "")
     (if hour
       (:hour hour)
       "0")
     " hours" (transform-qualifier (:qualifier hour)))))

(defn- transform-duration
  "Tranform a :duration node"
  [props & _]
  (format
   "a duration of %s%s"
   (join-and
    (mapv transform-duration-component (get props :components)))
   (if (get props :reverse)
     " in the reverse direction"
     "")))

(defn- transform-interval
  "Transform an :interval node"
  [props & _]
  (cond
    (and (:start props) (:end props) (= 1 (count (:start props))))
    (format
     "an interval with %s start, ending %s"
     (.replace (str (first (:start props))) ":" "")
     (first (transform-8601 (seq [(:end props)]))))

    (and (:start props) (:end props) (= 1 (count (:end props))))
    (format
     "an interval starting %s, end %s"
     (first (transform-8601 (seq [(:start props)])))
     (.replace (str (first (:end props))) ":" ""))

    (and (:start props) (:end props))
    (format
     "an interval from %s to %s"
     (first (transform-8601 (seq [(:start props)])))
     (first (transform-8601 (seq [(:end props)]))))

    (and (:start props) (:duration props))
    (format
     "an interval from %s, lasting for %s"
     (first (transform-8601 (seq [(:start props)])))
     (first (transform-8601 (seq [(:duration props)]))))

    (and (:end props) (:duration props))
    (format
     "an interval lasting for %s, ending %s"
     (first (transform-8601 (seq [(:duration props)])))
     (first (transform-8601 (seq [(:end props)]))))
    
    :else
    (str "an interval " props)))

(defn transform-8601
  [tree]
  (insta/transform
   {:date-year transform-date-year
    :calendar-date transform-calendar-date
    :week-date transform-week-date
    :ordinal-date transform-ordinal-date
    :time transform-time
    :calendar-date-time (fn [& p] (str (apply transform-calendar-date p) " at " (apply transform-time p)))
    :week-date-time (fn [& p] (str (apply transform-week-date p) " at " (apply transform-time p)))
    :ordinal-date-time (fn [& p] (str (apply transform-ordinal-date p) " at " (apply transform-time p)))
    :interval transform-interval
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