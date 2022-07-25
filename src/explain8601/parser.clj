(ns explain8601.parser
  (:require [instaparse.core :as insta]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn- triml0
  "Remove leading zeros from `str`, ignoring a leading minus."
  [str]
  (string/replace str #"^(-)?0*" "$1"))

(def parse-8601
  (insta/parser
   (clojure.java.io/resource "8601.abnf")
   :output-format :hiccup ; :hiccup or :enlive
   :input-format :abnf))

(defn- parse-date-year
  "Parse a :date-year node"
  [& c]
  (let [sign (first c)
        c (if (vector? sign)
            (rest c)
            c)
        sign (if (and (vector? sign)
                      (= :minus (first sign)))
               "-"
               "")
        exponent (first (filter vector? c))
        exponent (if (and (vector? exponent)
                          (= :exponent (first exponent)))
                   (Integer/parseInt (second exponent))
                   0)
        sig-digits (last (filter vector? c))
        sig-digits (if (and (vector? sig-digits)
                            (= :significant-digits (first sig-digits)))
                     (Integer/parseInt (second sig-digits))
                     nil)
        year (apply str (filter string? c))
        year (str sign year (.repeat "0" exponent))
        r (if sig-digits
            {:year year
             :min (str sign
                       (.substring year 0 sig-digits)
                       (.repeat "0" (- (.length year) sig-digits)))
             :max (str sign
                       (.substring year 0 sig-digits)
                       (.repeat "9" (- (.length year) sig-digits)))}
            {:year year})]
    [:date-year r]))

(defn- parse-qualifier
  "Parse a :qualifier node"
  [qualifier]
  (condp = qualifier
    "?" [:qualifier #{:uncertain}]
    "~" [:qualifier #{:approximate}]
    "%" [:qualifier #{:uncertain :approximate}]
    [:qualifier #{}]))

(defn- parse-duration
  "Parse a :duration node"
  [& c]
  (let [minus (first c)
        c (if (vector? minus)
            (rest c)
            c)]
    [:duration {:components (vec c)
                :reverse (vector? minus)}]))

(defn- parse-component
  "Generic function for parsing components like year, month, …"
  [kw]
  (fn [& c]
    (let [c (map
             (fn [n]
               (if (and (vector? n) (= :minus (first n)))
                 "-"
                 n))
             c)
          qualifier (second (first (filter vector? c)))
          value (triml0 (apply str (filter string? c)))]
      (if qualifier
        {kw value
         :qualifier qualifier}
        {kw value}))))

(defn- move-qualifier
  "Apply free qualifiers to components"
  [kw]
  (fn [& c]
    (loop [c (reverse c)
           qualifier nil
           return []]
      (if (> (count c) 0)
        (if (and (vector? (first c))
                 (= :qualifier (first (first c))))
          (recur (rest c)
                 (set/union qualifier (second (first c)))
                 return)
          (recur (rest c)
                 qualifier
                 (conj
                  return
                  (if (nil? qualifier)
                    (first c)
                    (assoc
                     (first c)
                     :qualifier
                     (set/union (get (first c) :qualifier) qualifier))))))
        (reverse (conj return kw))))))

(defn parse-all-8601
  [string]
  (insta/transform
   {:expression identity
    :DIGIT identity
    :DIGITX identity
    ;:date-time (fix-date-time-right-qualifier :date-time)
    :date-year parse-date-year
    :qualifier parse-qualifier
    :duration parse-duration
    :grouping (parse-component :grouping)
    :grouping-e (parse-component :grouping)
    :year (parse-component :year)
    :year-e (parse-component :year)
    :month (parse-component :month)
    :month-e (parse-component :month)
    :week (parse-component :week)
    :week-e (parse-component :week)
    :day (parse-component :day)
    :day-e (parse-component :day)
    :weekday (parse-component :weekday)
    :weekday-e (parse-component :weekday)
    :day-of-year (parse-component :day-of-year)
    :day-of-year-e (parse-component :day-of-year)
    :hour (parse-component :hour)
    :hour-e (parse-component :hour)
    :minute (parse-component :minute)
    :minute-e (parse-component :minute)
    :second (parse-component :second)
    :second-e (parse-component :second)
    :years (parse-component :years)
    :months (parse-component :months)
    :weeks (parse-component :weeks)
    :days (parse-component :days)
    :hours (parse-component :hours)
    :minutes (parse-component :minutes)
    :seconds (parse-component :seconds)
    :calendar-date (move-qualifier :calendar-date)
    :calendar-date-e (move-qualifier :calendar-date)
    :week-date (move-qualifier :week-date)
    :week-date-e (move-qualifier :week-date)
    :ordinal-date (move-qualifier :ordinal-date)
    :ordinal-date-e (move-qualifier :ordinal-date)
    :week-date-time-e (move-qualifier :week-date-time-e)}
   (insta/parses parse-8601 string)))