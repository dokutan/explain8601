(ns explain8601.parser
  (:require [instaparse.core :as insta]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn- triml0
  "Remove leading zeros from `str`, ignoring a leading minus."
  [str]
  (let [r (string/replace str #"^(-)?0*" "$1")]
    (if (empty? r)
      "0"
      r)))

(def ^:private flattenv (comp vec flatten))

(def ^:private parse-8601
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

(defn- parse-interval-limit
  "Parse the start or end of an interval"
  [limit type-hint]
  (let [value (cond
                (and (vector? limit)
                     (= :interval-limit-unknown (ffirst limit)))
                :unknown

                (and (vector? limit)
                     (= :interval-limit-open (ffirst limit)))
                :open

                (and (= :calendar-date (ffirst limit))
                     (or (= :time (first (second limit)))
                         (pos? (count (filter map? limit)))))
                (flattenv [:calendar-date-time (filter map? (flatten limit))])

                (and (= :week-date (ffirst limit))
                     (or (= :time (first (second limit)))
                         (pos? (count (filter map? limit)))))
                (flattenv [:week-date-time (filter map? (flatten limit))])

                (and (= :ordinal-date (ffirst limit))
                     (or (= :time (first (second limit)))
                         (pos? (count (filter map? limit)))))
                (flattenv [:ordinal-date-time (filter map? (flatten limit))])

                (or (= :calendar-date-tail (ffirst limit))
                    (= :week-date-tail (ffirst limit))
                    (= :ordinal-date-tail (ffirst limit)))
                (flattenv [(filter map? (flatten limit))])

                :else
                limit)]
    {(if (= :duration (first value))
       :duration
       type-hint)
     value}))

(defn- copy-missing-values
  "Copy missing values in an interval end from the interval start"
  [start end]
  (if (and (get end :end)
           (get start :start)
           (map? (first (get end :end))))
    (let [end (get end :end)
          start (get start :start)
          end-keys (apply merge (map (fn [e] {(set (keys e)) e}) (filter map? end)))
          start-keys (apply merge (map (fn [e] {(set (keys e)) e}) (filter map? start)))]
      {:end (vec (concat [(first start)] (vals (merge start-keys end-keys))))})
    end))

(defn- parse-interval
  "Parse a :duration node"
  [& c]
  (loop [start []
         end []
         state :start
         c c]
    (cond
      (zero? (count c))
      (let [start (parse-interval-limit start :start)
            end (parse-interval-limit end :end)
            end (copy-missing-values start end)]
        [:interval
         (merge start end)])

      (and (vector? (first c)) (= :solidus (first (first c))))
      (recur
       start
       end
       :end
       (rest c))

      (= :start state)
      (recur
       (conj start (first c))
       end
       state
       (rest c))

      :else
      (recur
       start
       (conj end (first c))
       state
       (rest c)))))

(defn- parse-date-time
  "Flatten a date and time expression"
  [kw]
  (fn [& c]
    (flattenv [kw (filter map? (flattenv c))])))

(defn- parse-timezone
  "Parse a :timezone node"
  [& c]
  (cond
    (and (= 1 (count c)) (=  "Z" (first c)))
    {:timezone :utc}

    :else
    {:timezone
     (if (and (= :pm (ffirst (filter vector? c)))
              (= "+" (second (first (filter vector? c)))))
       :plus
       :minus)
     :offset (vec (filter map? c))}))

(defn- parse-component
  "Generic function for parsing components like year, month, …"
  [kw]
  (fn [& c]
    (let [c (map
             (fn [n]
               (cond
                 (and (vector? n) (= :minus (first n)))
                 "-"
                 (and (vector? n) (= :pm (first n)))
                 (if (= "+" (second n))
                   ""
                   "-")
                 :else
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
                 (= :qualifier (ffirst c)))
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
        (vec (reverse (conj return kw)))))))

(defn parse-all-8601-1
  "The first pass of the parser"
  [string]
  (insta/parses parse-8601 string))

(defn parse-all-8601-2
  "The second pass of the parser"
  [tree]
  (insta/transform
   {:expression identity
    :DIGIT identity
    :DIGITX identity
    ;:date-time (fix-date-time-right-qualifier :date-time)
    :date-year parse-date-year
    :qualifier parse-qualifier
    :duration parse-duration

    :century (parse-component :century)
    :century-e (parse-component :century)
    :century-expanded (parse-component :century)
    :decade (parse-component :decade)
    :decade-e (parse-component :decade)
    :decade-expanded (parse-component :decade)
    :year (parse-component :year)
    :year-e (parse-component :year)
    :year-expanded (parse-component :year)
    :month (parse-component :month)
    :month-e (parse-component :month)
    :grouping (parse-component :grouping)
    :grouping-e (parse-component :grouping)
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

    :timezone parse-timezone
    :timezone-e parse-timezone


    :calendar-date (move-qualifier :calendar-date)
    :calendar-date-e (move-qualifier :calendar-date)
    :calendar-date-day (move-qualifier :calendar-date)
    :calendar-date-month (move-qualifier :calendar-date)
    :calendar-date-grouping (move-qualifier :calendar-date)
    :calendar-date-year (move-qualifier :calendar-date)
    :calendar-date-decade (move-qualifier :calendar-date)
    :calendar-date-century (move-qualifier :calendar-date)
    :calendar-date-day-e (move-qualifier :calendar-date)
    :calendar-date-month-e (move-qualifier :calendar-date)
    :calendar-date-grouping-e (move-qualifier :calendar-date)
    :calendar-date-year-e (move-qualifier :calendar-date)
    :calendar-date-decade-e (move-qualifier :calendar-date)
    :calendar-date-century-e (move-qualifier :calendar-date)
    :week-date (move-qualifier :week-date)
    :week-date-e (move-qualifier :week-date)
    :week-date-day (move-qualifier :week-date)
    :week-date-week (move-qualifier :week-date)
    :week-date-day-e (move-qualifier :week-date)
    :week-date-week-e (move-qualifier :week-date)
    :ordinal-date (move-qualifier :ordinal-date)
    :ordinal-date-e (move-qualifier :ordinal-date)
    :calendar-date-time-e (move-qualifier :calendar-date-time)
    :week-date-time-e (move-qualifier :week-date-time)
    :ordinal-date-time-e (move-qualifier :ordinal-date-time)
    :calendar-date-time (move-qualifier :calendar-date-time)
    :week-date-time (move-qualifier :week-date-time)
    :ordinal-date-time (move-qualifier :ordinal-date-time)
    :time (move-qualifier :time)
    :time-e (move-qualifier :time)
    :time-hour (move-qualifier :time)
    :time-hour-e (move-qualifier :time)
    :time-minute (move-qualifier :time)
    :time-minute-e (move-qualifier :time)
    :time-second (move-qualifier :time)
    :time-second-e (move-qualifier :time)}
   tree))

(defn parse-all-8601-3
  "The third pass of the parser"
  [tree]
  (insta/transform
   {;:calendar-date-time (parse-date-time :calendar-date-time)
    ;:week-date-time (parse-date-time :week-date-time)
    ;:ordinal-date-time (parse-date-time :ordinal-date-time)
    :interval parse-interval
    :interval-e parse-interval

    :calendar-date-time (parse-date-time :calendar-date-time)
    :week-date-time (parse-date-time :week-date-time)
    :ordinal-date-time (parse-date-time :ordinal-date-time)
    :date-time identity
    :date-time-e identity}
   tree))