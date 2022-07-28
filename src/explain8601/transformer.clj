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

(defn- transform-date-year
  "Tranform a :date-year node"
  [props]
  (if (get props :min)
    (format
     "a year between %s and %s, estimated %s"
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
    (mapv (fn [m]
            (reduce-kv
             (fn [_ k v]
               (.replace (str v " " k) ":" "")) "" m))
          (get props :components)))
   (if (get props :reverse)
     " in the reverse direction"
     "")))

(defn transform-8601
  [tree]
  (insta/transform
   {:date-year transform-date-year
    :duration transform-duration}
   tree))