(ns ez-time.time
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ez-time.timezone :as tz]
            [ez-time.util :as util])
  (:refer-clojure :exclude [second max min])
  (:import (org.joda.time DateTime DateMidnight LocalDateTime
                          YearMonth LocalDate)))


(def ^:private months {"Jan" 1
                       "Feb" 2
                       "Mar" 3
                       "Apr" 4
                       "May" 5
                       "Jun" 6
                       "Jul" 7
                       "Aug" 8
                       "Sep" 9
                       "Oct" 10
                       "Nov" 11
                       "Dec" 12})

(defn- read-leapseconds []
  (let [data (io/reader (io/resource "tzdata/leapseconds"))]
    (map (fn [[_ year month day & _]]
           {:year (Long/parseLong year)
            :month (get months month)
            :day (Long/parseLong day)
            :leap-ms 1000 ;; leap second in milliseconds
            })
     (map #(str/split % #"\t")
          (filter #(re-find #"^Leap" %)
                  (line-seq data))))))

(def ^:private leapseconds (read-leapseconds))


(defprotocol EzTimeProtocol
  (year [obj])
  (month [obj])
  (day [obj])
  (hour [obj])
  (minute [obj])
  (second [obj])
  (millisecond [obj])

  (after? [a b])
  (before? [a b])
  (plus [obj period])
  (minus [obj period])
  (leap? [obj]))

(defrecord EzTime [milliseconds timezone])



(extend-protocol EzTimeProtocol
  org.joda.time.DateTime
  (year [obj] (.getYear obj))
  (month [obj] (.getMonthOfYear obj))
  (day [obj] (.getDayOfMonth obj))
  (hour [obj] (.getHourOfDay obj))
  (minute [obj] (.getMinuteOfHour obj))
  (second [obj] (.getSecondOfMinute obj))
  (millisecond [obj] (.getMillisOfSecond obj))
  (after? [a b] (.isAfter a b))
  (before? [a b] (.isBefore a b))
  (plus [obj period] (.plus obj period))
  (minus [obj period] (.minus obj period))
  (leap? [obj] (.isLeap (.year obj)))

  org.joda.time.DateMidnight
  (year [obj] (.getYear obj))
  (month [obj] (.getMonthOfYear obj))
  (day [obj] (.getDayOfMonth obj))
  (hour [obj] (.getHourOfDay obj))
  (minute [obj] (.getMinuteOfHour obj))
  (second [obj] (.getSecondOfMinute obj))
  (millisecond [obj] (.getMillisOfSecond obj))
  (after? [a b] (.isAfter a b))
  (before? [a b] (.isBefore a b))
  (plus [obj period] (.plus obj period))
  (minus [obj period] (.minus obj period))
  (leap? [obj] (.isLeap (.year obj)))

  org.joda.time.LocalDateTime
  (year [obj] (.getYear obj))
  (month [obj] (.getMonthOfYear obj))
  (day [obj] (.getDayOfMonth obj))
  (hour [obj] (.getHourOfDay obj))
  (minute [obj] (.getMinuteOfHour obj))
  (second [obj] (.getSecondOfMinute obj))
  (millisecond [obj] (.getMillisOfSecond obj))
  (after? [a b] (.isAfter a b))
  (before? [a b] (.isBefore a b))
  (plus [obj period] (.plus obj period))
  (minus [obj period] (.minus obj period))
  (leap? [obj] (.isLeap (.year obj)))

  org.joda.time.YearMonth
  (year [obj] (.getYear obj))
  (month [obj] (.getMonthOfYear obj))
  (after? [a b] (.isAfter a b))
  (before? [a b] (.isBefore a b))
  (plus [obj period] (.plus obj period))
  (minus [obj period] (.minus obj period))
  (leap? [obj] (.isLeap (.year obj)))

  org.joda.time.LocalDate
  (year [obj] (.getYear obj))
  (month [obj] (.getMonthOfYear obj))
  (day [obj] (.getDayOfMonth obj))
  (after? [a b] (.isAfter a b))
  (before? [a b] (.isBefore a b))
  (plus [obj period] (.plus obj period))
  (minus [obj period] (.minus obj period))
  (leap? [obj] (.isLeap (.year obj))))




(defn year->ms
  "Get back the year in number of milliseconds from or before 1970"
  [year & [epoch?]]
  ;; have 1970 as the origo because of epoch time
  (let [year-range (if (>= year 1970) (range 1970 year) (range year 1970))
        days (apply + (map (fn [y] (if (util/leap? y) 366 365)) year-range))]
    (if (>= year 1970)
      (if epoch?
        (* days 86400000)
        (* days 86400000)) ;; add check against leap seconds
      (* days -86400000))))

(defmulti datetime (fn [& args] (type (last args))))
(defmethod datetime :default
  ([year]
     (datetime year 1 1 0 0 0 0))
  ([year month]
     (datetime year month 1 0 0 0 0))
  ([year month day]
     (datetime year month day 0 0 0 0))
  ([year month day hour]
     (datetime year month day hour 0 0 0))
  ([year month day hour minute]
     (datetime year month day hour minute 0 0))
  ([year month day hour minute second]
     (datetime year month day hour minute second 0))
  ([year month day hour
    minute second millis]
     (EzTime. 0 tz/utc)))
