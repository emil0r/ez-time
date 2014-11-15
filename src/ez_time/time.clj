(ns ez-time.time
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ez-time.timezone :as tz]
            [ez-time.util :as util])
  (:refer-clojure :exclude [second max min])
  (:import (org.joda.time DateTime DateMidnight LocalDateTime
                          YearMonth LocalDate)))



(defprotocol EzPeriodProtocol
  (get-milliseconds [instant period]))

(defrecord EzPeriod [year month month-rest week day hour minute second millisecond]
  EzPeriodProtocol
  (get-milliseconds [instant
                     {:keys [year month month-rest week day
                             hour minute second millisecond]
                      :or {year 0, month 0, week 0, day 0,
                           hour 0, minute 0, second 0, millisecond 0}}]
    (+ (* 365 24 3600000 year)
       (* 7 24 3600000 week)
       (* 24 3600000 day)
       (* 3600000 hour)
       (* 60000 minute)
       (* 1000 second)
       millisecond)))

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
  (plus [obj period] [obj period perodic?])
  (minus [obj period] [obj period perodic?])
  (create-period [a b])
  (leap? [obj]))

(defrecord EzTime [milliseconds timezone
                   year month day hour minute second millisecond])

(extend-protocol EzTimeProtocol
  EzTime
  (year [obj] (:year obj))
  (month [obj] (:month obj))
  (day [obj] (:day obj))
  (hour [obj] (:hour obj))
  (minute [obj] (:minute obj))
  (second [obj] (:second obj))
  (millisecond [obj] (:millisecond obj))

  (after? [a b] (if (and (:tz a) (:tz b))
                  (> (+ (-> a :tz :milliseconds)
                        (:milliseconds a))
                     (+ (-> b :tz :milliseconds)
                        (:milliseconds b)))
                  (> (:milliseconds a) (:milliseconds b))))
  (before? [a b] (if (and (:tz a) (:tz b))
                   (< (+ (-> a :tz :milliseconds)
                         (:millisecond a))
                      (+ (-> b :tz :milliseconds)
                         (:milliseconds b)))
                   (< (:millisecond a) (:milliseconds b))))
  (plus [obj period] obj)
  (minus [obj period] obj)
  (leap? [obj] (util/leap? (:year obj)))
  )



;; (extend-protocol EzTimeProtocol
;;   org.joda.time.DateTime
;;   (year [obj] (.getYear obj))
;;   (month [obj] (.getMonthOfYear obj))
;;   (day [obj] (.getDayOfMonth obj))
;;   (hour [obj] (.getHourOfDay obj))
;;   (minute [obj] (.getMinuteOfHour obj))
;;   (second [obj] (.getSecondOfMinute obj))
;;   (millisecond [obj] (.getMillisOfSecond obj))
;;   (after? [a b] (.isAfter a b))
;;   (before? [a b] (.isBefore a b))
;;   (plus [obj period] (.plus obj period))
;;   (minus [obj period] (.minus obj period))
;;   (leap? [obj] (.isLeap (.year obj)))

;;   org.joda.time.DateMidnight
;;   (year [obj] (.getYear obj))
;;   (month [obj] (.getMonthOfYear obj))
;;   (day [obj] (.getDayOfMonth obj))
;;   (hour [obj] (.getHourOfDay obj))
;;   (minute [obj] (.getMinuteOfHour obj))
;;   (second [obj] (.getSecondOfMinute obj))
;;   (millisecond [obj] (.getMillisOfSecond obj))
;;   (after? [a b] (.isAfter a b))
;;   (before? [a b] (.isBefore a b))
;;   (plus [obj period] (.plus obj period))
;;   (minus [obj period] (.minus obj period))
;;   (leap? [obj] (.isLeap (.year obj)))

;;   org.joda.time.LocalDateTime
;;   (year [obj] (.getYear obj))
;;   (month [obj] (.getMonthOfYear obj))
;;   (day [obj] (.getDayOfMonth obj))
;;   (hour [obj] (.getHourOfDay obj))
;;   (minute [obj] (.getMinuteOfHour obj))
;;   (second [obj] (.getSecondOfMinute obj))
;;   (millisecond [obj] (.getMillisOfSecond obj))
;;   (after? [a b] (.isAfter a b))
;;   (before? [a b] (.isBefore a b))
;;   (plus [obj period] (.plus obj period))
;;   (minus [obj period] (.minus obj period))
;;   (leap? [obj] (.isLeap (.year obj)))

;;   org.joda.time.YearMonth
;;   (year [obj] (.getYear obj))
;;   (month [obj] (.getMonthOfYear obj))
;;   (after? [a b] (.isAfter a b))
;;   (before? [a b] (.isBefore a b))
;;   (plus [obj period] (.plus obj period))
;;   (minus [obj period] (.minus obj period))
;;   (leap? [obj] (.isLeap (.year obj)))

;;   org.joda.time.LocalDate
;;   (year [obj] (.getYear obj))
;;   (month [obj] (.getMonthOfYear obj))
;;   (day [obj] (.getDayOfMonth obj))
;;   (after? [a b] (.isAfter a b))
;;   (before? [a b] (.isBefore a b))
;;   (plus [obj period] (.plus obj period))
;;   (minus [obj period] (.minus obj period))
;;   (leap? [obj] (.isLeap (.year obj))))




(defmulti datetime (fn [& args] (type (last args))))
(defmethod datetime ez_time.timezone.TimeZone
  ([year tz]
     (datetime year 1 1 0 0 0 0 tz))
  ([year month tz]
     (datetime year month 1 0 0 0 0 tz))
  ([year month day tz]
     (datetime year month day 0 0 0 0 tz))
  ([year month day hour tz]
     (datetime year month day hour 0 0 0 tz))
  ([year month day hour minute tz]
     (datetime year month day hour minute 0 0 tz))
  ([year month day hour minute second tz]
     (datetime year month day hour minute second 0 tz))
  ([year month day hour minute second millisecond tz]
     (EzTime. (+ (util/year->ms year)
                 (util/month->ms year month)
                 (* day 24 3600 1000)
                 (* hour 3600 1000)
                 (* minute 60 1000)
                 (* second 1000)
                 millisecond) tz
                 year month day hour minute second millisecond)))
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
  ([year month day hour minute second millisecond]
     (EzTime. (+ (util/year->ms year)
                 (util/month->ms year month)
                 (* day 24 3600 1000)
                 (* hour 3600 1000)
                 (* minute 60 1000)
                 (* second 1000)
                 millisecond) nil
                 year month day hour minute second millisecond)))
