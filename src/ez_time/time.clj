(ns ez-time.time
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ez-time.timezone :as tz]
            [ez-time.util :as util])
  (:refer-clojure :exclude [max min format second]))



(defprotocol EzPeriodProtocol
  (years [period])
  (months [period])
  (weeks [period])
  (days [period])
  (minutes [period])
  (seconds [period])
  (milliseconds [period])
  (get-milliseconds [period instant]))

(defrecord EzPeriod [years months month-rest weeks days hours minutes seconds milliseconds]
  EzPeriodProtocol
  (years [period] (:years period))
  (months [period] (:months period))
  (weeks [period] (:weeks period))
  (days [period] (:days period))
  (minutes [period] (:minutes period))
  (seconds [period] (:seconds period))
  (milliseconds [period] (:milliseconds period))
  (get-milliseconds [{:keys [year month month-rest week day
                             hour minute second millisecond]
                      :or {year 0, month 0, week 0, day 0,
                           hour 0, minute 0, second 0, millisecond 0}}
                     instant]
    (+ (* 365 24 3600000 year)
       (* 7 24 3600000 week)
       (* 24 3600000 day)
       (* 3600000 hour)
       (* 60000 minute)
       (* 1000 second)
       millisecond)))

(defprotocol EzTimeProtocol
  (year [instant])
  (month [instant])
  (day [instant])
  (hour [instant])
  (minute [instant])
  (second [instant])
  (millisecond [instant])
  (raw-milliseconds [instant])

  (after? [a b])
  (before? [a b])
  (plus [instant period] [instant period perodic?])
  (minus [instant period] [instant period perodic?])
  (leap? [instant]))

(defrecord EzTime [milliseconds timezone
                   year month day hour minute second millisecond])

(defrecord EzInterval [start end])

(defprotocol EzFormatProtocol
  (format [instant] [instant to]))

(defprotocol EzConvertProtocol
  (convert [instant] [instant to] [instant to tz]))

(extend-protocol EzConvertProtocol
  java.lang.Long
  (convert
    ([instant] (convert instant EzTime nil))
    ([instant to] (convert instant to nil))
    ([instant to tz]
       (case to
         (map->EzTime (assoc (util/long-to-map instant)
                        :timezone tz))))))

(defprotocol EzParseControl
  (parse [instant fmt]))



(extend-protocol EzTimeProtocol
  EzTime
  (year [instant] (:year instant))
  (month [instant] (:month instant))
  (day [instant] (:day instant))
  (hour [instant] (:hour instant))
  (minute [instant] (:minute instant))
  (second [instant] (:second instant))
  (millisecond [instant] (:millisecond instant))
  (raw-milliseconds [instant] (:milliseconds instant))

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
                   (< (:milliseconds a) (:milliseconds b))))
  (plus [instant period]
    (+ (raw-milliseconds instant)
       (get-milliseconds period instant)))
  (minus [instant period] instant)
  (leap? [instant] (util/leap? (:year instant)))
  )

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

(defn period
  "data -> map with keys 'year month month-rest week day hour
minute second millisecond'"
  [data]
  (map->EzPeriod data))

(defn interval
  [start end]
  (map->EzInterval {:start start :end end}))
