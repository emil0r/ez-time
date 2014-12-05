(ns ez-time.time
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ez-time.timezone :as tz]
            [ez-time.util :as util])
  (:import [java.util TimeZone])
  (:refer-clojure :exclude [max min format second]))



(defprotocol PeriodProtocol
  (years [period])
  (months [period])
  (weeks [period])
  (days [period])
  (minutes [period])
  (seconds [period])
  (milliseconds [period])
  (get-milliseconds [period instant direction]))

(defprotocol EzTimeProtocol
  (year [instant])
  (month [instant])
  (week [instant])
  (weekday [instant])
  (day [instant])
  (hour [instant])
  (minute [instant])
  (second [instant])
  (millisecond [instant])
  (raw [instant])
  (raw+ [instant])

  (after? [a b])
  (before? [a b])
  (equals? [a b])
  (same-tz? [a b])
  (within? [instant interval] [instant a b])
  (plus [instant period] [instant period perodic?])
  (minus [instant period] [instant period perodic?])
  (leap? [instant]))

(defrecord Period [years months weeks days hours minutes seconds milliseconds]
  PeriodProtocol
  (years [period] (:years period))
  (months [period] (:months period))
  (weeks [period] (:weeks period))
  (days [period] (:days period))
  (minutes [period] (:minutes period))
  (seconds [period] (:seconds period))
  (milliseconds [period] (:milliseconds period))
  (get-milliseconds
    [{:keys [years months weeks days
             hours minutes seconds milliseconds]}
     instant direction]
    (+ (* 31536000000 (or years 0))
       ;; add leap days
       (if years
         (* 86400000
            (if (= direction :plus)
              (util/leap-days (year instant) years)
              (util/leap-days (- (year instant) years) (year instant))))
         0)
       (if months
         ;; - count number of days for the months involved
         ;; - keep track of the year so we can get the extra day for leap years
         ;; - if we are going forward in time (plus) we start with the
         ;; range of months from the month of the year all the way up to
         ;; the number of months wished for. if it's going back in time
         ;; we start from the end position and work ourselves up to the
         ;; month of the year given
         ;; - we go to a 0-indexed range for the benefit of modulus and
         ;; then increase all the numbers in the range by 1 to go back
         ;; to a 1-indexed range
         (loop [days 0
                year (year instant)
                [month & months] (map #(inc (mod % 12))
                                      (if (= direction :plus)
                                        (range (dec (month instant)), (+ (dec (month instant)) months))
                                        (range (dec (- (month instant) months)), 1)))]
           (if (nil? month)
             (* days util/ms-per-day)
             (recur (+ days (util/month->days year month))
                    (cond
                     (= days 0) year
                     (= month 1) (inc year)
                     :else year)
                    months)))
         0)
       (* 604800000 (or weeks 0))
       (* 86400000 (or days 0))
       (* 3600000 (or hours 0))
       (* 60000 (or minutes 0))
       (* 1000 (or seconds 0))
       (or milliseconds 0))))

(defrecord EzTime [milliseconds timezone
                   year month day hour minute second millisecond])

(defprotocol IntervalProtocol
  (overlap? [a b])
  (abut? [a b]))

(defrecord Interval [a b]
  IntervalProtocol
  (overlap? [a b]
    (let [[a1 a2] (sort < (map raw+ (map #(% a) [:a :b])))
          [b1 b2] (sort < (map raw+ (map #(% b) [:a :b])))]
      ;; at least one point has to be within the two points of the other
      ;; interval
      (or (and (> a1 b1)
               (< a1 b2))
          (and (> a2 b1)
               (< a2 b2))
          (and (> b1 a1)
               (< b1 a2))
          (and (> b2 a1)
               (< b2 a2)))))
  (abut? [a b]
    (let [[a1 a2] (sort < (map raw+ (map #(% a) [:a :b])))
          [b1 b2] (sort < (map raw+ (map #(% b) [:a :b])))]
      (and
       ;; the edges have to touch
       (or (= a1 b2)
           (= b1 a2))
       ;; and there can be no overlap
       (not (overlap? a b))))))

(defprotocol EzConvertProtocol
  (convert [instant] [instant to] [instant to tz]))

(extend-protocol EzConvertProtocol
  java.lang.Long
  (convert
    ([instant] (convert instant EzTime nil))
    ([instant to] (convert instant to nil))
    ([instant to tz]
       (case to
         :java.util.Date (java.util.Date. instant)
         java.util.Date (java.util.Date. instant)
         :java.sql.Timestamp (java.sql.Timestamp. instant)
         java.sql.Timestamp (java.sql.Timestamp. instant)
         :java.sql.Date (java.sql.Date. instant)
         java.sql.Date (java.sql.Date. instant)
         :long instant
         java.lang.Long instant
         :epoch (long (/ instant 1000))
         (map->EzTime (assoc (util/long-to-map instant)
                        :timezone tz)))))
  java.sql.Date
  (convert
    ([instant] (convert (.getTime instant) EzTime nil))
    ([instant to] (convert (.getTime instant) to nil))
    ([instant to tz] (convert (.getTime instant) to tz)))
  java.sql.Timestamp
  (convert
    ([instant] (convert (.getTime instant) EzTime nil))
    ([instant to] (convert (.getTime instant) to nil))
    ([instant to tz] (convert (.getTime instant) to tz)))
  java.util.Date
  (convert
    ([instant] (convert (.getTime instant) EzTime nil))
    ([instant to] (convert (.getTime instant) to nil))
    ([instant to tz] (convert (.getTime instant) to tz)))
  EzTime
  (convert
    ([instant] (convert instant EzTime nil))
    ([instant to] (convert instant to nil))
    ([instant to tz]
       (case to
         (if tz
           (assoc instant :timezone tz)
           instant)))))

(defprotocol EzFormatProtocol
  (format [instant] [instant to]))

(defprotocol EzParseProtocol
  (parse [instant fmt]))



(extend-protocol EzTimeProtocol
  EzTime
  (year [instant] (:year instant))
  (month [instant] (:month instant))
  (week [instant]
    (let [ordinal (+ (:day instant)
                     (if (util/leap? (:year instant))
                       1
                       0)
                     (get {1 0
                           2 31
                           3 59
                           4 90
                           5 120
                           6 151
                           7 181
                           8 212
                           9 243
                           10 273
                           11 304
                           12 334} (:month instant)))
          week (long (/ (+ 10 (- ordinal (weekday instant)))
                        7))]
      week))
  (weekday [instant]
    ;; 1970-01-01 is weekday 4, a Thursday
    (let [instant (raw+ instant)
          days-since-epoch (long (/ instant util/ms-per-day))]
      (if (< days-since-epoch -3)
        (inc (mod (+ 2 days-since-epoch) 7))
        (inc (mod (+ 3 days-since-epoch) 7)))))
  (day [instant] (:day instant))
  (hour [instant] (:hour instant))
  (minute [instant] (:minute instant))
  (second [instant] (:second instant))
  (millisecond [instant] (:millisecond instant))
  (raw [instant] (:milliseconds instant))
  (raw+ [instant] (+ (:milliseconds instant)
                     (get-in instant [:timezone :milliseconds] 0)))

  (after? [a b] (> (raw+ a) (raw+ b)))
  (before? [a b] (< (raw+ a) (raw+ b)))
  (equals? [a b] (= (raw+ a) (raw+ b)))
  (same-tz? [a b] (= (:timezone a) (:timezone b)))
  (within?
    ([instant interval]
       (within? instant (:a interval) (:b interval)))
    ([instant a b]
       (let [[a b] (sort #(< (raw+ %) (raw+ %2)) [a b])]
         (and (>= (raw+ instant)
                  (raw+ a))
              (<= (raw+ instant)
                  (raw+ b))))))
  (plus
    ([instant period]
       (convert (+ (raw instant)
                   (get-milliseconds period instant :plus)) EzTime (:tz instant)))
    ([instant period periodic?]
       (if periodic?
         (cons (plus instant period)
               (lazy-seq (plus (plus instant period) period true)))
         (plus instant period))))
  (minus
    ([instant period]
       (convert (- (raw instant)
                   (get-milliseconds period instant :minus)) EzTime (:tz instant)))

    ([instant period periodic?]
       (if periodic?
         (cons (minus instant period)
               (lazy-seq (minus (minus instant period) period true)))
         (minus instant period))))
  (leap? [instant] (util/leap? (:year instant))))

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
                 ;; days are 1-indexed, but
                 ;; we calculate as 0-indexed
                 (* (dec day) 24 3600 1000)
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
                 ;; days are 1-indexed, but
                 ;; we calculate as 0-indexed
                 (* (dec day) 24 3600 1000)
                 (* hour 3600 1000)
                 (* minute 60 1000)
                 (* second 1000)
                 millisecond) nil
                 year month day hour minute second millisecond)))

(defn period
  "data -> map with keys 'years months weeks days hours minutes seconds milliseconds'"
  [data]
  (map->Period data))

(defn interval [a b]
  (map->Interval {:a a :b b}))


(defn now
  ([]
     (now false))
  ([tz?]
     (if tz?
       (convert (System/currentTimeMillis)
                EzTime
                (tz/timezone (.. TimeZone getDefault getID)))
       (convert (+ (System/currentTimeMillis)
                   (:milliseconds (tz/timezone (.. TimeZone getDefault getID))))))))
