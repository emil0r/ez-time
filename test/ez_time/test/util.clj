(ns ez-time.test.util
  (:require [ez-time.test.util :refer :all]
            [midje.sweet :refer :all]
            [ez-time.util :as util]))


(fact
 "leap-days from 1970"
 ;; leap years => 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008
 ;; a total of 11
 (fact
  "2014 AD"
  (util/leap-days 2014) => 11)
 (fact
  "1 AD"
  (util/leap-days 1) => 477))

(fact
 "get-days"
 {:1970 (util/get-days 1970)
  :1971 (util/get-days 1971)
  :1972 (util/get-days 1972)
  :1973 (util/get-days 1973)
  :1974 (util/get-days 1974)
  :1975 (util/get-days 1975)
  :1980 (util/get-days 1980)
  :2000 (util/get-days 2000)
  :2014 (util/get-days 2014)}
  => {:1970 0
      :1971 365
      :1972 (+ 365 366)
      :1973 (+ 365 366 365)
      :1974 (+ 365 366 365 365)
      :1975 (+ 365 366 365 365 365)
      :1980 (+ (* 7 365) (* 3 366))
      :2000 (+ (* 22 365) (* 8 366))
      :2014 (+ (* 33 365) (* 11 366))})

(fact
 "ms->year"
 (let [date #inst "2014-11-17T18:22:01.247-00:00"]
   (util/ms->year (.getTime date)) => 2014))

(fact
 "month->ms"
 (let [dt-ms (.getTime #inst "2014-01-01")
       dt-1 (.getTime #inst "2014-01-01")
       dt-2 (.getTime #inst "2014-02-01")
       dt-3 (.getTime #inst "2014-03-01")
       dt-4 (.getTime #inst "2014-04-01")
       dt-5 (.getTime #inst "2014-05-01")
       dt-6 (.getTime #inst "2014-06-01")
       dt-7 (.getTime #inst "2014-07-01")
       dt-8 (.getTime #inst "2014-08-01")
       dt-9 (.getTime #inst "2014-09-01")
       dt-10 (.getTime #inst "2014-10-01")
       dt-11 (.getTime #inst "2014-11-01")
       dt-12 (.getTime #inst "2014-12-01")]
   (fact "january"
         (+ dt-ms (util/month->ms 2014 1)) => dt-1)
   (fact "february"
         (+ dt-ms (util/month->ms 2014 2)) => dt-2)
   (fact "march"
         (+ dt-ms (util/month->ms 2014 3)) => dt-3)
   (fact "april"
         (+ dt-ms (util/month->ms 2014 4)) => dt-4)
   (fact "may"
         (+ dt-ms (util/month->ms 2014 5)) => dt-5)
   (fact "june"
         (+ dt-ms (util/month->ms 2014 6)) => dt-6)
   (fact "july"
         (+ dt-ms (util/month->ms 2014 7)) => dt-7)
   (fact "august"
         (+ dt-ms (util/month->ms 2014 8)) => dt-8)
   (fact "september"
         (+ dt-ms (util/month->ms 2014 9)) => dt-9)
   (fact "october"
         (+ dt-ms (util/month->ms 2014 10)) => dt-10)
   (fact "november"
         (+ dt-ms (util/month->ms 2014 11)) => dt-11)
   (fact "december"
         (+ dt-ms (util/month->ms 2014 12)) => dt-12)))

(fact
 "year->ms"
 (util/year->ms 2014) => (.getTime #inst "2014-01-01"))

(fact
 "ms->month"
 (fact
  "2014 1 jan"
  (let [date #inst "2014-01-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 1))
 (fact
  "2014 1 feb"
  (let [date #inst "2014-02-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 2))
 (fact
  "2014 1 mar"
  (let [date #inst "2014-03-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 3))
 (fact
  "2014 1 apr"
  (let [date #inst "2014-04-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 4))
 (fact
  "2014 1 may"
  (let [date #inst "2014-05-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 5))
 (fact
  "2014 1 jun"
  (let [date #inst "2014-06-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 6))
 (fact
  "2014 1 jul"
  (let [date #inst "2014-07-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 7))
 (fact
  "2014 1 aug"
  (let [date #inst "2014-08-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 8))
 (fact
  "2014 1 sep"
  (let [date #inst "2014-09-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 9))
 (fact
  "2014 1 oct"
  (let [date #inst "2014-10-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 10))
 (fact
  "2014 1 nov"
  (let [date #inst "2014-11-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 11))
 (fact
  "2014 1 dec"
  (let [date #inst "2014-12-01T00:00:00.000-00:00"]
    (util/ms->month (.getTime date) 2014) => 12))

 (fact
  "1970 30 nov"
  (let [date #inst "1970-11-30T23:59:59.999-00:00"]
    (util/ms->month (.getTime date) 1970) => 11))
 (fact
  "1970 1 nov"
  (let [date #inst "1970-11-01T23:59:59.999-00:00"]
    (util/ms->month (.getTime date) 1970) => 11)))
