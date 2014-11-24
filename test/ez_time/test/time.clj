(ns ez-time.test.time
  (:require [midje.sweet :refer :all]
            [ez-time.time :as time]
            [ez-time.timezone :as tz]))



(defn- date-components [date]
  (map #(% date) [time/year time/month time/day time/hour
                  time/minute time/second time/millisecond]))


(fact
 "datetime"
 (let [dt (time/datetime 2014 2 3 4 5 6 700)]
   (date-components dt))
 => [2014 2 3 4 5 6 700])

(fact
 "datetime edge (after 2014)"
 (let [dt (time/datetime 2014 1 1)]
   (map #(% dt) [time/year time/month time/day]))
 => [2014 1 1])

(fact
 "datetime edge (before 2014)"
 (let [dt (time/datetime 2013 12 31 23 59 59 999)]
   (date-components dt))
 => [2013 12 31 23 59 59 999])


(fact
 "a comes before b"
 (let [a (time/datetime 2013 12 31 23)
       b (time/datetime 2014 1 1 1)]
   (time/before? a b)) => true)


(fact
 "a comes after b"
 (let [a (time/datetime 2015 12 31 23)
       b (time/datetime 2014 1 1 1)]
   (time/after? a b)) => true)



(fact
 "Creating date from long"
 (let [date #inst "2014-11-17T18:22:01.247-00:00"
       ez-date (time/convert (.getTime date))]
   (fact "year"
         (time/year ez-date) => 2014)
   (fact "month"
         (time/month ez-date) => 11)
   (fact "week"
         (time/week ez-date) => 47)
   (fact "day"
         (time/day ez-date) => 17)
   (fact "hour"
         (time/hour ez-date) => 18)
   (fact "minute"
         (time/minute ez-date) => 22)
   (fact "second"
         (time/second ez-date) => 1)
   (fact "millisecond"
         (time/millisecond ez-date) => 247)))

(fact
 "weekday"
 (fact "2012, 21 oct, wednesday"
       (time/weekday (time/datetime 2012 11 21)) => 4)
 (fact "1780, 7 feb, monday"
       (time/weekday (time/datetime 1780 2 7)) => 1)
 (fact "1780, 8 feb, tuesday"
       (time/weekday (time/datetime 1780 2 8)) => 2)
 (fact "1780, 9 feb, wednesday"
       (time/weekday (time/datetime 1780 2 9)) => 3)
 (fact "1780, 10 feb, thursday"
       (time/weekday (time/datetime 1780 2 10)) => 4)
 (fact "1780, 11 feb, friday"
       (time/weekday (time/datetime 1780 2 11)) => 5)
 (fact "1780, 12 feb, saturday"
       (time/weekday (time/datetime 1780 2 12)) => 6)
 (fact "2050, 13 mar, sunday"
       (time/weekday (time/datetime 2050 3 13)) => 7)
 (fact "monday"
       (time/weekday (time/datetime 2014 11 24)) => 1)
 (fact "tuesday"
       (time/weekday (time/datetime 2014 11 25)) => 2)
 (fact "wednesday"
       (time/weekday (time/datetime 2014 11 26)) => 3)
 (fact "thursday"
       (time/weekday (time/datetime 2014 11 27)) => 4)
 (fact "friday"
       (time/weekday (time/datetime 2014 11 28)) => 5)
 (fact "saturday"
       (time/weekday (time/datetime 2014 11 29)) => 6)
 (fact "sunday"
       (time/weekday (time/datetime 2014 11 30)) => 7))



(fact
 "get-milliseconds for period"
 (fact "year"
       (time/get-milliseconds
        (time/period {:years 6})
        (time/datetime 2011)
        :plus)
       ;; 2 leap years and 4 common years
       => (+ (* 2 366 24 3600 1000)
             (* 4 365 24 3600 1000)))
 (fact "month"
       (time/get-milliseconds
        (time/period {:months 5})
        (time/datetime 2011 1 4)
        :plus)
       => (+
           ;; remainder of january, 4th of january
           ;; is included
           (* 28 24 3600 1000)

           (* 28 24 3600 1000) ;; february
           (* 31 24 3600 1000) ;; march
           (* 30 24 3600 1000) ;; april
           (* 31 24 3600 1000) ;; may
           ;; remainder of june (we're supposed to
           ;; land on the 4th of june)
           (* 3 24 3600 1000)))
 (fact "week"
       (time/get-milliseconds
        (time/period {:weeks 6})
        (time/now)
        :plus)
       => (* 6 7 24 3600 1000))
 (fact "day"
       (time/get-milliseconds
        (time/period {:days 1})
        (time/now)
        :plus)
       => (* 24 3600 1000))
 (fact "hour"
       (time/get-milliseconds
        (time/period {:hours 2})
        (time/now)
        :plus)
       => (* 2 3600 1000))
 (fact "minute"
       (time/get-milliseconds
        (time/period {:minutes 699})
        (time/now)
        :plus)
       => (* 699 60 1000))
 (fact "second"
       (time/get-milliseconds
        (time/period {:seconds 2})
        (time/now)
        :plus)
       => (* 2 1000)))


(fact
 "java.util.Date and EzTime gives the same number of milliseconds"
 (.getTime #inst "2014-02-03T04:05:06.789")
 => (time/raw (time/datetime 2014 2 3 4 5 6 789)))

(fact
 "addition"
 (fact "3 days"
       (let [period (time/period {:days 3})
             dt (time/datetime 2014 1 1)]
         (time/day (time/plus dt period)))
       => 4)
 (fact "40 days"
       (let [period (time/period {:days 40})
             dt (time/datetime 2014 1 1)]
         (time/day (time/plus dt period)))
       => 10)
 (fact "40 months"
       (let [period (time/period {:months 40})
             dt (time/datetime 2011 2 3 4 5 6 789)]
         (date-components (time/plus dt period)))
       => [2014 6 3 4 5 6 789])
 (fact "4 hours"
       (let [period (time/period {:hours 4})
             dt (time/datetime 2014 1 1)]
         (time/hour (time/plus dt period)))
       => 4)
 (fact "59 minutes"
       (let [period (time/period {:minutes 59})
             dt (time/datetime 2014 1 1)]
         (time/minute (time/plus dt period)))
       => 59)
 (fact "59 seconds"
       (let [period (time/period {:seconds 59})
             dt (time/datetime 2014 1 1)]
         (time/second (time/plus dt period)))
       => 59)
 (fact "go past a year"
       (let [period (time/period {:months 11 :days 30 :weeks 1})
             dt (time/datetime 2013 1 1)]
         (time/year (time/plus dt period)))
       => 2014))

(fact
 "subtraction"
 (fact "3 days"
       (let [period (time/period {:days 3})
             dt (time/datetime 2014 1 1)]
         (time/day (time/minus dt period)))
       => 29)
 (fact "40 days"
       (let [period (time/period {:days 40})
             dt (time/datetime 2014 1 1)]
         (time/day (time/minus dt period)))
       => 22)
 (fact "40 months"
       (let [period (time/period {:months 40})
             dt (time/datetime 2011 2 3 4 5 6 789)]
         (date-components (time/minus dt period)))
       => [2007 10 3 4 5 6 789])
 (fact "4 hours"
       (let [period (time/period {:hours 4})
             dt (time/datetime 2014 1 1)]
         (time/hour (time/plus dt period)))
       => 4)
 (fact "59 minutes"
       (let [period (time/period {:minutes 59})
             dt (time/datetime 2014 1 1)]
         (time/minute (time/plus dt period)))
       => 59)
 (fact "59 seconds"
       (let [period (time/period {:seconds 59})
             dt (time/datetime 2014 1 1)]
         (time/second (time/plus dt period)))
       => 59)
 (fact "go past a year"
       (let [period (time/period {:months 11 :days 30 :weeks 1})
             dt (time/datetime 2013 1 1)]
         (time/year (time/plus dt period)))
       => 2014))


(fact
 "leap year"
 (fact "trip over into 2012-02-29 00:00:00 by 1 second"
       (let [period (time/period {:seconds 1})
             dt (time/datetime 2012 2 28 23 59 59)
             added (time/plus dt period)]
         (map #(% added) [time/year time/month time/day time/hour
                          time/minute time/second time/millisecond]))
       => [2012 2 29 0 0 0 0])
 (fact "trip over into 2012-02-29 00:00:00 by 1 minute"
       (let [period (time/period {:minutes 1})
             dt (time/datetime 2012 2 28 23 59 59)
             added (time/plus dt period)]
         (map #(% added) [time/year time/month time/day time/hour
                          time/minute time/second time/millisecond]))
       => [2012 2 29 0 0 59 0])
 (fact "trip back into 2012-02-29 23:59:59 by 1 second"
       (let [period (time/period {:seconds 1})
             dt (time/datetime 2012 3 1 0 0 0)
             subtracted (time/minus dt period)]
         (map #(% subtracted) [time/year time/month time/day time/hour
                          time/minute time/second time/millisecond]))
       => [2012 2 29 23 59 59 0])

 (fact "trip back into 2012-02-29 00:00:00 by 1 day"
       (let [period (time/period {:days 1})
             dt (time/datetime 2012 3 1 0 0 0)
             subtracted (time/minus dt period)]
         (map #(% subtracted) [time/year time/month time/day time/hour
                          time/minute time/second time/millisecond]))
       => [2012 2 29 0 0 0 0])

 (fact "go past 29th of feb to 2012-04-1 00:00:00"
       (let [period (time/period {:days (+ 29 31)})
             dt (time/datetime 2012 2 1 0 0 0)
             added (time/plus dt period)]
         (map #(% added) [time/year time/month time/day time/hour
                          time/minute time/second time/millisecond]))
       => [2012 4 1 0 0 0 0])
 (fact "from 2014 to 2016"
       (let [period (time/period {:years 2})
             dt (time/datetime 2014)
             added (time/plus dt period)]
         (map #(% added) [time/year time/month time/day time/hour
                          time/minute time/second time/millisecond]))
       => [2016 1 1 0 0 0 0])
 (fact "from 2014 to 2020"
       (let [period (time/period {:years 6})
             dt (time/datetime 2014)
             added (time/plus dt period)]
         (map #(% added) [time/year time/month time/day time/hour
                          time/minute time/second time/millisecond]))
       => [2020 1 1 0 0 0 0]))

(fact "equals?"
      (let [a (time/datetime 2014 5 4 23 4 56 (tz/timezone "UTC+01:00"))
            b (time/datetime 2014 5 4 23 4 56 (tz/timezone "UTC+01:00"))]
        (time/equals? a b))
      => true)

(fact "same-tz?"
      (let [a (time/datetime 2014 5 4 23 4 56 (tz/timezone "UTC+01:00"))
            b (time/datetime 2014 5 4 23 4 56 (tz/timezone "UTC+02:00"))]
        (time/same-tz? a b))
      => false)


(fact
 "periodic"
 (fact "plus"
       (let [dates (take 3 (time/plus
                            (time/datetime 2014)
                            (time/period {:days 1})
                            true))]
         (map time/day dates))
       => [2 3 4])
 (fact "minus"
       (let [dates (take 3 (time/minus
                            (time/datetime 2014)
                            (time/period {:days 1})
                            true))]
         (map time/day dates))
       => [31 30 29]))

(fact
 "interval"
 (fact "within?"
       (let [instant (time/datetime 2014 1 2)
             interval (time/interval
                       (time/datetime 2014 1 3)
                       (time/datetime 2014 1 1))]
         (time/within? instant interval))
       => true)
 (fact "overlap?"
       (fact "partly"
             (let [a (time/interval
                      (time/datetime 2014 1 3)
                      (time/datetime 2014 1 1))
                   b (time/interval
                      (time/datetime 2014 1 2)
                      (time/datetime 2014 1 3))]
               (time/overlap? a b))
             => true)
       (fact "completely"
             (let [a (time/interval
                      (time/datetime 2014 1 4)
                      (time/datetime 2014 1 1))
                   b (time/interval
                      (time/datetime 2014 1 2)
                      (time/datetime 2014 1 3))]
               (time/overlap? a b))
             => true)
       (fact "no overlap"
             (let [a (time/interval
                      (time/datetime 2014 1 4)
                      (time/datetime 2014 1 1))
                   b (time/interval
                      (time/datetime 2014 1 4)
                      (time/datetime 2014 1 5))]
               (time/overlap? a b))
             => false))
 (fact "abut?"
       (fact "true"
             (let [a (time/interval
                      (time/datetime 2014 1 3)
                      (time/datetime 2014 1 1))
                   b (time/interval
                      (time/datetime 2014 1 3)
                      (time/datetime 2014 1 4))]
               (time/abut? a b))
             => true)
       (fact "false, actually overlaps"
             (let [a (time/interval
                      (time/datetime 2014 1 3)
                      (time/datetime 2014 1 1))
                   b (time/interval
                      (time/datetime 2014 1 2)
                      (time/datetime 2014 1 4))]
               (time/abut? a b))
             => false)
       (fact "false, completeley separate"
             (let [a (time/interval
                      (time/datetime 2014 1 3)
                      (time/datetime 2014 1 1))
                   b (time/interval
                      (time/datetime 2014 1 4)
                      (time/datetime 2014 1 5))]
               (time/abut? a b))
             => false)))
