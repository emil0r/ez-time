(ns ez-time.test.time
  (:require [midje.sweet :refer :all]
            [ez-time.time :as time]))



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
