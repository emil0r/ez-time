(ns ez-time.test.time
  (:require [midje.sweet :refer :all]
            [ez-time.time :as time]))




(fact
 "datetime"
 (let [dt (time/datetime 2014 2 3 4 5 6 700)]
   (map #(% dt) [time/year time/month time/day time/hour
                 time/minute time/second time/millisecond]))
 => [2014 2 3 4 5 6 700])

(fact
 "datetime edge (after 2014)"
 (let [dt (time/datetime 2014 1 1)]
   (map #(% dt) [time/year time/month time/day]))
 => [2014 1 1])

(fact
 "datetime edge (before 2014)"
 (let [dt (time/datetime 2013 12 31 23 59 59 999)]
   (map #(% dt) [time/year time/month time/day
                 time/hour time/minute time/second time/millisecond]))
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
 "added three days"
 (let [period (time/period {:days 3})
       dt (time/datetime 2014 1 1)]
   (time/day (time/plus dt period)) => 4))
