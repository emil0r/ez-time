(ns ez-time.util)


(defn leap? [year]
  (and (= (mod year 4) 0) ;; dividable by 4
       (or (not= (mod year 100) 0) ;; not a leap year if divideable by 100
           (= (mod year 400) 0)) ;; unless it's also divideable by 400
       ))

(def ms-per-minute (* 60 1000))
(def ms-per-hour (* 60 ms-per-minute))
(def ms-per-day (* 24 ms-per-hour))
(def ms-per-year (* 365 ms-per-day))
(def ms-per-leap-year (* 366 ms-per-day))



(defn leap-days
  "Number of leap days with 1970 as the origo or number of leap days
between instant and number of years"
  ([year]
     (let [year (Math/abs year)]
       (Math/abs
        (- (+ (long (/ year 4)) (long (/ year 400)))
           (long (/ year 100))
           477 ;; number of leap-days from 0 AD
           ))))
  ([year num-years]
     (count (filter leap?
                    (range (inc year)
                           (inc (+ year num-years)))))))

(defn get-days
  "Get days since origo"
  [year]
  (+ (Math/abs (* 365 (- year 1970)))
     (leap-days year)))


(defn year->ms
  "Get back the year in number of milliseconds from or before 1970"
  [year]
  ;; have 1970 as the origo because of epoch time
  (if (>= year 1970)
    (* (get-days year) ms-per-day)
    (* (get-days year) -1 ms-per-day)))

(defn month->days [year month]
  (if (leap? year)
    (case month
      12 31
      11 30
      10 31
      9 30
      8 31
      7 31
      6 30
      5 31
      4 30
      3 31
      2 29
      31)
    (case month
      12 31
      11 30
      10 31
      9 30
      8 31
      7 31
      6 30
      5 31
      4 30
      3 31
      2 28
      31)))

(defn month->ms
  "Get how many milliseconds is needed in order to start at the beginning of the month"
  [year month]
  (if (leap? year)
    (case month
      12 28944000000
      11 26352000000
      10 23673600000
      9 21081600000
      8 18403200000
      7 15724800000
      6 13132800000
      5 10454400000
      4 7862400000
      3 5184000000
      2 2678400000
      0)
    (case month
      12 28857600000
      11 26265600000
      10 23587200000
      9 20995200000
      8 18316800000
      7 15638400000
      6 13046400000
      5 10368000000
      4 7776000000
      3 5097600000
      2 2678400000
      0)))

(defn ms->year [ms]
  (loop [year (+ 1970 (long (/ ms ms-per-year)))]
    (let [year-ms (year->ms year)
          diff (- ms year-ms)]
      (cond
       (< diff 0) (recur (- year 1))
       (> diff ms-per-leap-year) (recur (+ year 1))
       (> diff ms-per-year) (recur (+ year 1))
       :else year))))

(defn ms->month
  ([ms]
     (ms->month ms (ms->year ms)))
  ([ms year]
     (let [ms (- ms (* (get-days year) ms-per-day))]
       (if (leap? year)
         (cond
          (>= ms 28944000000) 12
          (>= ms 26352000000) 11
          (>= ms 23673600000) 10
          (>= ms 21081600000) 9
          (>= ms 18403200000) 8
          (>= ms 15724800000) 7
          (>= ms 13132800000) 6
          (>= ms 10454400000) 5
          (>= ms 7862400000) 4
          (>= ms 5184000000) 3
          (>= ms 2678400000) 2
          :else 1)
         (cond
          (>= ms 28857600000) 12
          (>= ms 26265600000) 11
          (>= ms 23587200000) 10
          (>= ms 20995200000) 9
          (>= ms 18316800000) 8
          (>= ms 15638400000) 7
          (>= ms 13046400000) 6
          (>= ms 10368000000) 5
          (>= ms 7776000000) 4
          (>= ms 5097600000) 3
          (>= ms 2678400000) 2
          :else 1)))))

(defn long-to-map [instant]
  (let [year (ms->year instant)
        year-ms (year->ms year)
        month (ms->month instant)
        month-ms (month->ms year month)
        day (long (/ (- instant year-ms month-ms)
                     ms-per-day))
        day-ms (* day ms-per-day)
        hour (long (/ (- instant year-ms month-ms day-ms)
                      ms-per-hour))
        hour-ms (* hour ms-per-hour)
        minute (long (/ (- instant year-ms month-ms day-ms
                           hour-ms)
                        ms-per-minute))
        minute-ms (* minute ms-per-minute)
        second (long (/ (- instant year-ms month-ms day-ms
                           hour-ms minute-ms)
                        1000))
        second-ms (* second 1000)
        millisecond (- instant year-ms month-ms day-ms
                       hour-ms minute-ms second-ms)]
    {
     ;; year, month and day are 1-indexed
     :year year
     :month month
     :day (+ day 1) ;; go from 0-indexed to 1-indexed

     ;; hour, minute, second and millisecond are 0-index
     :hour hour
     :minute minute
     :second second
     :millisecond millisecond
     :milliseconds instant}))
