(ns ez-time.util)


(defn leap? [year]
  (and (= (mod year 4) 0) ;; dividable by 4
       (or (not= (mod year 100) 0) ;; not a leap year if divideable by 100
           (= (mod year 400) 0)) ;; unless it's also divideable by 400
       ))

(def ms-per-day (* 24 60 60 1000))
(def ms-per-year (* 365 24 60 60 1000))
(def ms-per-leap-year (* 366 24 60 60 1000))



(defn leap-days
  "Number of leap days with 1970 as the origo"
  [year]
  (let [year (Math/abs year)]
    (Math/abs
     (- (+ (long (/ year 4)) (long (/ year 400)))
        (long (/ year 100))
        477))))

(defn get-days [year]
  (+ (Math/abs (* 365 (- year 1970)))
     (leap-days year)))


(defn year->ms
  "Get back the year in number of milliseconds from or before 1970"
  [year]
  ;; have 1970 as the origo because of epoch time
  (if (>= year 1970)
    (* (get-days year) ms-per-day)
    (* (get-days year) -1 ms-per-day)))

(defn month->ms [year month]
  (if (leap? year)
    (case month
      12 31622400000
      11 28944000000
      10 26352000000
      9 23673600000
      8 21081600000
      7 18403200000
      6 15724800000
      5 13132800000
      4 10454400000
      3 7862400000
      2 5184000000
      2678400000)
    (case month
      12 31536000000
      11 28857600000
      10 26265600000
      9 23587200000
      8 20995200000
      7 18316800000
      6 15638400000
      5 13046400000
      4 10368000000
      3 7776000000
      2 5097600000
      2678400000)))

(defn ms->year [ms]
  (loop [year (+ 1970 (long (/ ms ms-per-year)))]
    (let [year-ms (year->ms year)
          diff (- ms year-ms)]
      (cond
       (< diff 0) (recur (- year 1))
       (> diff ms-per-leap-year) (recur (+ year 1))
       (> diff ms-per-year) (recur (+ year 1))
       :else year))))

(defn ms->month [ms]
  (let [year (ms->year ms)
        ms (- ms (* (get-days year) ms-per-day))]
    (if (leap? year)
      (cond
       (>= ms 31622400000) 12
       (>= ms 28944000000) 11
       (>= ms 26352000000) 10
       (>= ms 23673600000) 9
       (>= ms 21081600000) 8
       (>= ms 18403200000) 7
       (>= ms 15724800000) 6
       (>= ms 13132800000) 5
       (>= ms 10454400000) 4
       (>= ms 7862400000) 3
       (>= ms 5184000000) 2
       :else 1)
      (cond
       (>= ms 31536000000) 12
       (>= ms 28857600000) 11
       (>= ms 26265600000) 10
       (>= ms 23587200000) 9
       (>= ms 20995200000) 8
       (>= ms 18316800000) 7
       (>= ms 15638400000) 6
       (>= ms 13046400000) 5
       (>= ms 10368000000) 4
       (>= ms 7776000000) 3
       (>= ms 5097600000) 2
       :else 1))))
