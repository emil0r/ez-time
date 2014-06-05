(ns ez-time.util)


(defn leap? [year]
  (and (= (mod year 4) 0) ;; dividable by 4
       (or (not (= (mod year 100) 0)) ;; not a leap year if divideable by 100
           (= (mod year 400) 0)) ;; unless it's also divideable by 400
       ))
