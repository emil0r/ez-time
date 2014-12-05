(ns ez-time.formatter
  (:require [clojure.string :as str]))

(defrecord Format [era ;; G
                   century ;; C
                   weekyear ;; x
                   year ;; y
                   year-of-era ;; Y
                   month ;; M
                   day ;; D
                   week-of-weekyear ;; w
                   timezone ;; z
                   timezone-offset ;; Z
                   literal-text ;; '
                   ])

(defn parse-pattern [pattern]
  (loop [[char & chars] pattern
         token [char 1]
         out []]
    (if (nil? char)
      out
      (let [changing? (not (= (first token) (first chars)))]
        (recur chars
               (if changing?
                 [(first chars) 1]
                 [char (inc (second token))])
               (if changing?
                 (conj out token)
                 out))))))
