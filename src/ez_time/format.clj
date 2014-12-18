(ns ez-time.format
  (:require [clojure.string :as str]
            [ez-time.time :as time :refer [EzParseProtocol]]
            [ez-time.util :as util])
  (:import [ez_time FormatException]))

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

(extend-protocol EzParseProtocol
  java.lang.String
  (parse [instant pattern]
    (if-not (= (count instant) (count pattern))
      (throw (ex-info "instant and pattern are not equal length"
                      {:instant instant :pattern pattern}))
      (let [pattern (parse-pattern pattern)]
        (loop [[[char num] & pattern] pattern
               data {}
               instant instant]
          (if (nil? char)
            (let [year (cond
                        (get data [\Y 4])
                        (try (Long/parseLong (get data [\Y 4]))
                             (catch Exception e
                               (throw (FormatException. (str "Tried parsing YYYY, got " (get data [\Y 4]))))))
                        :else 1970)
                  month (cond
                         (get data [\M 2])
                         (try (Long/parseLong (get data [\M 2]))
                              (catch Exception e
                                (throw (FormatException. (str "Tried parsing MM, got " (get data [\M 2]))))))
                         :else 1)
                  day (cond
                       (get data [\D 2])
                       (try (Long/parseLong (get data [\D 2]))
                            (catch Exception e
                              (throw (FormatException. (str "Tried parsing DD, got " (get data [\D 2]))))))
                       :else 1)]
              (time/convert (+ (util/year->ms year)
                               (util/month->ms year month)
                               (* util/ms-per-day (dec day)))))
            (recur pattern
                   (conj data {[char num] (apply str (take num instant))})
                   (apply str (drop num instant)))))))))
