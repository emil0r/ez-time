(ns ez-time.timezone
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn- get-timezone [data tz offset?]
  ;; get timezone if it exists in data
  ;; if it does not exist but tz is set and offset? is not nil
  ;; return the tz
  ;; otherwise return nil
  (let [new-tz (->> data (re-seq #"^Zone\s+([^\s]+)") last last)]
    (cond
     new-tz new-tz
     (and tz (not (nil? offset?))) tz
     :else nil)))

(defn- get-timezone-offset
  [data]
  (let [offset (->> data
                    (re-seq #"\s-?\d{1,2}(:\d{2,2}){0,2}")
                    ffirst)]
    (if offset
        (let [[hour minute second] (map #(Long/parseLong %) (str/split (str/trim offset) #":"))]
         {:hour hour
          :minute (or minute 0)
          :second (or second 0)}))))

(defn- read-timezones []
  (let [filenames ["africa" "northamerica" "southamerica" "europe" "asia"
                   "australasia" "antarctica"]]
    (merge {"UTC" {:hour 0 :minute 0 :second 0}
            "UTC+00:00" {:hour 0 :minute 0 :second 0}
            "UTC+01:00" {:hour 1 :minute 0 :second 0}
            "UTC+02:00" {:hour 2 :minute 0 :second 0}
            "UTC+03:00" {:hour 3 :minute 0 :second 0}
            "UTC+03:30" {:hour 3 :minute 30 :second 0}
            "UTC+04:00" {:hour 4 :minute 0 :second 0}
            "UTC+04:30" {:hour 4 :minute 30 :second 0}
            "UTC+05:00" {:hour 5 :minute 0 :second 0}
            "UTC+05:30" {:hour 5 :minute 30 :second 0}
            "UTC+05:45" {:hour 5 :minute 45 :second 0}
            "UTC+06:00" {:hour 6 :minute 0 :second 0}
            "UTC+06:30" {:hour 6 :minute 30 :second 0}
            "UTC+07:00" {:hour 7 :minute 0 :second 0}
            "UTC+08:00" {:hour 8 :minute 0 :second 0}
            "UTC+08:45" {:hour 8 :minute 45 :second 0}
            "UTC+09:00" {:hour 9 :minute 0 :second 0}
            "UTC+09:30" {:hour 9 :minute 30 :second 0}
            "UTC+10:00" {:hour 10 :minute 0 :second 0}
            "UTC+10:30" {:hour 10 :minute 30 :second 0}
            "UTC+11:00" {:hour 11 :minute 0 :second 0}
            "UTC+11:30" {:hour 11 :minute 30 :second 0}
            "UTC+12:00" {:hour 12 :minute 0 :second 0}
            "UTC+12:45" {:hour 12 :minute 45 :second 0}
            "UTC+13:00" {:hour 13 :minute 0 :second 0}
            "UTC+14:00" {:hour 14 :minute 0 :second 0}
            "UTC-01:00" {:hour -1 :minute 0 :second 0}
            "UTC-02:00" {:hour -2 :minute 0 :second 0}
            "UTC-03:00" {:hour -3 :minute 0 :second 0}
            "UTC-03:30" {:hour -3 :minute 30 :second 0}
            "UTC-04:00" {:hour -4 :minute 0 :second 0}
            "UTC-04:30" {:hour -4 :minute 30 :second 0}
            "UTC-05:00" {:hour -5 :minute 0 :second 0}
            "UTC-06:00" {:hour -6 :minute 0 :second 0}
            "UTC-07:00" {:hour -7 :minute 0 :second 0}
            "UTC-08:00" {:hour -8 :minute 0 :second 0}
            "UTC-09:00" {:hour -9 :minute 0 :second 0}
            "UTC-09:30" {:hour -9 :minute 30 :second 0}
            "UTC-10:00" {:hour -10 :minute 0 :second 0}
            "UTC-11:00" {:hour -11 :minute 0 :second 0}
            "UTC-12:00" {:hour -12 :minute 0 :second 0}}
     (into {}
           (map (fn [filename]
                  (let [data (io/reader (io/resource (str/join "/" ["tzdata" filename])))]
                    (loop [out {}
                           [line & lines] (remove #(re-find #"^\#" %) (line-seq data))
                           tz nil]
                      (if (nil? line)
                        out
                        (let [offset (get-timezone-offset line)
                              name (get-timezone line tz offset)]
                          (if (nil? name)
                            (recur out lines name)
                            (recur (assoc out name offset) lines name))))))) filenames)))))


(def ^:dynamic *timezones* (read-timezones))

(defrecord TimeZone [name hour minute second])

(defmulti timezone (fn [x & args] (type x)))
(defmethod timezone String [id]
  (if-let [tz (get *timezones* id)]
    (TimeZone. id (:hour tz) (:minute tz) (:second tz))))
(defmethod timezone :default
  ([hours]
     {:pre [(> hours -23) (< hours 23)]}
     (TimeZone. nil hours 0 0))
  ([hours minutes]
     {:pre [(> hours -23) (< hours 23)
            (> minutes -59) (< minutes 59)]}
     (TimeZone. nil hours minutes 0)))



(def utc (timezone "UTC"))

(defn default []
  (timezone (System/getProperty "user.timezone")))
