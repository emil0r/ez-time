(ns ez-time.FormatException
  (:gen-class
   :extends java.lang.Exception
   :implements [clojure.lang.IDeref]
   :init init
   :state state
   :constructors {[String] [String]
                  [String String String] [String]}))

(defn -init
  ([message]
     [[message] [message]])
  ([message given expected]
     [[message] {:message message :given given :expected expected}]))


(defn -deref [this]
  (.state this))
