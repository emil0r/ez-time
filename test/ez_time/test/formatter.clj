(ns ez-time.test.formatter
  (:require [ez-time.formatter :as fmt]
            [midje.sweet :refer :all]))


(fact
 "parse-pattern"
 (fact
  "YYYY-MM-DD HH:mm:ss"
  (fmt/parse-pattern "YYYY-MM-DD HH:mm:ss")
  => [[\Y 4] [\- 1] [\M 2] [\- 1] [\D 2] [\space 1] [\H 2] [\: 1] [\m 2] [\: 1] [\s 2]]))
