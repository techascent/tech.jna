(ns tech.jna.timbre-log
  (:require [taoensso.timbre :as log]))



(defn log-info
  [log-str]
  (log/info log-str))
