(ns tech.jna.timbre-log
  (:require [taoensso.timbre :as log]))



(defn log-info
  [log-str]
  (log/info log-str))


(defn log-warn
  [log-str]
  (log/warn log-str))


(defn log-error
  [log-str]
  (log/error log-str))
