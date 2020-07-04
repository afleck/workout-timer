(ns workout-timer.prod
  (:require
    [workout-timer.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
