(ns schedule-test
  #+clj (:require [clojure.test :as t :refer (is deftest testing are)]
                  [schedule :as s :refer (anchor)])
  #+cljs (:require-macros [cemerick.cljs.test
                           :refer (is deftest testing are)])
  #+cljs (:require [cemerick.cljs.test :as t]
                   [schedule :as s :refer (anchor)]
                   [cljs.reader :refer (read-string)])
  (:import schedule.WeeklyPattern
           schedule.WeeklySchedule))

(deftest test-pattern-reading
  (testing "types"
    (are [x] (= (type (read-string x)) WeeklyPattern)
         "#schedule/weekly-pattern \"Every day\""
         "#schedule/weekly-pattern \"Every day at 9:00\""
         "#schedule/weekly-pattern \"Every day PST\""
         "#schedule/weekly-pattern \"Every day at 9:00 PST\"")
    (are [x y] (= (let [pattern (read-string x)] [(.-hour pattern) (.-tz pattern)])
                  y)
         "#schedule/weekly-pattern \"Every day\"" [nil nil]
         "#schedule/weekly-pattern \"Every day at 9:00\"" [9 nil]
         "#schedule/weekly-pattern \"Every day PST\"" [nil "PST"]
         "#schedule/weekly-pattern \"Every day at 9:00 PST\"" [9 "PST"])))

(deftest test-pattern-printing
  (are [x y] (= (pr-str x) y)
       (WeeklyPattern. nil nil nil) "#schedule/weekly-pattern \"Every day\""
       (WeeklyPattern. nil 9 nil) "#schedule/weekly-pattern \"Every day at 9:00\""
       (WeeklyPattern. nil nil "PST") "#schedule/weekly-pattern \"Every day PST\""
       (WeeklyPattern. nil 9 "PST") "#schedule/weekly-pattern \"Every day at 9:00 PST\""))

(deftest test-schedule-reading
  (are [x y] (= (let [schedule (read-string x)
                      pattern (.-pattern schedule)]
                  [(s/as-ts (.-start schedule)) (.-hour pattern) (.-tz pattern)])
                y)
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"
       [#inst "2014-02-17T14:00:00.00-00:00" nil nil]
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day at 9:00\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"
       [#inst "2014-02-17T14:00:00.00-00:00" 9 nil]
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day PST\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"
       [#inst "2014-02-17T14:00:00.00-00:00" nil "PST"]
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day at 9:00 PST\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"
       [#inst "2014-02-17T14:00:00.00-00:00" 9 "PST"]))

(deftest test-schedule-printing
  (are [x y] (= (pr-str x) y)
       (WeeklySchedule. (WeeklyPattern. nil nil nil)
                  #inst "2014-02-17T14:00:00.000-00:00")
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"))

(deftest test-pattern-dereference
  (let [pattern (read-string "#schedule/weekly-pattern \"Every day at 9:00 PST\"")
        roughly (fn [tolerance x y]
                  (< (Math/abs (- x y)) tolerance))]
    (is (roughly 50 (.-start @pattern) (s/current-time-millis)))))

(deftest test-schedule-seq
  (let [pattern (read-string "#schedule/weekly-pattern \"Every day at 9:00 PST\"")
        start #inst "2014-02-17T14:00:00.000-00:00"
        sched (anchor pattern start)
        sample (take 5 sched)]
    (is (every? #(= #+clj java.util.Date
                    #+cljs js/Date
                    (type %)) sample))
    (is (= sample
           [#inst "2014-02-17T17:00:00.000-00:00"
            #inst "2014-02-18T17:00:00.000-00:00"
            #inst "2014-02-19T17:00:00.000-00:00"
            #inst "2014-02-20T17:00:00.000-00:00"
            #inst "2014-02-21T17:00:00.000-00:00"])))
  (let [pattern (read-string "#schedule/weekly-pattern \"Every day at 9:00\"")
        start #inst "2014-02-17T14:00:00.000-00:00"
        sched (anchor pattern start)
        sample (take 5 sched)]
    (is (every? #(= #+clj java.util.Date
                    #+cljs js/Date
                    (type %)) sample))
    (is (= sample
           [#inst "2014-02-18T09:00:00.000-00:00"
            #inst "2014-02-19T09:00:00.000-00:00"
            #inst "2014-02-20T09:00:00.000-00:00"
            #inst "2014-02-21T09:00:00.000-00:00"
            #inst "2014-02-22T09:00:00.000-00:00"]))))