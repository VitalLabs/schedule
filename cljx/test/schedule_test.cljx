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
    (are [x y] (= (let [{:keys [days time-matches tz]} (read-string x)]
                    [(set days) time-matches tz])
                  y)
         "#schedule/weekly-pattern \"Every day\"" [#{} nil nil]
         "#schedule/weekly-pattern \"Every day at 9:00\"" [#{} #{[9 0]} nil]
         "#schedule/weekly-pattern \"Every day at 9:05\"" [#{} #{[9 5]} nil]
         "#schedule/weekly-pattern \"Every day at 9:05, and 4:15\"" [#{} #{[9 5] [4 15]} nil]
         "#schedule/weekly-pattern \"Every day PST\"" [#{} nil "PST"]
         "#schedule/weekly-pattern \"Every day at 9:00 PST\"" [#{} #{[9 0]} "PST"]
         "#schedule/weekly-pattern \"Monday, Wednesday, and Friday at 9:00, and 15:00\"" [#{:monday :wednesday :friday} #{[9 0] [15 0]} nil])))

(deftest test-pattern-printing
  (are [x y] (= (pr-str x) y)
       (WeeklyPattern. nil nil nil) "#schedule/weekly-pattern \"Every day\""
       (WeeklyPattern. nil #{[9 0]} nil) "#schedule/weekly-pattern \"Every day at 9:00\""
       (WeeklyPattern. nil nil "PST") "#schedule/weekly-pattern \"Every day PST\""
       (WeeklyPattern. nil #{[9 0]} "PST") "#schedule/weekly-pattern \"Every day at 9:00 PST\""
       (WeeklyPattern. nil #{[9 5]} "PST") "#schedule/weekly-pattern \"Every day at 9:05 PST\""
       (WeeklyPattern. [:monday :wednesday :friday] #{[9 0] [15 0]} "PST") "#schedule/weekly-pattern \"Monday, Wednesday, and Friday at 9:00, and 15:00 PST\""))

(deftest test-schedule-reading
  (are [x y] (= (let [{{:keys [time-matches tz] :as pattern} :pattern
                       start :start
                       :as schedule} (read-string x)]
                  [(s/as-ts start) time-matches tz])
                y)
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"
       [#inst "2014-02-17T14:00:00.00-00:00" nil nil]
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day at 9:00\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"
       [#inst "2014-02-17T14:00:00.00-00:00" #{[9 0]} nil]
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day PST\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"
       [#inst "2014-02-17T14:00:00.00-00:00" nil "PST"]
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day at 9:00 PST\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"
       [#inst "2014-02-17T14:00:00.00-00:00" #{[9 0]} "PST"]))

(deftest test-schedule-printing
  (are [x y] (= (pr-str x) y)
       (WeeklySchedule. (WeeklyPattern. nil nil nil)
                  #inst "2014-02-17T14:00:00.000-00:00")
       "#schedule/weekly-schedule {:pattern #schedule/weekly-pattern \"Every day\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"))

(deftest test-pattern-dereference
  (let [pattern (read-string "#schedule/weekly-pattern \"Every day at 9:00 PST\"")
        roughly (fn [tolerance x y]
                  (< (Math/abs (- x y)) tolerance))]
    (is (roughly 50 (:start @pattern) (s/current-time-millis)))))

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
            #inst "2014-02-22T09:00:00.000-00:00"])))
  (let [pattern (read-string "#schedule/weekly-pattern \"Every day at 10:05\"")
        start #inst "2014-02-17T14:00:00.000-00:00"
        sched (anchor pattern start)
        sample (take 5 sched)]
    (is (every? #(= #+clj java.util.Date
                    #+cljs js/Date
                    (type %)) sample))
    (is (= sample
           [#inst "2014-02-18T10:05:00.000-00:00"
            #inst "2014-02-19T10:05:00.000-00:00"
            #inst "2014-02-20T10:05:00.000-00:00"
            #inst "2014-02-21T10:05:00.000-00:00"
            #inst "2014-02-22T10:05:00.000-00:00"])))
  (let [pattern (read-string "#schedule/weekly-pattern \"Monday, Wednesday, and Friday at 9:00, and 15:00\"")
        start #inst "2014-02-17T14:00:00.000-00:00"
        sched (anchor pattern start)
        sample (take 5 sched)]
    (is (every? #(= #+clj java.util.Date
                    #+cljs js/Date
                    (type %)) sample))
    (is (= sample
           [#inst "2014-02-17T15:00:00.000-00:00"
            #inst "2014-02-19T09:00:00.000-00:00"
            #inst "2014-02-19T15:00:00.000-00:00"
            #inst "2014-02-21T09:00:00.000-00:00"
            #inst "2014-02-21T15:00:00.000-00:00"])))
  (let [pattern (read-string "#schedule/weekly-pattern \"Monday at 9:00, and 15:00\"")
        start #inst "2014-02-17T14:00:00.000-00:00"
        sched (anchor pattern start)
        sample (take 5 sched)]
    (is (every? #(= #+clj java.util.Date
                    #+cljs js/Date
                    (type %)) sample))
    (is (= sample
           [#inst "2014-02-17T15:00:00.000-00:00"
            #inst "2014-02-24T09:00:00.000-00:00"
            #inst "2014-02-24T15:00:00.000-00:00"
            #inst "2014-03-03T09:00:00.000-00:00"
            #inst "2014-03-03T15:00:00.000-00:00"])))
  (let [pattern (read-string "#schedule/weekly-pattern \"Monday, and Wednesday at 9:00\"")
        start #inst "2014-02-17T14:00:00.000-00:00"
        sched (anchor pattern start)
        sample (take 5 sched)]
    (is (every? #(= #+clj java.util.Date
                    #+cljs js/Date
                    (type %)) sample))
    (is (= sample
           [#inst "2014-02-19T09:00:00.000-00:00"
            #inst "2014-02-24T09:00:00.000-00:00"
            #inst "2014-02-26T09:00:00.000-00:00"
            #inst "2014-03-03T09:00:00.000-00:00"
            #inst "2014-03-05T09:00:00.000-00:00"]))))