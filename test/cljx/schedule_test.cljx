(ns schedule-test
  #+clj (:require [clojure.test :as t :refer (is deftest testing are)]
                  [schedule :as s :refer (anchor current-time-millis)])
  #+cljs (:require-macros [cemerick.cljs.test
                           :refer (is deftest testing are)])
  #+cljs (:require [cemerick.cljs.test :as t]
                   [schedule :as s :refer (anchor current-time-millis)]
                   [cljs.reader :refer (read-string)])
  (:import schedule.FloatingPattern
           schedule.Schedule))

(deftest test-pattern-reading
  (testing "types"
    (are [x] (= (type (read-string x)) FloatingPattern)
         "#schedule/pattern \"Every day\""
         "#schedule/pattern \"Every day at 9:00\""
         "#schedule/pattern \"Every day PST\""
         "#schedule/pattern \"Every day at 9:00 PST\"")
    (are [x y] (= (let [pattern (read-string x)] [(.-hour pattern) (.-tz pattern)])
                  y)
         "#schedule/pattern \"Every day\"" [nil nil]
         "#schedule/pattern \"Every day at 9:00\"" [9 nil]
         "#schedule/pattern \"Every day PST\"" [nil "PST"]
         "#schedule/pattern \"Every day at 9:00 PST\"" [9 "PST"])))

(deftest test-pattern-printing
  (are [x y] (= (pr-str x) y)
       (FloatingPattern. nil nil nil) "#schedule/pattern \"Every day\""
       (FloatingPattern. nil 9 nil) "#schedule/pattern \"Every day at 9:00\""
       (FloatingPattern. nil nil "PST") "#schedule/pattern \"Every day PST\""
       (FloatingPattern. nil 9 "PST") "#schedule/pattern \"Every day at 9:00 PST\""))

(deftest test-schedule-printing
  (are [x y] (= (pr-str x) y)
       (Schedule. (FloatingPattern. nil nil nil)
                  #inst "2014-02-17T14:00:00.000-00:00")
       "#schedule/schedule {:pattern #schedule/pattern \"Every day\", :start #inst \"2014-02-17T14:00:00.000-00:00\"}"))

(deftest test-pattern-dereference
  (let [pattern (read-string "#schedule/pattern \"Every day at 9:00 PST\"")
        roughly (fn [tolerance x y]
                  (< (Math/abs (- x y)) tolerance))]
    (is (roughly 50 (.-start @pattern) (current-time-millis)))))

(deftest test-schedule-seq
  (let [pattern (read-string "#schedule/pattern \"Every day at 9:00 PST\"")
        start #inst "2014-02-17T14:00:00.000-00:00"
        sched (anchor pattern start)
        sample (take 10 sched)]
    (is (every? #(= #+clj java.util.Date
                    #+cljs js/Date
                    (type %)) sample))))