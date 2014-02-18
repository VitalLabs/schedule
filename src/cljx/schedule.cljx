(ns schedule
  #+clj (:import clojure.lang.IDeref
                 clojure.lang.Seqable
                 clojure.lang.Cons
                 java.util.Date
                 java.util.Calendar
                 java.util.TimeZone
                 java.io.Writer)
  #+cljs (:require [cljs.reader :as reader]))

(defprotocol Pattern
  (anchor  [pattern t]    "Anchors the scheduling pattern at time `t`, yielding a schedule starting after `t`")
  (at-hour [pattern hour] "Constrains pattern, returning a new pattern that generates instants at hour `hour`")
  (in-tz   [pattern tz]   "Shifts pattern, returning a new pattern that occurs in timezoe `tz`."))

(defprotocol Instant
  (as-ts [instant] "Return a plaform native timestamp type corresponding to this instant.")
  (as-millis [instant] "Return a long representing the number of millis since the epoch corresponding to this instant."))

(defn current-time-millis
  []
  #+clj (System/currentTimeMillis)
  #+cljs (.now js/Date)
  )

#+clj (extend-protocol Instant
        Date
        (as-ts [date] date)
        (as-millis [date] (.getTime date))
        Long
        (as-ts [long] (Date. long))
        (as-millis [long] long)
        Calendar
        (as-ts [cal] (.getTime cal))
        (as-millis [cal] (.getTimeInMillis cal)))

#+cljs (extend-protocol Instant
         js/Date
         (as-ts [date] date)
         (as-millis [date] (.getTime date))
         number
         (as-ts [num] (js/Date. num))
         (as-millis [num] num))

(declare update-schedule-pattern)
(declare schedule-lazy-seq)

#+cljs (def tz-offsets
         {"EST" 5
          "EDT" 4
          "CST" 6
          "CDT" 5
          "MST" 7
          "MDT" 6
          "PST" 8
          "PDT" 7})

(defn calculate-next-schedule-instant
  [schedule]
  #+clj (let [pattern (.-pattern schedule)
              hour (.-hour pattern)
              tz (.-tz pattern)
              start (.-start schedule)
              cal (Calendar/getInstance)
              candidate-cal (Calendar/getInstance)]
          (.setTimeInMillis cal start)
          (.setTimeInMillis candidate-cal start)
          (.set candidate-cal Calendar/HOUR_OF_DAY hour)
          (.set candidate-cal Calendar/MINUTE 0)
          (.set candidate-cal Calendar/SECOND 0)
          (.set candidate-cal Calendar/MILLISECOND 0)
          (.setTimeZone candidate-cal (TimeZone/getTimeZone (or tz "UTC")))
          (if (< (.compareTo cal candidate-cal) 0)
            (.getTimeInMillis candidate-cal)
            (do (.add candidate-cal Calendar/DAY_OF_YEAR 1)
                (.getTimeInMillis candidate-cal))))
  #+cljs (let [pattern (.-pattern schedule)
               hour (.-hour pattern)
               tz (.-tz pattern)
               start (.-start schedule)
               date (as-ts start)
               candidate-date (js/Date. (.UTC js/Date
                                              (.getUTCFullYear date)
                                              (.getUTCMonth date)
                                              (.getUTCDate date)
                                              (+ hour (or (tz-offsets tz)
                                                          0))
                                              0
                                              0
                                              0))]
           (if (< (as-millis date) (as-millis candidate-date))
             (as-millis candidate-date)
             (do (.setDate candidate-date (inc (.getDate candidate-date)))
                 (as-millis candidate-date)))))

(deftype Schedule [pattern start]
  Pattern
  (anchor  [this t]    (Schedule. pattern (as-millis t)))
  (at-hour [this hour] (update-schedule-pattern at-hour hour))
  (in-tz   [this tz]   (update-schedule-pattern in-tz tz))
  #+clj Seqable
  #+clj (seq [this] (schedule-lazy-seq this))
  #+cljs ISeqable
  #+cljs (-seq [this] (schedule-lazy-seq this))
  #+cljs IPrintWithWriter
  #+cljs (-pr-writer [schedule writer opts]
           (let [pattern (.-pattern schedule)
                 start (.-start schedule)]
             (-write writer "#schedule/schedule ")
             (-pr-writer {:pattern pattern
                          :start (as-ts start)} writer opts))))

(defn- schedule-lazy-seq
  [sched]
  (let [next-instant-ms (calculate-next-schedule-instant sched)
        pattern (.-pattern sched)]
    (cons (as-ts next-instant-ms)
          (lazy-seq (Schedule. pattern (inc next-instant-ms))))))

(defn- update-schedule-pattern
  [schedule f & args]
  (let [pattern (.-pattern schedule)
        start (.-start pattern)]
   (Schedule. (apply f pattern args) start)))


#+clj (defmethod print-method Schedule
        [^Schedule schedule ^Writer w]
        (let [pattern (.-pattern schedule)
              start (.-start schedule)]
          (.write w "#schedule/schedule ")
          (binding [*out* w]
            (pr {:pattern pattern
                 :start (as-ts start)})) w))

#+cljs (defn- cljs-print-to-writer
         [pattern writer opts]
         (let [hour (.-hour pattern)
               tz (.-tz pattern)]
           (-write writer "#schedule/pattern \"Every day")
           (when hour
             (-write writer " at ")
             (-write writer (str hour))
             (-write writer ":00"))
           (when tz
             (-write writer " ")
             (-write writer tz))
           (-write writer "\"")))

(deftype FloatingPattern [period hour tz]
  Pattern
  (anchor  [pattern t]    (Schedule. pattern (as-millis t)))
  (at-hour [pattern new-hour] (FloatingPattern. period new-hour tz))
  (in-tz   [pattern new-tz]   (FloatingPattern. period hour new-tz))
  #+clj IDeref
  #+clj (deref [pattern] (anchor pattern (current-time-millis)))
  #+cljs IDeref
  #+cljs (-deref [pattern] (anchor pattern (current-time-millis)))
  #+cljs IPrintWithWriter
  #+cljs (-pr-writer [pattern writer opts]
           (cljs-print-to-writer pattern writer opts)))

#+clj (defmethod print-method FloatingPattern
        [^FloatingPattern pattern ^Writer w]
        (let [hour (.-hour pattern)
              tz (.-tz pattern)]
          (.write w "#schedule/pattern \"Every day")
          (when hour
            (.write w " at ")
            (.write w (str hour))
            (.write w ":00"))
          (when tz
            (.write w " ")
            (.write w tz))
          (.write w "\"")))

(defn read-schedule
  [m]
  (let [pattern (:pattern m)
        start (:start m)]
    (Schedule. pattern (as-millis start))))

(defn- string->int
  [int]
  (when int
    #+clj (Integer/parseInt int)
    #+cljs (js/parseInt int 10)))

(defn- floating-pattern
  [period hour tz]
  (FloatingPattern. period (string->int hour) tz))

#+clj (defn read-floating-pattern
        [s]
        (let [re #"Every day(?: at (\d+):00)?(?: (.+))?"
              matcher (re-matcher re s)
              [match hour tz] (re-find matcher)]
          (if match
            (floating-pattern nil hour tz)
            (throw (ex-info (str "Could not read floating pattern: \"" s \")
                            {:s s
                             :match match
                             :hour hour
                             :tz tz})))))

#+cljs (defn read-floating-pattern
         [s]
         (let [re #"Every day(?: at (\d+):00)?(?: (.+))?"
               [match hour tz] (re-matches re s)]
           (if match
             (floating-pattern nil hour tz)
             (throw (ex-info (str "Could not read floating pattern: \"" s \")
                             {:s s
                              :match match
                              :hour hour
                              :tz tz})))))

#+cljs (reader/register-tag-parser! "schedule/pattern" read-floating-pattern)