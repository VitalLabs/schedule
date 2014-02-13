(ns schedule
  (:import clojure.lang.IDeref
           clojure.lang.Seqable
           clojure.lang.Cons
           java.util.Date
           java.util.Calendar
           java.util.TimeZone
           java.io.Writer))

(defprotocol Pattern
  (anchor  [pattern t]    "Anchors the scheduling pattern at time `t`, yielding a schedule starting after `t`")
  (at-hour [pattern hour] "Constrains pattern, returning a new pattern that generates instants at hour `hour`")
  (in-tz   [pattern tz]   "Shifts pattern, returning a new pattern that occurs in timezoe `tz`."))

(declare update-schedule-pattern)

(defn calculate-next-schedule-instant
  [schedule]
  (let [{:keys [hour tz] :as pattern} (.pattern schedule)
        start (.start schedule)
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
          (.getTimeInMillis candidate-cal)))))

(deftype Schedule [pattern start]
  Pattern
  (anchor  [this t]    (Schedule. pattern t))
  (at-hour [this hour] (update-schedule-pattern at-hour hour))
  (in-tz   [this tz]   (update-schedule-pattern in-tz tz))
  Seqable
  (seq [this] (let [next-instant-ms (calculate-next-schedule-instant this)
                    pattern (.pattern this)]
                (cons (Date. next-instant-ms)
                      (lazy-seq (Schedule. pattern (inc next-instant-ms)))))))

(defn- update-schedule-pattern
  [schedule f & args]
  (let [pattern (.pattern schedule)
        start (.start pattern)]
   (Schedule. (apply f pattern args) start)))


(defmethod print-method Schedule
  [^Schedule schedule ^Writer w]
  (let [pattern (.pattern schedule)
        start (.start schedule)]
    (.write w "#schedule/schedule ")
    (binding [*out* w]
      (pr {:pattern pattern
           :start (Date. start)})) w))

(defrecord FloatingPattern [period hour tz]
  Pattern
  (anchor  [pattern t]    (Schedule. pattern t))
  (at-hour [pattern hour] (assoc pattern :hour hour))
  (in-tz   [pattern tz]   (assoc pattern :tz tz))
  IDeref
  (deref [pattern] (anchor pattern (System/currentTimeMillis))))

(defmethod print-method FloatingPattern
  [^FloatingPattern {:keys [hour tz] :as pattern} ^Writer w]
  (.write w "#schedule/pattern \"Every day")
  (when hour
    (.write w " at ")
    (.write w (str hour))
    (.write w ":00"))
  (when tz
    (.write w " ")
    (.write w tz))
  (.write w "\""))

(defn read-schedule
  [m]
  (let [pattern (:pattern m)
        start (:start m)]
    (Schedule. pattern (.getTime start))))

(defn read-floating-pattern
  [s]
  (let [re #"Every day(?: at (\d+):00)?(?: (.+))?"
        matcher (re-matcher re s)
        [match hour tz] (re-find matcher)]
    (if match
      (FloatingPattern. nil (Integer/parseInt hour) tz)
      (throw (ex-info (str "Could not read floating pattern: \"" s \")
                      {:s s
                       :match match
                       :hour hour
                       :tz tz})))))