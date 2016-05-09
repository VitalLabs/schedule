(ns schedule
  #+clj (:require [clojure.string :as str])
  #+clj (:import clojure.lang.IDeref
                 clojure.lang.Seqable
                 clojure.lang.Cons
                 clojure.lang.ILookup
                 java.util.Date
                 java.util.Calendar
                 java.util.TimeZone
                 java.io.Writer)
  #+cljs (:require [cljs.reader :as reader]
                   [clojure.string :as str]))

(defprotocol Pattern
  (anchor  [pattern t]    "Anchors the scheduling pattern at time `t`, yielding a schedule starting after `t`"))

(defprotocol WeeklyTriggered
  (at-hour [pattern hour] "Return a new pattern that only generates instants at hour `hour`")
  (add-hour [pattern hour] "Returns a new pattern that generates instants at hour `hour`")
  (remove-hour [pattern hour] "Returns a new pattern that will not generate instants at hour `hour`")
  (at-time [pattern hour minute] "Return a new pattern that only generates instants at the time indicated by `hour` and `minute`")
  (add-time [pattern hour minute] "Returns a new pattern that generates instants at the time indicated by `hour` and `minute")
  (remove-time [pattern hour minute] "Returns a new pattern that will not generate instants at the time indicated by `hour` and `minute`"))

(defprotocol MonthlyTriggered
  (at-day [pattern day] "Return a new pattern that only generates instants at day of the month `day`")
  (add-day [pattern day] "Returns a new pattern that generates instants at day of the month `day`")
  (remove-day [pattern day] "Returns a new pattern that will not generate instants at day of the month `day`")
  (at-hour [pattern hour] "Return a new pattern that only generates instants at hour `hour`")
  (add-hour [pattern hour] "Returns a new pattern that generates instants at hour `hour`")
  (remove-hour [pattern hour] "Returns a new pattern that will not generate instants at hour `hour`")
  (at-time [pattern hour minute] "Return a new pattern that only generates instants at the time indicated by `hour` and `minute`")
  (add-time [pattern hour minute] "Returns a new pattern that generates instants at the time indicated by `hour` and `minute")
  (remove-time [pattern hour minute] "Returns a new pattern that will not generate instants at the time indicated by `hour` and `minute`"))

(defprotocol TimeZoneable
  (in-tz [timezonable tz] "Shifts timezoneable, returning a new object projected in timezoe `tz`."))

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

(declare ^:private update-schedule-pattern)
(declare ^:private update-monthly-schedule-pattern)
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

(defn roll-to
  "Rolls ms forward, finding the next time ms which occurs on day at
  hour and minute in tz"
  [ms day hour minute tz]
  #+clj (let [weekday-map {:monday Calendar/MONDAY
                           :tuesday Calendar/TUESDAY
                           :wednesday Calendar/WEDNESDAY
                           :thursday Calendar/THURSDAY
                           :friday Calendar/FRIDAY
                           :saturday Calendar/SATURDAY
                           :sunday Calendar/SUNDAY}
              cal (Calendar/getInstance)]
          (.setTimeInMillis cal ms)
          (.set cal Calendar/DAY_OF_WEEK (weekday-map day))
          (.set cal Calendar/HOUR_OF_DAY hour)
          (.set cal Calendar/MINUTE minute)
          (.set cal Calendar/SECOND 0)
          (.set cal Calendar/MILLISECOND 0)
          (.setTimeZone cal (TimeZone/getTimeZone (or tz "UTC")))
          (if (<= ms (.getTimeInMillis cal))
            (.getTimeInMillis cal)
            (do (.add cal Calendar/DAY_OF_YEAR 7)
                (.getTimeInMillis cal))))
  #+cljs (let [weekday-map {:monday 1
                            :tuesday 2
                            :wednesday 3
                            :thursday 4
                            :friday 5
                            :saturday 6
                            :sunday 0}
               date (as-ts ms)
               new-date (js/Date. (.UTC js/Date
                                        (.getUTCFullYear date)
                                        (.getUTCMonth date)
                                        (.getUTCDate date)
                                        (+ hour (or (tz-offsets tz)
                                                    0))
                                        minute
                                        0
                                        0))
               distance (- (weekday-map day) (.getDay date))]
           (.setDate new-date (+ (.getDate new-date) distance))
           (if (<= ms (as-millis new-date))
             (as-millis new-date)
             (do (.setDate new-date (+ (.getDate new-date) 7))
                 (as-millis new-date)))))

(defn- calculate-next-schedule-instant
  [{{:keys [tz time-matches days] :as pattern} :pattern
    start :start
    :as schedule}]
  (let [days (or days
                 [:monday :tuesday :wednesday :thursday :friday :saturday :sunday])
        candidates (for [day days [hour min] time-matches]
                     (roll-to start day hour min tz))]
    (apply min candidates)))

(defn print-to-writer
  [val writer opts]
  #+cljs (-pr-writer val writer opts)
  #+clj (binding [*out* writer]
          (pr val)))

(defn print-weekly-schedule-to-abstract-writer
  [{:keys [pattern start] :as schedule} writer write-fn opts]
  (write-fn writer "#schedule/weekly-schedule ")
  (print-to-writer {:pattern pattern
                    :start (as-ts start)} writer opts))

(defn- lookup-schedule-key
  ([schedule key] (lookup-schedule-key schedule key nil))
  ([schedule key not-found]
   (let [lookup (condp get key
                  #{:pattern 'pattern} (.-pattern schedule)
                  #{:start 'start} (.-start schedule)
                  ::not-found)]
     (if (= lookup ::not-found)
       not-found
       lookup))))

(declare schedules-equiv?)

;;
;; WeeklySchedule
;;

(deftype WeeklySchedule [pattern start]
  Pattern
  (anchor  [this t]    (WeeklySchedule. pattern (as-millis t)))
  WeeklyTriggered
  (at-hour [this hour] (update-schedule-pattern this at-hour hour))
  (add-hour [this hour] (update-schedule-pattern this add-hour hour))
  (remove-hour [this hour] (update-schedule-pattern this remove-hour hour))
  (at-time [this hour minute] (update-schedule-pattern this at-time hour minute))
  (add-time [this hour minute] (update-schedule-pattern this add-time hour minute))
  (remove-time [this hour minute] (update-schedule-pattern this remove-time hour minute))
  TimeZoneable
  (in-tz   [this tz]   (update-schedule-pattern this in-tz tz))
  #+clj Object
  #+clj (equals [this schedule] (schedules-equiv? this schedule))
  #+cljs IEquiv
  #+cljs (-equiv [this schedule] (schedules-equiv? this schedule))
  #+clj ILookup
  #+clj (valAt [this key] (lookup-schedule-key this key))
  #+clj (valAt [this key not-found] (lookup-schedule-key this key not-found))
  #+cljs ILookup
  #+cljs (-lookup [this key] (lookup-schedule-key this key))
  #+cljs (-lookup [this key not-found] (lookup-schedule-key this key not-found))
  #+clj Seqable
  #+clj (seq [this] (schedule-lazy-seq this))
  #+cljs ISeqable
  #+cljs (-seq [this] (schedule-lazy-seq this))
  #+cljs IPrintWithWriter
  #+cljs (-pr-writer [schedule writer opts]
           (print-weekly-schedule-to-abstract-writer schedule writer -write opts)))

;;
;; Monthly
;;

(deftype MonthlySchedule [pattern start]
  Pattern
  (anchor  [this t]    (MonthlySchedule. pattern (as-millis t)))
  MonthlyTriggered
  (at-day [this day] (update-monthly-schedule-pattern this at-day day))
  (add-day [this day] (update-monthly-schedule-pattern this add-day day))
  (remove-day [this day] (update-monthly-schedule-pattern this remove-day day))
  (at-hour [this hour] (update-monthly-schedule-pattern this at-hour hour))
  (add-hour [this hour] (update-monthly-schedule-pattern this add-hour hour))
  (remove-hour [this hour] (update-monthly-schedule-pattern this remove-hour hour))
  (at-time [this hour minute] (update-monthly-schedule-pattern this at-time hour minute))
  (add-time [this hour minute] (update-monthly-schedule-pattern this add-time hour minute))
  (remove-time [this hour minute] (update-monthly-schedule-pattern this remove-time hour minute))
  TimeZoneable
  (in-tz   [this tz]   (update-monthly-schedule-pattern this in-tz tz))
  #+clj Object
  #+clj (equals [this schedule] (schedules-equiv? this schedule))
  #+cljs IEquiv
  #+cljs (-equiv [this schedule] (schedules-equiv? this schedule))
  #+clj ILookup
  #+clj (valAt [this key] (lookup-schedule-key this key))
  #+clj (valAt [this key not-found] (lookup-schedule-key this key not-found))
  #+cljs ILookup
  #+cljs (-lookup [this key] (lookup-schedule-key this key))
  #+cljs (-lookup [this key not-found] (lookup-schedule-key this key not-found))
  #+clj Seqable
  #+clj (seq [this] (schedule-lazy-seq this))
  #+cljs ISeqable
  #+cljs (-seq [this] (schedule-lazy-seq this))
  #+cljs IPrintWithWriter
  #+cljs (-pr-writer [schedule writer opts]
           (print-weekly-schedule-to-abstract-writer schedule writer -write opts)))

(defn schedules-equiv?
  [schedule1 schedule2]
  (and (instance? WeeklySchedule schedule1)
       (instance? WeeklySchedule schedule2)
       (= (:pattern schedule1) (:pattern schedule2))
       (= (:start schedule1) (:start schedule2))))

#+clj (defmethod print-method WeeklySchedule
        [^WeeklySchedule schedule ^Writer w]
        (print-weekly-schedule-to-abstract-writer schedule w #(.write %1 %2) nil))

(defn schedule-lazy-seq
  [{:keys [pattern] :as schedule}]
  (let [next-instant-ms (calculate-next-schedule-instant schedule)]
    (cons (as-ts next-instant-ms)
          (lazy-seq (WeeklySchedule. pattern (inc next-instant-ms))))))

(defn- update-schedule-pattern
  [{:keys [pattern start]} f & args]
  (println [f pattern args start])
  (WeeklySchedule. (apply f pattern args) start))

(defn- update-monthly-schedule-pattern
  [{:keys [pattern start]} f & args]
  (println [f pattern args start])
  (MonthlySchedule. (apply f pattern args) start))

(defn- write-n
  [coll writer write-fn serial-fn empty-fn]
  (case (count coll)
    0 (write-fn writer (empty-fn))
    1 (write-fn writer (serial-fn (first coll)))
    2 (do (write-fn writer (serial-fn (first coll)))
          (write-fn writer ", and ")
          (write-fn writer (serial-fn (second coll))))
    7 (write-fn writer (empty-fn))
    (do (doseq [item (butlast coll)]
          (write-fn writer (serial-fn item))
          (write-fn writer ", "))
        (write-fn writer "and ")
        (write-fn writer (serial-fn (last coll))))))

(defn- print-weekly-pattern-to-abstract-writer
  [{:keys [days time-matches tz] :as pattern} writer write-fn opts]
  (write-fn writer "#schedule/weekly-pattern \"")
  (write-n days writer write-fn #(str/capitalize (name %)) (constantly "Every day"))
  (when time-matches
    (write-fn writer " at "))
  (write-n (sort time-matches) writer write-fn #(let [[hour minute] %]
                                                  (str hour ":" (when (< minute 10) "0") minute))
           (constantly ""))
  (when tz
    (write-fn writer " ")
    (write-fn writer tz))
  (write-fn writer "\""))

(defn- print-monthly-pattern-to-abstract-writer
  [{:keys [days time-matches tz] :as pattern} writer write-fn opts]
  (write-fn writer "#schedule/monthly-pattern \"")
  (write-n days writer write-fn #(str/capitalize (name %)) (constantly "Every day"))
  (when time-matches
    (write-fn writer " at "))
  (write-n (sort time-matches) writer write-fn #(let [[hour minute] %]
                                                  (str hour ":" (when (< minute 10) "0") minute))
           (constantly ""))
  (when tz
    (write-fn writer " ")
    (write-fn writer tz))
  (write-fn writer "\""))

(defn- lookup-pattern-key
  ([pattern key] (lookup-pattern-key pattern key nil))
  ([pattern key not-found]
   (let [lookup (condp get key
                  #{:days 'days} (.-days pattern)
                  #{:time-matches 'time-matches} (.-time-matches pattern)
                  #{:tz 'tz} (.-tz pattern)
                  ::not-found)]
     (if (= lookup ::not-found)
       not-found
       lookup))))

(declare patterns-equiv? monthly-patterns-equiv?)

(deftype WeeklyPattern [days time-matches tz]
  Pattern
  (anchor  [pattern t]    (WeeklySchedule. pattern (as-millis t)))
  WeeklyTriggered
  (at-hour [pattern new-hour] (WeeklyPattern. days #{[new-hour 0]} tz))
  (add-hour [pattern new-hour] (WeeklyPattern. days (conj time-matches [new-hour 0]) tz))
  (remove-hour [pattern del-hour] (WeeklyPattern. days (disj time-matches [del-hour 0]) tz))
  (at-time [pattern new-hour new-minute] (WeeklyPattern. days #{[new-hour new-minute]} tz))
  (add-time [pattern new-hour new-minute] (WeeklyPattern. days (conj time-matches [new-hour new-minute]) tz))
  (remove-time [pattern del-hour del-minute] (WeeklyPattern. days (disj time-matches [del-hour del-minute]) tz))
  TimeZoneable
  (in-tz   [pattern new-tz]   (WeeklyPattern. days time-matches new-tz))
  #+clj Object
  #+clj (equals [this pattern] (patterns-equiv? this pattern))
  #+cljs IEquiv
  #+cljs (-equiv [this pattern] (patterns-equiv? this pattern))
  #+clj ILookup
  #+clj (valAt [this key] (lookup-pattern-key this key))
  #+clj (valAt [this key not-found] (lookup-pattern-key this key not-found))
  #+cljs ILookup
  #+cljs (-lookup [this key] (lookup-pattern-key this key))
  #+cljs (-lookup [this key not-found] (lookup-pattern-key this key not-found))
  #+clj IDeref
  #+clj (deref [pattern] (anchor pattern (current-time-millis)))
  #+cljs IDeref
  #+cljs (-deref [pattern] (anchor pattern (current-time-millis)))
  #+cljs IPrintWithWriter
  #+cljs (-pr-writer [pattern writer opts]
           (print-weekly-pattern-to-abstract-writer pattern writer -write opts)))

(deftype MonthlyPattern [days time-matches tz]
  Pattern
  (anchor  [pattern t]    (MonthlySchedule. pattern (as-millis t)))
  WeeklyTriggered
  (at-hour [pattern new-hour] (MonthlyPattern. days #{[new-hour 0]} tz))
  (add-hour [pattern new-hour] (MonthlyPattern. days (conj time-matches [new-hour 0]) tz))
  (remove-hour [pattern del-hour] (MonthlyPattern. days (disj time-matches [del-hour 0]) tz))
  (at-time [pattern new-hour new-minute] (MonthlyPattern. days #{[new-hour new-minute]} tz))
  (add-time [pattern new-hour new-minute] (MonthlyPattern. days (conj time-matches [new-hour new-minute]) tz))
  (remove-time [pattern del-hour del-minute] (MonthlyPattern. days (disj time-matches [del-hour del-minute]) tz))
  TimeZoneable
  (in-tz   [pattern new-tz]   (MonthlyPattern. days time-matches new-tz))
  #+clj Object
  #+clj (equals [this pattern] (monthly-patterns-equiv? this pattern))
  #+cljs IEquiv
  #+cljs (-equiv [this pattern] (monthly-patterns-equiv? this pattern))
  #+clj ILookup
  #+clj (valAt [this key] (lookup-pattern-key this key))
  #+clj (valAt [this key not-found] (lookup-pattern-key this key not-found))
  #+cljs ILookup
  #+cljs (-lookup [this key] (lookup-pattern-key this key))
  #+cljs (-lookup [this key not-found] (lookup-pattern-key this key not-found))
  #+clj IDeref
  #+clj (deref [pattern] (anchor pattern (current-time-millis)))
  #+cljs IDeref
  #+cljs (-deref [pattern] (anchor pattern (current-time-millis)))
  #+cljs IPrintWithWriter
  #+cljs (-pr-writer [pattern writer opts]
           (print-monthly-pattern-to-abstract-writer pattern writer -write opts)))

(defn patterns-equiv?
  [pattern1 pattern2]
  (and (instance? WeeklyPattern pattern1)
       (instance? WeeklyPattern pattern2)
       (= (:days pattern1) (:days pattern2))
       (= (:time-matches pattern1) (:time-matches pattern2))
       (= (:tz pattern1) (:tz pattern2))))


#+clj (defmethod print-method WeeklyPattern
        [^WeeklyPattern pattern ^Writer w]
        (print-weekly-pattern-to-abstract-writer pattern w #(.write %1 %2) nil))

(defn monthly-patterns-equiv?
  [pattern1 pattern2]
  (and (instance? MonthlyPattern pattern1)
       (instance? MonthlyPattern pattern2)
       (= (:days pattern1) (:days pattern2))
       (= (:time-matches pattern1) (:time-matches pattern2))
       (= (:tz pattern1) (:tz pattern2))))


#+clj (defmethod print-method MonthlyPattern
        [^MonthlyPattern pattern ^Writer w]
        (print-monthly-pattern-to-abstract-writer pattern w #(.write %1 %2) nil))

(defn read-schedule
  [m]
  (let [pattern (:pattern m)
        start (:start m)]
    (WeeklySchedule. pattern (as-millis start))))

(defn read-monthly-schedule
  [m]
  (let [pattern (:pattern m)
        start (:start m)]
    (MonthlySchedule. pattern (as-millis start))))

(defn- string->int
  [int]
  (when int
    #+clj (Integer/parseInt int)
    #+cljs (js/parseInt int 10)))

(defn- floating-pattern
  [days time-matches tz]
  (WeeklyPattern. days time-matches tz))

(defn- match-globs
  [re s]
  #+clj (let [matcher (re-matcher re s)]
          (re-find matcher))
  #+cljs (re-matches re s))

(def every-day [:monday :tuesday :wednesday :thursday :friday :saturday :sunday])

(defn pull-days
  ([s]
   (pull-days s nil))
  ([s days]
   (let [day-re #"((?:, )?(?:and )?((?:Every |Mon|Tues|Wednes|Thurs|Fri|Satur|Sun)day)).*"
         [match root first-day] (match-globs day-re s)]
     (cond (nil? first-day)
           {:days (reverse days)
            :days-rest (.substring s (count root))}
           (= first-day "Every day")
           {:days every-day
            :days-rest (.substring s (count root))}
           :else
           (recur (.substring s (count root))
                  (conj days (keyword (.toLowerCase first-day))))))))

(defn pull-times
  ([s]
   (let [[match times-etc] (match-globs #" at (.*)" s)]
     (if times-etc
       (pull-times times-etc #{})
       {:times nil
        :times-rest s})))
  ([s times]
   (let [time-re #"((?:, )?(?:and )?(\d{1,2}):(\d{2})).*"
         [match root hour-str minute-str] (match-globs time-re s)]
     (if-not (and hour-str minute-str)
       {:times times
        :times-rest (.substring s (count root))}
       (recur (.substring s (count root))
              (conj times [(string->int hour-str) (string->int minute-str)]))))))

(defn read-floating-pattern
  [s]
  (let [{:keys [days days-rest]} (pull-days s)
        {:keys [times times-rest]} (pull-times days-rest)
        tz-re #"(?: (.+))?"
        [match tz] (match-globs tz-re times-rest)]
    (floating-pattern days times tz)))

#+cljs (reader/register-tag-parser! "schedule/weekly-pattern" read-floating-pattern)
#+cljs (reader/register-tag-parser! "schedule/weekly-schedule" read-schedule)

#+cljs (reader/register-tag-parser! "schedule/monthly-pattern" read-floating-pattern)
#+cljs (reader/register-tag-parser! "schedule/monthly-schedule" read-monthly-schedule)
