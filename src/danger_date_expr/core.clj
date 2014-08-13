(ns danger-date-expr.core
  "Date expr central!

  Precisions are to the nearest second.
  Formats are in UTC.
  Accepted time formats are:
    (1) java.lang.Long, representing seconds since the Unix epoch
    (2) EpochTime, defined inside this module
    (3) org.joda.time.DateTime"
  {:author "Matt Halverson"
   :date "Wed Aug 13 09:48:42 PDT 2014"}
  (:require [roxxi.utils.print :refer [print-expr]])
  (:require [clj-time.core :as t]
            [clj-time.periodic :as p]
            [clj-time.coerce :as c]))

;; # Define an EpochTime type

(defrecord EpochTime [seconds])
(defn make-epoch-time [seconds]
  (->EpochTime seconds))

;; # Define coercion functions

(defmulti ->joda-date-time type)
(defmethod ->joda-date-time java.lang.Long [long]
  ;; assume that long is the number of *seconds* since epoch
  (c/from-long (* long 1000)))
(defmethod ->joda-date-time EpochTime [epoch-time]
  (->joda-date-time (:seconds epoch-time)))
(defmethod ->joda-date-time org.joda.time.DateTime [joda-dt]
  joda-dt)
(defmethod ->joda-date-time :default [thing]
  (throw (RuntimeException.
          (format "Don't know how to convert %s to joda DateTime" (str thing)))))

(defmulti ->epoch-time type)
(defmethod ->epoch-time java.lang.Long [long]
  ;; assume that long is the number of *seconds* since epoch
  (make-epoch-time long))
(defmethod ->epoch-time EpochTime [epoch-time]
  epoch-time)
(defmethod ->epoch-time org.joda.time.DateTime [joda-dt]
  (make-epoch-time (quot (c/to-long joda-dt) 1000)))
(defmethod ->epoch-time :default [thing]
  (throw (RuntimeException.
          (format "Don't know how to convert %s to EpochTime" (str thing)))))


(defmulti ->seconds-since-epoch type)
(defmethod ->seconds-since-epoch java.lang.Long [long]
  ;; assume that long is the number of *seconds* since epoch
  long)
(defmethod ->seconds-since-epoch EpochTime [epoch-time]
  (:seconds epoch-time))
(defmethod ->seconds-since-epoch org.joda.time.DateTime [joda-dt]
  (quot (c/to-long joda-dt) 1000))
(defmethod ->seconds-since-epoch :default [thing]
  (throw (RuntimeException.
          (format "Don't know how to convert %s to seconds since epoch" (str thing)))))


(defn epoch-time-now []
  (->epoch-time (quot (System/currentTimeMillis) 1000)))

;; # Core protocols

(defprotocol DateExprPartP
  "DateExprPartP is the sub-unit of date-expression. It represents
  a portion of a DateExpr. It's fully described by:
    (1) a constant prefix-string
    (2) a date-pattern (java.text.SimpleDateFormat style)
    (3) a constant postfix-string"
  (format-part [_ time]
    "Generates the formatted DateExprPart from the given instant in time."))

(defprotocol DateExprP
  "DateExprP is the basic, all-purpose unit of date-expression. It represents
  a date-dependent string. It's defined by a seq of DateExprParts which all get
  formatted and joined together to create the full date expression."
  (format-expr [_ time]
    "Generates the formatted DateExpr from the given instant in time."))

;; # Implementations of protocols
;;
;; These know how to operate on the types defined above
;; (EpochTime, Joda DateTime, and number of seconds since the epoch).
;;
;; They format into UTC timezone.

(defrecord DateExprPart [pre expr post]
  DateExprPartP
  (format-part [_ time]
    (let [utc (java.util.TimeZone/getTimeZone "UTC")
          formatter (doto (java.text.SimpleDateFormat. expr)
                      (.setTimeZone utc))
          timestamp (* 1000 (->seconds-since-epoch time))
          formatted (.format formatter timestamp)]
      (str pre formatted post))))

(defn make-date-expr-part [pre expr post]
  (->DateExprPart pre expr post))

(defrecord DateExpr [date-expr-parts]
  DateExprP
  (format-expr [_ time]
    (clojure.string/join "" (map #(format-part % time) date-expr-parts))))

(defn make-date-expr [date-expr-parts]
  (->DateExpr date-expr-parts))

;; # Ranges

(defn- date-range
  "Returns a lazy seq of joda DateTimes.

  s and f represent start and finish time.
  f defaults to end of time (i.e. infinite sequence).
  period-maker should be a function which generates a joda Period."
  ([s period-maker]
     (p/periodic-seq (->joda-date-time s)
                     (period-maker 1)))
  ([s f period-maker]
     (let [s (->seconds-since-epoch s)
           f (->seconds-since-epoch f)
           ;; Round s and f to the nearest second, because this library
           ;; only provides second-level precision.
           ;;
           ;; Why? Ultimately, we round to prevent unexpected behavior in
           ;; which s is sub-second precision and f is not. i.e. if we have
           ;;    s is today at 4:00:00.556 a.m.
           ;;    f is today at 4:01:00.000 a.m.
           ;;    period-maker is t/minutes
           ;; then the seq would be (4:00:00.556) and not
           ;; (4:00:00.556, 4:01:00.556)
           ;;
           ;; So for more behavior that's (imo) more intuitive, round
           ;; to the nearest second. That means that in the above example,
           ;; the seq would be (4:00:00.000, 4:01:00.000)
           f-dt (->joda-date-time f)]
       (take-while #(or (t/before? % f-dt)
                        (= % f-dt))
                   (date-range s period-maker)))))

(defn formatted-date-range
  "Generates a seq of dates beginning at s, separated by the specified period,
  up to the date specified by f.

  Formats the DateExpr across that seq of dates, and returns *that* seq.

  s and f each may be a joda DateTimes, a java.lang.Long representing the number
  of seconds since the epoch, or an EpochTime. The type of s and the type of f
  may be different."
  [s f date-expr period]
  (let [period-maker (get {:years t/years
                           :months t/months
                           :weeks t/weeks
                           :days t/days
                           :hours t/hours
                           :minutes t/minutes}
                          period)
        the-range (date-range s f period-maker)
        formatted-range (map (partial format-expr date-expr) the-range)]
    formatted-range))

(defn year-range   [s f date-expr] (formatted-date-range s f date-expr :years))
(defn month-range  [s f date-expr] (formatted-date-range s f date-expr :months))
(defn week-range   [s f date-expr] (formatted-date-range s f date-expr :weeks))
(defn day-range    [s f date-expr] (formatted-date-range s f date-expr :days))
(defn hour-range   [s f date-expr] (formatted-date-range s f date-expr :hours))
(defn minute-range [s f date-expr] (formatted-date-range s f date-expr :minutes))
