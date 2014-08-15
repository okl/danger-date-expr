(ns danger-date-expr.core
  "Date expr central!

  Precisions are down to the nearest second.
  Formats are in UTC.
  Accepted time formats are:
    (1) java.lang.Long, representing seconds since the Unix epoch
    (2) EpochTime, defined inside this module
    (3) org.joda.time.DateTime"
  {:author "Matt Halverson"
   :date "Wed Aug 13 09:48:42 PDT 2014"}
  (:require [roxxi.utils.print :refer [print-expr]]
            [roxxi.utils.common :refer [def-]])
  (:require [clj-time.core :as t]
            [clj-time.periodic :as p]
            [clj-time.coerce :as c])
  (:require [clojure.string :refer [replace]]))

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

;; # Granularity

(def- granularity-order
  [:year
   :month
   :day
   :meridian
   :hour
   :minute
   :second])

(defn- get-finest-granularity [granularity-seq]
  ;; XXX what if all are invalid granularities?
  ;; XXX what if some are invalid granularities?
  (let [positions (map #(.indexOf granularity-order %) granularity-seq)
        position-of-finest (apply max positions)]
    (get granularity-order position-of-finest)))

(def- date-conversion-specs
  {"%Y" {:pattern "yyyy", :granularity :year} ;; = year in four digit format
   "%m" {:pattern "MM", :granularity :month} ;; = month (01 to 12)
   "%d" {:pattern "dd", :granularity :day} ;; = day (01 to 31)
   "%p" {:pattern "aa", :granularity :meridian} ;; = AM or PM
   "%H" {:pattern "HH", :granularity :hour} ;; = hour (00 to 23)
   "%h" {:pattern "hh", :granularity :hour} ;; = hour (01 to 12)
   "%M" {:pattern "mm", :granularity :minute} ;; = minute (00 to 59)
   "%S" {:pattern "ss", :granularity :second} ;; = second (00 to 60 -- 60 to allow for occasional leap seconds)
   })

(def- granularity=>period
  {:year (t/years 1)
   :month (t/months 1)
   :day (t/days 1)
   ;; A change-of-meridian is 11, 12, or 13 hours depending, if you allow
   ;; switching timezones to/from daylight savings halfway through. But if
   ;; you're only formatting things in UTC, like this module is, then
   ;; a change-of-meridian is always a period of 12 hours. Yay!
   :meridian (t/hours 12)
   :hour (t/hours 1)
   :minute (t/minutes 1)
   :second (t/seconds 1)})

(defn- extract-date-parts [string]
  (let [pattern-str (clojure.string/join "|" (keys date-conversion-specs))
        pattern (re-pattern pattern-str)]
    (re-seq pattern string)))

(defn- compute-granularity [string]
  (let [conv-specs (extract-date-parts string)
        grans (map #(get-in date-conversion-specs [% :granularity]) conv-specs)]
    (get-finest-granularity grans)))

;; # Core protocol

(defprotocol DateExprP
  "DateExprP is the basic, all-purpose unit of date-expression. It represents
  a date-dependent string. It's defined by a seq of DateExprParts which all get
  formatted and joined together to create the full date expression."
  (format-expr [_ time]
    "Generates the formatted DateExpr from the given instant in time.")
  (parse-expr [_ formatted-string]
    "Extracts the approximate instant in time (seconds since the epoch) that was
    used to format a formatted DateExpr.

    Why only the approximate instant in time? Because formatting a timestamp
    may involve some loss in precision! If I format '2014/08/13 at 2 a.m.'
    down to a granularity of day, I've lost the information 'at 2 a.m.'

    More generally:
      (format-expr d (parse-expr d formatted)) will get you back formatted
      (parse-expr d (format-expr d time)) will NOT get you back time "))

;; # Implementation of protocol
;;
;; DateExpr knows how to operate on the types defined above
;; (EpochTime, Joda DateTime, and number of seconds since the epoch).
;;
;; It formats into UTC timezone.

(defn- empty-string? [x] (= x ""))
(defn- quote-out-pattern
  "java.text.SimpleDateFormats will ignore parts of the pattern
  that are enclosed in single quotes. e.g. if you try to create
  a SimpleDate Format from the string s3://bucket/yyyy/MM/dd, it
  won't let you because the 'b' in bucket is an illegal pattern.

  But, it will happily work on the string 's3://bucket/'yyyy'/'MM'/'dd

  This function quotes-out the portions of the input string that
  are not date-conversion-specs."
  [string]
  (let [re (re-pattern (clojure.string/join "|" (keys date-conversion-specs)))
        matches (re-seq re string)
        splits (clojure.string/split string re)
        quoted-splits (map #(if (empty-string? %)
                              %
                              (str \' % \')) splits)
        interleaved (interleave quoted-splits (concat matches '("")))
        ;; (count splits) will always be one more than (count matches).
        ;; Hence, (interleave splits matches) will drop the last split!
        ;; To include the last split in there, add an empty string to
        ;; matches -- it'll disappear in the join step :)
        ]
     (clojure.string/join "" interleaved)))

(defn- translate-pattern [string]
  (reduce (fn [to-be-formatted kv]
            (replace to-be-formatted (key kv) (:pattern (val kv))))
          string
          date-conversion-specs))

(defn- ->formatter [date-expr-string]
  (let [quoted (quote-out-pattern date-expr-string)
        translated (translate-pattern quoted)]
    (doto (java.text.SimpleDateFormat. translated)
      (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))))

(defrecord DateExpr [string]
  DateExprP
  (format-expr [_ time]
    (let [formatter (->formatter string)
          timestamp (* 1000 (->seconds-since-epoch time))]
      (.format formatter timestamp)))
  (parse-expr [_ formatted-string]
    (let [formatter (->formatter string)
          date (.parse formatter formatted-string)
          timestamp (.getTime date)]
      (quot timestamp 1000))))



(defn make-date-expr [string]
  (->DateExpr string))

;; # Date-ranges

(defn- date-range
  "Returns a lazy seq of joda DateTimes.

  s and f represent start and finish time.
  f defaults to end of time (i.e. infinite sequence).
  period should be a joda Period."
  ([s period]
     (p/periodic-seq (->joda-date-time s) period))
  ([s f period]
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
                   (date-range s period)))))

(defn formatted-date-range
  "Generates a seq of dates beginning at s, separated by the finest granularity
  of the date-expr, up to the date specified by f.

  Formats the DateExpr across that seq of dates, and returns *that* seq.

  s and f each may be a joda DateTimes, a java.lang.Long representing the number
  of seconds since the epoch, or an EpochTime. The type of s and the type of f
  may be different."
  [s f date-expr]
  (let [gran (compute-granularity (:string date-expr))
        period (get granularity=>period gran)
        the-range (date-range s f period)
        formatted-range (map #(format-expr date-expr %) the-range)]
    formatted-range))
