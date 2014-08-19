(ns date-expr.core
  "Date expr central!

  Precisions are down to the nearest second.
  Formats are in UTC timezone, unless another timezone is specified.
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
            [clj-time.coerce :as c]
            [clj-time.format :as f])
  (:require [clojure.string :as s]
            [clojure.tools.logging :as log])
  (:import (org.joda.time DateTime DateTimeZone)
           (java.util TimeZone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Define an EpochTime type

(defrecord EpochTime [seconds])
(defn make-epoch-time [seconds]
  (->EpochTime seconds))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Coercion functions

(defmulti ->joda-date-time type)
(defmethod ->joda-date-time java.lang.Long [long]
  ;; assume that long is the number of *seconds* since epoch
  (c/from-long (* long 1000)))
(defmethod ->joda-date-time EpochTime [epoch-time]
  (->joda-date-time (:seconds epoch-time)))
(defmethod ->joda-date-time DateTime [joda-dt]
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
(defmethod ->epoch-time DateTime [joda-dt]
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
(defmethod ->seconds-since-epoch DateTime [joda-dt]
  (quot (c/to-long joda-dt) 1000))
(defmethod ->seconds-since-epoch :default [thing]
  (throw (RuntimeException.
          (format "Don't know how to convert %s to seconds since epoch" (str thing)))))

(defn epoch-time-now []
  (->epoch-time (quot (System/currentTimeMillis) 1000)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Pattern definitions (including their granularities)

(def- granularity-order
  [:year
   :month
   :day
   :meridian
   :hour
   :minute
   :second])

(defn- get-finest-granularity [granularity-seq]
  ;; XXX what if it's an empty seq?
  ;; XXX what if all are invalid granularities?
  ;; XXX what if some are invalid granularities?
  (let [positions (map #(.indexOf granularity-order %) granularity-seq)
        position-of-finest (apply max positions)]
    (get granularity-order position-of-finest)))

(def- date-conversion-specs
  ;; Joda-time patterns are defined at
  ;;  http://www.joda.org/joda-time/key_format.html
  {"%Y" {:pattern "yyyy",
         :granularity :year,
         :desc "year in four digit format"}
   "%m" {:pattern "MM",
         :granularity :month,
         :desc "month (01 to 12)"}
   "%d" {:pattern "dd",
         :granularity :day,
         :desc "day (01 to 31)"}
   "%p" {:pattern "aa",
         :granularity :meridian,
         :desc "AM or PM"}
   "%H" {:pattern "HH",
         :granularity :hour,
         :desc "hour (00 to 23)"}
   "%h" {:pattern "hh",
         :granularity :hour,
         :desc "hour (01 to 12)"}
   "%M" {:pattern "mm",
         :granularity :minute,
         :desc "minute (00 to 59)"}
   "%S" {:pattern "ss",
         :granularity :second,
         :desc "second (00 to 60 -- 60 to allow for occasional leap seconds)"}
   "%z" {:pattern "Z",
         :timezone true,
         :desc (str "The +hhmm or -hhmm numeric timezone"
                    "(that is, the hour and minute offset from UTC)"
                    "Note that there is no granularity associated with timezone."
                    "Note ALSO the :timezone key being set to true!")
         }
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
  (let [pattern-str (s/join "|" (keys date-conversion-specs))
        pattern (re-pattern pattern-str)]
    (re-seq pattern string)))

(defn- compute-granularity [string]
  (let [conv-specs (extract-date-parts string)
        grans (map #(get-in date-conversion-specs [% :granularity]) conv-specs)]
    (get-finest-granularity grans)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Core protocol

(defprotocol DateExprP
  "DateExprP is the basic, all-purpose unit of date-expression. It represents
  a date-dependent string."
  (format-expr [_ time]
    "Generates the formatted DateExpr from the given instant in time.")
  (parse-expr [_ formatted-string]
    "Extracts the approximate instant in time (seconds since the epoch) that was
    used to format a formatted DateExpr.

    Why only the approximate instant in time? Because formatting a timestamp
    may involve some loss in precision! If I format '2014/08/13 at 2 a.m.'
    to a granularity of day, I've lost the information 'at 2 a.m.'

    More generally:
      (format-expr d (parse-expr d formatted)) will get you back formatted
      (parse-expr d (format-expr d time)) will NOT get you back time")
  (timezone [_]
    "Returns the org.joda.time.DateTimeZone of this date expr"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Implementation of protocol
;;
;; DateExpr knows how to operate on the types defined above
;; (EpochTime, Joda DateTime, and number of seconds since the epoch).
;;
;; It formats into UTC timezone.

(defn- empty-string? [x] (= x ""))
(defn- quote-out-pattern
  "joda DateTimeFormats will ignore parts of the pattern
  that are enclosed in single quotes. e.g. if you try to create
  a SimpleDate Format from the string s3://bucket/yyyy/MM/dd, it
  won't let you because the 'b' in bucket is an illegal pattern.

  But, it will happily work on the string 's3://bucket/'yyyy'/'MM'/'dd

  This function quotes-out the portions of the input string that
  are not date-conversion-specs."
  [string]
  (let [giant-or (s/join "|" (keys date-conversion-specs))
        re (re-pattern (str "(" giant-or ")+"))
        ;; Map first across the re-seq because the greedy matches
        ;; will return every capture group that matched... i.e.
        ;; if you have %M%z then the re-seq for that will look like
        ;; [%M%z %z]. We just want the greediest match.
        matches (map first (re-seq re string))
        splits (s/split string re)
        ;; don't replace empty string with "''"... leave it as
        ;; empty string. Why? Because "''" is how you specify an
        ;; escaped single quote! If you replace empty string
        ;; with that, you're adding a single-quote into the pattern
        ;; where there should be nothing.
        quoted-splits (map #(if (empty-string? %)
                              %
                              (str \' % \')) splits)
        interleaved (interleave quoted-splits (concat matches '("")))
        ;; (count splits) will always be one more than (count matches).
        ;; Hence, (interleave splits matches) will drop the last split!
        ;; To include the last split in there, add an empty string to
        ;; matches -- it'll disappear in the join step :)
        ]
    (s/join "" interleaved)))

;; (defn- quote-out-pattern-2
;;   [string]
;;   (let [idx-of-percents

(defn- translate-pattern [string]
  (reduce (fn [to-be-formatted kv]
            (s/replace to-be-formatted (key kv) (:pattern (val kv))))
          string
          date-conversion-specs))

(defn- ->formatter [date-expr-string ^DateTimeZone dtz]
  (let [quoted (quote-out-pattern date-expr-string)
        translated (translate-pattern quoted)]
    (f/formatter translated dtz)))

(defrecord DateExpr [string ^DateTimeZone dtz]
  DateExprP
  (format-expr [_ time]
    (let [f (->formatter string dtz)
          dt (->joda-date-time time)]
      (f/unparse f dt)))
  (parse-expr [_ formatted-string]
    (let [f (->formatter string dtz)
          dt (f/parse f formatted-string)]
      (->seconds-since-epoch dt)))
  (timezone [_]
    dtz))

(def- utc t/utc)
(defn- parse-tz [tz]
  (cond (nil? tz) (do
                    (log/debug "No timezone specified. Using UTC")
                    utc)
        (instance? DateTimeZone tz) tz
        (instance? TimeZone tz) (DateTimeZone/forTimeZone tz)
        (re-matches #"\w+/\w+" (str tz)) (DateTimeZone/forID tz)
        (re-matches #"[+|-]\d{4}" (str tz)) (DateTimeZone/forTimeZone
                                             (TimeZone/getTimeZone
                                              (str "GMT" tz)))
        :else (let [msg (format "Unrecognized timezone specified: %s" tz)]
                (log/error msg)
                (throw (RuntimeException. msg)))))

(defn make-date-expr
  ([string]
     (make-date-expr string nil))
  ([string tz]
     (->DateExpr string (parse-tz tz))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Timezones

(defn- extract-tz-parts [string]
  (filter #(get-in date-conversion-specs [% :timezone])
          (extract-date-parts string)))

(defn has-tz? [string]
  (not (empty? (extract-tz-parts string))))

(defn extract-timezone
  "Given a date-expr-string and a formatted-string, returns the timezone
  of the DateExpr that was used to format the string.

  If the date-expr-string has no timezone conversion specifier, it is an error"
  [date-expr-string formatted-string]
  (when-not (has-tz? date-expr-string)
    (let [msg (format (str "Can't extract-timezone from formatted-string "
                           "(%s) because the date-expr (%s) doesn't have "
                           "a timezone in it!")
                      formatted-string
                      date-expr-string)]
      (log/error msg)
      (throw (RuntimeException. msg))))
  (let [f (->formatter date-expr-string utc)
        f' (.withOffsetParsed f)
        dt (.parseDateTime f' formatted-string)]
    (.getZone dt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  of the date-expr, up to the date specified by f. Returns the seq generated
  by mapping format-expr across the seq of dates.

  s and f each may be:
   - a joda DateTime
   - a java.lang.Long representing the number of seconds since the epoch
   - an EpochTime
   - a formatted date-expr.

  The type of s and the type of f may be different.

  Regarding timezones: it is allowed for s and f to be formatted in
  different timezones. A timezone is an artifact of formatting -- the
  times described by s and f are absolute. Note that, whatever the
  timezones of s and f, the formatted date-seq returned by this function
  will be in the timezone of the date-expr itself!"
  [s f date-expr]
  (let [->timestamp #(if (string? %)
                       (parse-expr date-expr %)
                       %)
        s (->timestamp s)
        f (->timestamp f)
        gran (compute-granularity (:string date-expr))
        period (get granularity=>period gran)
        the-range (date-range s f period)
        formatted-range (map #(format-expr date-expr %) the-range)]
    formatted-range))
