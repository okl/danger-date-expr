# date-expr

A Clojure library for working with date-expressions.

General functionality provided:

 * Given a date-expr pattern-string and an optional timezone,
   produce a date-expr!
 * Given a date-expr and a timestamp, compute the formatted date
 * Given a date-expr and a formatted date, parse the formatted date
   to back-compute the corresponding timestamp
 * Given a date-expr, a start time, and a stop time, compute all
   the formatted dates in between start and stop time (spaced apart by
   the finest granularity of time-period in the date expr)

Functionality around timezones:

 * Given a date-expr pattern-string, determine if it has any
   timezone conversion-specs in it
 * Given a date-expr pattern-string (that you are certain has a timezone in it)
   and a formatted date, compute the timezone of the formatted date

Functionality around shortening date-expr pattern-strings:

 * Given a date-expr pattern-string, make it "coarser" by one level of
   granularity, by removing everything before the finest-grained
   conversion-spec.
 * Given a date-expr pattern-string, make it "dateless" by removing
   everything before the first conversion-spec.

Notes:

 * Precisions are down to the second.
 * Date-expr timezone defaults to UTC if unspecified
 * Accepted timestamp formats are:
    * java.lang.Long, representing seconds since the Unix epoch
    * EpochTime, defined inside this library
    * org.joda.time.DateTime
 * Accepted timezone formats are:
    * nil (will be interpreted as utc)
    * org.joda.time.DateTimeZone
    * java.util.TimeZone
    * long form of timezone id, e.g. "America/Los_Angeles"
    * hours and minutes offset from UTC, e.g. "-0800" or "+0530"

## Terminology

 * **date-expr or date-expression**: the Clojure type representing a
   date expression
 * **date-expr pattern-string**: string that describes the format of a
   date-expr... e.g. "s3://bucket/%Y-%m-%d/foo"
 * **formatted date**: what you get when you format a date-expr using
   a particular timestamp
 * **conversion-spec or conversion specification**: the "active"
   portions of a date-expr pattern-string, which encode for time and
   get replaced when formatted. e.g. %Y is the conversion-spec for
   "four-digit year"

## Usage

Sample usage at a repl:

    > (require '[clj-time.core :as t])
    > (def ts (t/now))
    > ts
      ;; => #<DateTime 2014-08-15T15:55:15.829Z>
    > (def de (make-date-expr "s3://bucket/foo/%Y/%m/%d/bar/%H.%M/file-A"))
    > (format-expr de ts)
      ;; => "s3://bucket/foo/2014/08/15/bar/15.55/file-A"
    > (parse-expr de "s3://bucket/foo/2014/08/15/bar/15.55/file-A")
      ;; => 1408118100
    > (quot (clj-time.coerce/to-long ts) 1000)
      ;; => 1408118115 which is not quite equal to the return value of
      ;;    parse-expr... loss of precision in action!
    > (formatted-date-range (t/minus ts (t/minutes 3)) ts de)
      ;; => ("s3://bucket/foo/2014/08/15/bar/15.52/file-A"
      ;;     "s3://bucket/foo/2014/08/15/bar/15.53/file-A"
      ;;     "s3://bucket/foo/2014/08/15/bar/15.54/file-A"
      ;;     "s3://bucket/foo/2014/08/15/bar/15.55/file-A")
    > (formatted-date-range "s3://bucket/foo/2014/08/15/bar/15.52/file-A"
                            "s3://bucket/foo/2014/08/15/bar/15.55/file-A"
                            de)
      ;; => ("s3://bucket/foo/2014/08/15/bar/15.52/file-A"
      ;;     "s3://bucket/foo/2014/08/15/bar/15.53/file-A"
      ;;     "s3://bucket/foo/2014/08/15/bar/15.54/file-A"
      ;;     "s3://bucket/foo/2014/08/15/bar/15.55/file-A")
      ;; same result as the other way!

Allowable date-exprs are defined by the `date-conversion-specs` map in
the source code. It's a subset of the POSIX conversion
specifications[1]. All conversion-specs are two characters long,
beginning with a percent sign `%`. Currently (as of 2014-08-15)
escapes are not implemented, meaning that date-exprs cannot contain
the literal sequence `%Y`... that would always get interpreted to be
the year. Escapes may be implemented someday -- today is not that
day.

[1] http://linux.die.net/man/3/strftime

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
