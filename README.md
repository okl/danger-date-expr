# date-expr

A Clojure library for working with date-expressions.

Functionality provided:

 * Given a date-expression and a timestamp, compute the formatted
   date-expr string
 * Given a date-expression and a formatted date-expr string, compute
   the timestamp that the date-expression represents
 * Given a date-expression, a start time, and a stop time, compute all
   the formatted date-expr strings in between start and stop time
   (spaced apart by the finest granularity of time-period in the date
   expression)

Notes:

 * Precisions are down to the nearest second.
 * Formats are in UTC.
 * Accepted timestamp formats are:
    * java.lang.Long, representing seconds since the Unix epoch
    * EpochTime, defined inside this library
    * org.joda.time.DateTime

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
