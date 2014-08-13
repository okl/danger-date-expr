(ns danger-date-expr.core-test
  "Tests for danger-date-expr.core"
  {:author "Matt Halverson"
   :date "Wed Aug 13 09:48:42 PDT 2014"}
  (:require [roxxi.utils.print :refer [print-expr]])
  (:require [clojure.test :refer :all])
  (:require [danger-date-expr.core :refer :all]
            [clj-time.core :as t]))

(def ts 1407949842) ;; 2014-08-13 at 17:10:42.000 UTC

;; # Coercion functions

(deftest coercion-test
  (let [jdt (->joda-date-time ts)
        et (->epoch-time ts)]
    (testing "Coercion functions are correct"
      (is (= (t/date-time 2014 8 13 17 10 42 0) (->joda-date-time ts)))
      (is (= (make-epoch-time 1407949842) (->epoch-time ts)))
      (is (= ts (->seconds-since-epoch ts))))

    (testing "Coercion functions are transitive (?)"
      (is (= jdt (->joda-date-time (->joda-date-time ts))))
      (is (= et (->epoch-time (->joda-date-time ts))))
      (is (= ts (->seconds-since-epoch (->joda-date-time ts))))

      (is (= jdt (->joda-date-time (->epoch-time ts))))
      (is (= et (->epoch-time (->epoch-time ts))))
      (is (= ts (->seconds-since-epoch (->epoch-time ts))))

      (testing "even if we go through another coercion step"
        (testing "(beginning with jdt)"
          (is (= jdt (->joda-date-time (->joda-date-time jdt))))
          (is (= et (->epoch-time (->joda-date-time jdt))))
          (is (= ts (->seconds-since-epoch (->joda-date-time jdt))))

          (is (= jdt (->joda-date-time (->epoch-time jdt))))
          (is (= et (->epoch-time (->epoch-time jdt))))
          (is (= ts (->seconds-since-epoch (->epoch-time jdt)))))
        (testing "(beginning with et)"
          (is (= jdt (->joda-date-time (->joda-date-time et))))
          (is (= et (->epoch-time (->joda-date-time et))))
          (is (= ts (->seconds-since-epoch (->joda-date-time et))))

          (is (= jdt (->joda-date-time (->epoch-time et))))
          (is (= et (->epoch-time (->epoch-time et))))
          (is (= ts (->seconds-since-epoch (->epoch-time et)))))))))

;; # Implementations of core protocols

(deftest date-expr-part-test
  (testing "We can format a DateExprPart"
    (let [part (make-date-expr-part "yodaetl/" "yyyy/MM/dd" "/path/to/logfile")]
      (is (= (format-part part ts)
             "yodaetl/2014/08/13/path/to/logfile")))))

(def parts
  (vector (make-date-expr-part "s3://okl-danger-stg/humperdink/" "yyyy/MM/dd" "/stg")
          (make-date-expr-part "/route-66/" "HH.mm" "/file-A")))
(def date-expr (make-date-expr parts))

(deftest date-expr-test
  (testing "We can format a DateExpr"
    (is (= (format-expr date-expr ts)
           "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.10/file-A"))))

;; # formatted-date-range

(deftest formatted-date-range-test
  (testing "Testing the basic range-functions"
    (let [jdt (->joda-date-time ts)]
      (is (= (year-range (t/minus jdt (t/years 2)) jdt date-expr)
             (list
              "s3://okl-danger-stg/humperdink/2012/08/13/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2013/08/13/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.10/file-A")))
      (is (= (month-range (t/minus jdt (t/months 4)) jdt date-expr)
             (list
              "s3://okl-danger-stg/humperdink/2014/04/13/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/05/13/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/06/13/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/07/13/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.10/file-A")))
      (is (= (week-range (t/minus jdt (t/months 1)) jdt date-expr)
             (list
              "s3://okl-danger-stg/humperdink/2014/07/13/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/07/20/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/07/27/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/03/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/10/stg/route-66/17.10/file-A")))
      (is (= (day-range (t/minus jdt (t/weeks 1)) jdt date-expr)
             (list
              "s3://okl-danger-stg/humperdink/2014/08/06/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/07/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/08/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/09/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/10/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/11/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/12/stg/route-66/17.10/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.10/file-A")))
      (is (= (hour-range (t/minus (t/minus jdt (t/hours 3)) (t/minutes 40))
                         jdt date-expr)
             (list
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/13.30/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/14.30/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/15.30/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/16.30/file-A")))
      (is (= (minute-range (t/minus jdt (t/minutes 5)) jdt date-expr)
             (list
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.05/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.06/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.07/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.08/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.09/file-A"
              "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.10/file-A")))))
  (testing "They accept args in all the combinations of types"
    (let [jdt (->joda-date-time ts)
          the-result (list
                      "s3://okl-danger-stg/humperdink/2014/08/12/stg/route-66/17.10/file-A"
                      "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/17.10/file-A")]
      (is (= (day-range (t/minus jdt (t/days 1))
                        jdt
                        date-expr)
             the-result))
      (is (= (day-range (t/minus jdt (t/days 1))
                        (->epoch-time jdt)
                        date-expr)
             the-result))
      (is (= (day-range (t/minus jdt (t/days 1))
                        (->seconds-since-epoch jdt)
                        date-expr)
             the-result))

      (is (= (day-range (->epoch-time (t/minus jdt (t/days 1)))
                        jdt
                        date-expr)
             the-result))
      (is (= (day-range (->epoch-time (t/minus jdt (t/days 1)))
                        (->epoch-time jdt)
                        date-expr)
             the-result))
      (is (= (day-range (->epoch-time (t/minus jdt (t/days 1)))
                        (->seconds-since-epoch jdt)
                        date-expr)
             the-result))

      (is (= (day-range (->seconds-since-epoch (t/minus jdt (t/days 1)))
                        jdt
                        date-expr)
             the-result))
      (is (= (day-range (->seconds-since-epoch (t/minus jdt (t/days 1)))
                        (->epoch-time jdt)
                        date-expr)
             the-result))
      (is (= (day-range (->seconds-since-epoch (t/minus jdt (t/days 1)))
                        (->seconds-since-epoch jdt)
                        date-expr)
             the-result)))))
