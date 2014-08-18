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

;; # Implementations of core protocol

(def date-expr (make-date-expr "s3://bucket/foo/%Y/%m/%d/bar/%H.%M/file-A"))

(deftest format-test
  (testing "We can format a DateExpr"
    (is (= (format-expr date-expr ts)
           "s3://bucket/foo/2014/08/13/bar/17.10/file-A"))
    (testing "even if it starts with a date"
      (is (= (format-expr (make-date-expr "%Y/%m/foo/bar") ts)
             "2014/08/foo/bar")))
    (testing "with a meridian specified!"
      (is (= (format-expr (make-date-expr "s3://bucket/ponies/%Y/%m/%d/%p/foo/%H.%M.%S/bar") ts)
             "s3://bucket/ponies/2014/08/13/PM/foo/17.10.42/bar")))))

(deftest parse-test
  (let [f (format-expr date-expr ts)]
    (testing "We can parse a formatted DateExpr"
      (is (= (parse-expr date-expr f)
             ;; note the loss of the "42 seconds" from ts, because date-expr
             ;; only has minute precision!
             1407949800))
      (is (= (format-expr date-expr (parse-expr date-expr f))
             f)))))

;; # formatted-date-range

(deftest formatted-date-range-test
  (testing "Testing the basic range-functions"
    (let [jdt (->joda-date-time ts)]
      (is (= (formatted-date-range (t/minus jdt (t/years 2)) jdt
                                   (make-date-expr "s3://bucket/foo/%Y"))
             (list
              "s3://bucket/foo/2012"
              "s3://bucket/foo/2013"
              "s3://bucket/foo/2014")))
      (is (= (formatted-date-range (t/minus jdt (t/months 4)) jdt
                                   (make-date-expr "s3://bucket/foo/%Y/%m"))
             (list
              "s3://bucket/foo/2014/04"
              "s3://bucket/foo/2014/05"
              "s3://bucket/foo/2014/06"
              "s3://bucket/foo/2014/07"
              "s3://bucket/foo/2014/08")))
      (is (= (formatted-date-range (t/minus jdt (t/weeks 1)) jdt
                                   (make-date-expr "s3://bucket/foo/%Y/%m/%d/"))
             (list
              "s3://bucket/foo/2014/08/06/"
              "s3://bucket/foo/2014/08/07/"
              "s3://bucket/foo/2014/08/08/"
              "s3://bucket/foo/2014/08/09/"
              "s3://bucket/foo/2014/08/10/"
              "s3://bucket/foo/2014/08/11/"
              "s3://bucket/foo/2014/08/12/"
              "s3://bucket/foo/2014/08/13/")))
      (is (= (formatted-date-range (t/minus jdt (t/hours 13)) jdt
                                   (make-date-expr "s3://bucket/foo/%Y/%m/%d/%p"))
             (list
              "s3://bucket/foo/2014/08/13/AM"
              "s3://bucket/foo/2014/08/13/PM")))
      (is (= (formatted-date-range (t/minus (t/minus jdt (t/hours 3)) (t/minutes 40))
                                   jdt
                                   (make-date-expr "s3://bucket/foo/%Y/%m/%d/bar/%H"))
             (list
              "s3://bucket/foo/2014/08/13/bar/13"
              "s3://bucket/foo/2014/08/13/bar/14"
              "s3://bucket/foo/2014/08/13/bar/15"
              "s3://bucket/foo/2014/08/13/bar/16")))
      (is (= (formatted-date-range (t/minus jdt (t/minutes 5)) jdt
                                   (make-date-expr "s3://bucket/foo/%Y/%m/%d/bar/%H.%M/file-A"))
             (list
              "s3://bucket/foo/2014/08/13/bar/17.05/file-A"
              "s3://bucket/foo/2014/08/13/bar/17.06/file-A"
              "s3://bucket/foo/2014/08/13/bar/17.07/file-A"
              "s3://bucket/foo/2014/08/13/bar/17.08/file-A"
              "s3://bucket/foo/2014/08/13/bar/17.09/file-A"
              "s3://bucket/foo/2014/08/13/bar/17.10/file-A")))))
  (testing "They accept args in all the combinations of types"
    (let [jdt (->joda-date-time ts)
          the-result (list
                      "s3://bucket/foo/2014/08/12/bar"
                      "s3://bucket/foo/2014/08/13/bar")
          date-expr (make-date-expr "s3://bucket/foo/%Y/%m/%d/bar")]
      (is (= (formatted-date-range (t/minus jdt (t/days 1))
                        jdt
                        date-expr)
             the-result))
      (is (= (formatted-date-range (t/minus jdt (t/days 1))
                        (->epoch-time jdt)
                        date-expr)
             the-result))
      (is (= (formatted-date-range (t/minus jdt (t/days 1))
                        (->seconds-since-epoch jdt)
                        date-expr)
             the-result))

      (is (= (formatted-date-range (->epoch-time (t/minus jdt (t/days 1)))
                        jdt
                        date-expr)
             the-result))
      (is (= (formatted-date-range (->epoch-time (t/minus jdt (t/days 1)))
                        (->epoch-time jdt)
                        date-expr)
             the-result))
      (is (= (formatted-date-range (->epoch-time (t/minus jdt (t/days 1)))
                        (->seconds-since-epoch jdt)
                        date-expr)
             the-result))

      (is (= (formatted-date-range (->seconds-since-epoch (t/minus jdt (t/days 1)))
                        jdt
                        date-expr)
             the-result))
      (is (= (formatted-date-range (->seconds-since-epoch (t/minus jdt (t/days 1)))
                        (->epoch-time jdt)
                        date-expr)
             the-result))
      (is (= (formatted-date-range (->seconds-since-epoch (t/minus jdt (t/days 1)))
                        (->seconds-since-epoch jdt)
                        date-expr)
             the-result))))
  (testing "They even accept formatted date-exprs"
    (let [s "s3://bucket/foo/2014/08/12/bar"
          f "s3://bucket/foo/2014/08/14/bar"
          date-expr (make-date-expr "s3://bucket/foo/%Y/%m/%d/bar")]
      (is (= (formatted-date-range s f date-expr)
             (list
              "s3://bucket/foo/2014/08/12/bar"
              "s3://bucket/foo/2014/08/13/bar"
              "s3://bucket/foo/2014/08/14/bar"))))))
