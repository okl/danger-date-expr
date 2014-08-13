(ns danger-date-expr.core-test
  "Tests for danger-date-expr.core"
  {:author "Matt Halverson"
   :date "Wed Aug 13 09:48:42 PDT 2014"}
  (:require [roxxi.utils.print :refer [print-expr]])
  (:require [clojure.test :refer :all]
            [danger-date-expr.core :refer [make-date-expr-part
                                           make-date-expr
                                           format-part
                                           format-expr]]))

(def ts 1407949842) ;; August 13

(deftest date-expr-part-test
  (testing "We can format a DateExprPart"
    (let [part (make-date-expr-part "yodaetl/" "yyyy/MM/dd" "/path/to/logfile")]
      (is (= (format-part part ts)
             "yodaetl/2014/08/13/path/to/logfile")))))

(deftest date-expr-test
  (testing "We can format a DateExpr"
    (let [parts (vector (make-date-expr-part "s3://okl-danger-stg/humperdink/" "yyyy/MM/dd" "/stg")
                        (make-date-expr-part "/route-66/" "HH.mm" "/file-A"))
          expr (make-date-expr parts)]
      (is (= (format-expr expr ts)
             "s3://okl-danger-stg/humperdink/2014/08/13/stg/route-66/10.10/file-A")))))
