(ns danger-date-expr.core
  "Date expr central"
  {:author "Matt Halverson"
   :date "Wed Aug 13 09:48:42 PDT 2014"}
  (:require [roxxi.utils.print :refer [print-expr]]))

;; # Auxiliary functions

(defn epoch-time-now []
  (quot (System/currentTimeMillis) 1000))

;; # Core protocols

(defprotocol DateExprPartP
  "DateExprPartP is the smallest sub-unit of date-expression. It represents
   a portion of a DateExpr. It's fully described by:
     (1) a constant prefix-string
     (2) a date-pattern (java.text.SimpleDateFormat style)
     (3) a constant postfix-string"
  (format-part [_ timestamp]
    "Generates the formatted DateExprPart. The timestamp should be in
     seconds since the Unix epoch."))

(defprotocol DateExprP
  "DateExprP is the basic, all-purpose unit of date-expression. It represents
  a date-dependent string. It's defined by a seq of DateExprParts which all get
  formatted and joined together to create the full date expression."
  (format-expr [_ timestamp]
    "Generates the formatted DateExpr. The timestamp should be in
     seconds since the Unix epoch."))

;; # Implementations of protocols

(defrecord DateExprPart [pre expr post]
  DateExprPartP
  (format-part [_ timestamp]
    (str pre
         (.format (java.text.SimpleDateFormat. expr) (* 1000 timestamp))
         post)))

(defn make-date-expr-part [pre expr post]
  (->DateExprPart pre expr post))

(defrecord DateExpr [date-expr-parts]
  DateExprP
  (format-expr [_ timestamp]
    (clojure.string/join "" (map #(format-part % timestamp) date-expr-parts))))

(defn make-date-expr [date-expr-parts]
  (->DateExpr date-expr-parts))
