(ns csci-909.term
  (:use [csci-909.util]))

(defn make-app
  [e e']
  (list e e'))

(defn make-constructor
  [k ms]
  (list 'constructor k ms))

(defn make-fn-constructor
  [k ip op]
  (list 'fn-constructor k ip op))

(defn make-prog-inst
  [o t e]
  (list 'inst o t e))

(defn make-data-inst
  [k args]
  (list 'data-inst k args))

(defn make-data-fn-inst
  [k l]
  (list 'data-fn-inst k l))


(defn make-op
  [op args]
  (list 'op op args))

(defn make-overload
  [v]
  (list 'overload v))

(defn make-closure
  [f args env]
  (list 'closure f args env))

; boolean?, integer?, double?, string? are defined in clojure.core

(defn lambda? [a] (and (tagged-list? a) (= (first a) 'lambda)))

(defn let? [a] (and (tagged-list? a) (= (first a) 'let)))

(defn prog-inst? [a] (and (tagged-list? a) (= (first a) 'inst)))

(defn constructor? [a] (and (tagged-list? a) (= (first a) 'constructor)))

(defn fn-constructor? [a] (and (tagged-list? a) (= (first a) 'fn-constructor)))

(defn data-inst? [a] (and (tagged-list? a) (= (first a) 'data-inst)))

(defn data-fn-inst? [a] (and (tagged-list? a) (= (first a) 'data-fn-inst)))

(def variable? symbol?)

(defn func?
  [a]
  (and (tagged-list? a) (= (first a) 'closure)))

(defn const? [a]
  (or (boolean? a)
      (integer? a)
      (double? a)
      (char? a)
      (string? a)))

(defn data? [a] (and (tagged-list? a) (= (first a) 'data)))

(defn data-fn? [a] (and (tagged-list? a) (= (first a) 'data-fn)))

(defn define? [a] (and (tagged-list? a) (= (first a) 'define)))

(defn overload? [a] (and (tagged-list? a) (= (first a) 'overload)))

(defn if-expr? [a] (and (tagged-list? a) (= (first a) 'if)))

(defn const-type
  [v]
  (cond (boolean? v) 'boolean
        (integer? v) 'integer
        (double? v)  'double
        (char? v)    'char
        (string? v)  'string))