(ns csci-909.term
  (:use [csci-909.util]))

(defn make-app
  [e e']
  (list e e'))

(defn make-lambda
  [a e]
  (list 'lambda a e))

(defn make-constructor
  [k ms]
  (list 'constructor k ms))

(defn make-fn-constructor
  [k ip]
  (list 'fn-constructor k ip))

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

(defn make-inst-accessor
  [t a]
  (list 'inst-accessor t a))

(defn make-inst-predicate
  [t]
  (list 'inst-predicate t))

(defn make-primitive-action
  [p a]
  (list 'primitive-action p a))

(defn make-typeclass-def
  [t a fs]
  (list 'typeclass-def t a fs))

; boolean?, integer?, double?, string? are defined in clojure.core

(defn lambda? [a] (and (tagged-list? a) (= (first a) 'lambda)))

(defn let? [a] (and (tagged-list? a) (= (first a) 'let)))

(defn prog-inst? [a] (and (tagged-list? a) (= (first a) 'inst)))

(defn constructor? [a] (and (tagged-list? a) (= (first a) 'constructor)))

(defn fn-constructor? [a] (and (tagged-list? a) (= (first a) 'fn-constructor)))

(defn data-inst? [a] (and (tagged-list? a) (= (first a) 'data-inst)))

(defn data-fn-inst? [a] (and (tagged-list? a) (= (first a) 'data-fn-inst)))

(defn typeclass? [a] (and (tagged-list? a) (= (first a) 'typeclass)))

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

(defn inst-accessor? [a] (and (tagged-list? a) (= (first a) 'inst-accessor)))

(defn inst-predicate? [a] (and (tagged-list? a) (= (first a) 'inst-predicate)))

(defn primitive-action? [a] (and (tagged-list? a) (= (first a) 'primitive-action)))

(defn typeclass-def? [a] (and (tagged-list? a) (= (first a) 'typeclass-def)))

(defn typeclass-inst? [a] (and (tagged-list? a) (= (first a) 'typeclass-inst)))

(defn const-type
  [v]
  (cond (boolean? v) 'boolean
        (integer? v) 'integer
        (double? v)  'double
        (char? v)    'char
        (string? v)  'string))