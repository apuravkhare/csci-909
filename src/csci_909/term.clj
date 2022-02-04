(ns csci-909.term
  (:require [blancas.kern.core :refer :all])
  (:require [blancas.kern.lexer.basic])
  (:use [csci-909.util]))

(defn make-lambda
  [u e]
  (list 'lambda u e))

(defn make-app
  [e e']
  (list e e'))

(defn make-let
  [x e e']
  (list 'let x e e'))

(defn make-constructor
  [k ms]
  (list 'constructor k ms))

(defn make-prog-inst
  [o t e]
  (list 'inst o t e))

(defn make-data-inst
  [k args]
  (list 'data-inst k args))

(defn make-op
  [op args]
  (list 'op op args))

(defn make-overload
  [v]
  (list 'overload v))

; boolean?, integer?, double?, string? are defined in clojure.core

(defn op? [a] (and (tagged-list? a) (= (first a) 'op)))

(defn app? [a] (and (seq? a) (= (count a) 2)))

(defn if? [a] (and (tagged-list? a) (= (first a) 'if)))

(defn lambda? [a] (and (tagged-list? a) (= (first a) 'lambda)))

(defn let? [a] (and (tagged-list? a) (= (first a) 'let)))

(defn prog-inst? [a] (and (tagged-list? a) (= (first a) 'inst)))

(defn constructor? [a] (and (tagged-list? a) (= (first a) 'constructor)))

(defn data-inst? [a] (and (tagged-list? a) (= (first a) 'data-inst)))

; (defn variable? [a] (and (seq? a) (and (not (empty? a)) (symbol? a))))
(def variable? symbol?)

(defn const? [a]
  (or (boolean? a)
      (integer? a)
      (double? a)
      (char? a)
      (string? a)))

(defn data? [a] (and (tagged-list? a) (= (first a) 'data)))

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