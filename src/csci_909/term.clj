(ns csci-909.term
  (:require [blancas.kern.core :refer :all])
  (:require [blancas.kern.lexer.basic]))

(defn make-lambda
  [u e]
  (list 'lambda u e))

(defn make-app
  [e e']
  (list 'app e e'))

(defn make-let
  [x e e']
  (list 'let x e e'))

(defn make-constructor
  [k ms e]
  (list 'cons k ms e))

(defn make-prog-inst
  [o t e]
  (list 'inst o t e))

(defn make-op
  [op args]
  (list 'op op args))

; boolean?, integer?, double?, string? are defined in clojure.core

(defn op? [a] (and (seq? a) (not (empty? a)) (= (first a) 'op)))

(defn app? [a] (and (seq? a) (not (empty? a)) (= (first a) 'app)))

(defn if? [a] (and (seq? a) (not (empty? a)) (= (first a) 'if)))

(defn lambda? [a] (and (seq? a) (not (empty? a)) (= (first a) 'lambda)))

(defn let? [a] (and (seq? a) (not (empty? a)) (= (first a) 'let)))

(defn prog-inst? [a] (and (seq? a) (not (empty? a)) (= (first a) 'inst)))

(defn constructor? [a] (and (seq? a) (not (empty? a)) (= (first a) 'cons)))

; (defn variable? [a] (and (seq? a) (and (not (empty? a)) (symbol? a))))
(def variable? symbol?)