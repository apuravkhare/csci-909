(ns csci-909.util
  (:require [clojure.string :as str]))

(defn tagged-list? [a] (and (seq? a) (not (empty? a))))

(defn to-list
  [s]
  (concat s))

(defn in?
  [coll elm]
  (some (fn [c] (= elm c)) coll))

(defn lookup-cont [x env k]
  (if (empty? env)
    (k x)
    (let [pair (first env)
          key  (first pair)
          val  (second pair)]
      (if (= x key)
        val
        (recur x (rest env) k)))))

(defn lookup-logic-var [x env] (lookup-cont x env (fn [x] nil)))

(defn find-first
  [f coll]
  (first (filter f coll)))

(defn find-replace
  [to-find to-replace coll]
  (loop [coll coll
         coll' '()]
    (if (empty? coll)
      (reverse coll')
      (if (= (first coll) to-find)
        (recur (rest coll) (cons to-replace coll'))
        (recur (rest coll) (cons (first coll) coll'))))))

(defn unique [coll]
  (seq (set coll)))