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

(defn lookup-gamma [gamma x]
  (lookup-cont x gamma (fn [x] (throw (Exception. (str "Undeclared name: " (str x)))))))

(defn lookup-gamma-all [gamma1 gamma2 gamma3 x]
  (lookup-cont x
               gamma3
               (fn [x] (lookup-cont x
                                    gamma2
                                    (fn [x] (lookup-cont x
                                                         gamma1
                                                         (fn [x] (throw (Exception. (str "Undeclared variable: " (str x)))))))))))

(defn rename-lookup [x env] (lookup-cont x env (fn [x] x)))