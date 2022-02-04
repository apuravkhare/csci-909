(ns csci-909.util
  (:require [clojure.string :as str]))

(defn tagged-list? [a] (and (seq? a) (not (empty? a))))

(defn to-list
  [s]
  (concat s))