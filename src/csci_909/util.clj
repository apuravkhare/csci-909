(ns csci-909.util
  (:require [clojure.string :as str]))

;; (defn lookup
;;   [pairs key]
;;   (loop [ps pairs]
;;     (if (empty? ps)
;;       (throw (Exception. (str "Element not found: " key)))
;;       ; nil
;;       (if (= (first (first ps)) key)
;;         (second (first ps))
;;         (recur (rest ps))))))

(defn lookup
  [pairs key]
  (if (empty? pairs)
    (throw (Exception. (str "Unbound symbol: " key)))
    (loop [tbl (first pairs)
           keys (first tbl)
           vals (second tbl)]
      (if (empty? keys)
        (lookup (rest pairs) key)
        (if (= (first keys) key)
         (first vals)
         (recur tbl (rest keys) (rest vals)))))))

(defn to-list
  [s]
  (concat s))