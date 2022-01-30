(ns csci-909.parser
  (:use [csci-909.term]))

(defn parse
  [arg]
  (if (seq? arg)
    (cond
      (= + (first arg)) (make-op + (map parse (rest arg)))
      (= - (first arg)) (make-op - (map parse (rest arg)))
      (= * (first arg)) (make-op * (map parse (rest arg)))
      (= / (first arg)) (make-op / (map parse (rest arg)))
      (= 'data (first arg)) (make-constructor (nth arg 1) (nth arg 2) (nth arg 3))
      (= 'inst (first arg)) (make-let ))
    arg))