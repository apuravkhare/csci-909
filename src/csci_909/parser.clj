(ns csci-909.parser
  (:use [csci-909.term]))

(defn parse
  [arg]
  ;; (make-constructor (nth arg 1) (nth arg 2) (nth arg 3))
  (if (seq? arg)
     (cond
       (= '+ (first arg)) (make-op '+ (map parse (rest arg)))
       (= '- (first arg)) (make-op '- (map parse (rest arg)))
       (= '* (first arg)) (make-op '* (map parse (rest arg)))
       (= '/ (first arg)) (make-op '/ (map parse (rest arg)))
       (= 'data (first arg)) (let [ps (make-constructor (nth arg 1) (nth arg 2) (nth arg 3))]
                               (println (str "Parsed: " (str ps)))
                               ps)
       (= 'inst (first arg)) (make-prog-inst (nth arg 1) (nth arg 2) (nth arg 3))
       ; (= 'fn (first arg)) (make-lambda)
       :else (throw (Exception. (str "Unknown form: " (str arg)))))
     (list arg)))
