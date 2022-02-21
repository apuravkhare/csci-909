(ns csci-909.unification
  (:use [csci-909.util])
  (:use [csci-909.env])
  (:use clojure.core))

;;; history

(defn extend-history [hList t1 t2] (conj hList (list t1 t2)))


(defn display-history [hList]
  (println "Attempted type check:")
  (loop [L hList]
    (if (empty? L)
      nil
      (do (println (str (first L)))
          (recur (rest L))))))

;;; failure

(defn failure [history message]
  ; (display-history history)
  (throw (Exception. message)))

;;; logic variables

(defn logic-variable? [a]
  (and (symbol? a)
       (= (first (str a)) \?)))

;;; substitution code

(def theta-identity '())

(defn applyUnifier [theta x]
  (let [term (lookup-logic-var x theta)]
    (cond (= term nil) x
          (logic-variable? term) (recur theta term)
          :else term)))


(defn occurs-in? [x term theta] ; x is canonical
  (cond (seq? term) (some (fn [t] (occurs-in? x t theta)) term)
        (logic-variable? term)
        (let [y (applyUnifier theta term)] (= x y))
        :else false))

(defn extendUnifier [theta x term history]
  (if (occurs-in? x term theta) ; do occurs check
    (failure history (str "Circular equation " (str (list x term))))
    (conj theta (list x term))))


;;; unification

(declare unifyTerm4)

(defn unifyList [L1 L2 theta history]
  (cond (and (empty? L1) (empty? L2)) theta
        (empty? L1) (failure history "Type check failed!") ; fail
        (empty? L2) (failure history "Type check failed!") ; fail
        :else (let [term1 (first L1)
                    term2 (first L2)
                    rest1 (rest L1)
                    rest2 (rest L2)
                    theta2 (unifyTerm4 term1 term2 theta (extend-history history term1 term2))]
                (recur rest1 rest2 theta2 (extend-history history rest1 rest2)))))


(defn unifyVar [x term theta history]
  (let [xt (applyUnifier theta x)]
    (if (logic-variable? xt)
      (if (logic-variable? term)
        (let [y (applyUnifier theta term)]
          (if (= xt y)
            theta
            (extendUnifier theta xt y history)))
        (extendUnifier theta xt term history))
      (unifyTerm4 xt term theta (extend-history history xt term)))))


(defn constant? [a]
  (or (number? a)
      (string? a)
      (symbol? a) ; careful to place after logic-variable check
      (= a true)
      (= a false)))

(defn unifyTerm4 [term1 term2 theta history]
  (cond (logic-variable? term1) (unifyVar term1 term2 theta history)
        (logic-variable? term2) (unifyVar term2 term1 theta history)
        (and (constant? term1) (constant? term2))
        (if (= term1 term2)
          theta
          (failure history "Type check failed!")) ; fail
        (and (seq? term1) (seq? term2)) (unifyList term1 term2 theta history)
        :else (do (println (str "Terms " term1 " " term2)) (failure history "Type check failed!")))) ; fail

(defn unifyTerm5 [term1 term2 theta history constraints type-tc-map]
  (if (empty? constraints)
    (unifyTerm4 term1 term2 theta history)
    (let [matches (lookup-environment* term2 type-tc-map)
          matched-constraint (find-first (fn [c] (= (second c) term1)) constraints)]
      (println (pr-str "term1 " term1 " term2 " term2))
      (println (pr-str "matches " matches " matched-constraint " matched-constraint))
      (if matched-constraint
        (if
         (in? matches (first matched-constraint))
          theta
          (failure history "Type check failed!"))
        (unifyTerm4 term1 term2 theta history))))) ; fail

(defn unifyTerm3 [term1 term2 theta]
  (unifyTerm4 term1 term2 theta (extend-history '() term1 term2)))

(defn unifyTerm2 [term1 term2]
  (unifyTerm3 term1 term2 theta-identity))
