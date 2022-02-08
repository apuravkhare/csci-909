(ns csci-909.interpreter
  (:use [csci-909.term])
  (:use [csci-909.util])
  (:use [csci-909.env]))

(def wrong '(wrong))

(defn wrong? [a] (and (tagged-list? a) (= (first a) 'wrong)))

(defn primitive-procedure?
  [a]
  (and (tagged-list? a)
       (or (= (first a) '+)
           (= (first a) '-)
           (= (first a) '*)
           (= (first a) '/)
           (= (first a) '=)
           (= (first a) 'item)
           (= (first a) 'str)
           (= (first a) 'get))))

(defn overloaded-inst?
  [o env]
  (> (count (filter overload? (lookup-environment* o env))) 0))

(defn find-inst
  [o env]
  (filter prog-inst? (lookup-environment* o env)))

(defn get-inst-type
  [v]
  (cond (const? v)     (const-type v)
        (data-inst? v) (nth v 1)
        :else          wrong))

(defn get-inst-type*
  [vs]
  (map get-inst-type vs))

(defn apply-primitive
  [f args]
  (cond
    (resolve f) (apply (resolve f) args)
    (= f 'item) (if (and (= 2 (count args)) (data-inst? (first args)) (integer? (second args)))
                  (nth (nth (first args) 2) (second args))
                  (throw (Exception. "Invalid input to function 'item'")))))

(defn meaning
  [term env]
  ;; (println (str "meanig " term " | env " (count env)))
  (cond
    (const? term)        term
    (data-inst? term)    term
    (data-fn-inst? term) term
    (variable? term) (lookup-environment term env)
    (lambda? term)   (let
                      [us (to-list (nth term 1))
                       e (nth term 2)]
                       (make-closure e us env))
    (overload? term)     (do
                           (define-variable! (nth term 1) (make-overload (nth term 1)) env)
                           (nth term 1))
    (let? term)      (let
                      [x  (nth term 1)
                       e  (nth term 2)
                       e' (nth term 3)]
                       (meaning e' (extend-environment x (meaning e env) env)))
    (prog-inst? term)   (let
                         [o (nth term 1)
                          e (nth term 3)
                          me (meaning e env)]
                          (if (func? me)
                            (do
                              (overload-variable! o term env)
                              o)
                            wrong))
    (data? term)         (let [id   (nth term 1)
                               args (to-list (nth term 2))]
                           (define-variable! id (make-constructor id args) env)
                           (define-variable! (symbol (str id "?")) (make-inst-predicate id) env)
                           (loop [args' args]
                             (if (empty? args')
                               true
                               (do
                                 (define-variable!
                                   (symbol (str id "-" (first args')))
                                   (make-inst-accessor id (first args')) env)
                                 (recur (rest args')))))
                           id)
    (data-fn? term)      (let [id   (nth term 1)
                               args (to-list (nth term 2))]
                           (if (= 0 (count args))
                           (throw (Exception. "At least one type argument expected for the type " (str id)))
                           (do
                            (define-variable! id (make-fn-constructor id args) env)
                             id)))
    (define? term)       (let [v (nth term 1)
                               e (meaning (nth term 2) env)]
                             (define-variable! v e env)
                             v)
    (if-expr? term)      (let [c (nth term 1)
                               t (nth term 2)
                               f (nth term 3)]
                           (if (meaning c env)
                             (meaning t env)
                             (meaning f env)))
    ;;; everything else is function application
    :else                (let
                             [e  (nth term 0)
                              e's (drop 1 term)
                              mf (meaning e env)]
                              (cond
                                (primitive-procedure? mf) (let [vs (map (fn [e'] (meaning e' env)) e's)]
                                                            (apply-primitive (first mf) vs))
                                (func? mf) (let [args (to-list (nth mf 2))
                                                 exp (nth mf 1)
                                                 vs (map (fn [e'] (meaning e' env)) e's)
                                                 f-env (nth mf 3)]
                                             (meaning exp (extend-environment* args vs f-env)))
                                (data-fn-inst? mf) (let [k (lookup-environment (nth mf 1) env)
                                                         ip (nth k 2)
                                                         l (nth mf 2)
                                                         vs (map (fn [e'] (meaning e' env)) e's)
                                                         vts (get-inst-type* vs)]
                                                     (if (= vts ip)
                                                       (meaning (concat (list l) vs) env)
                                                       (throw (Exception. (str "Incorrect arguments passed to function " (str e))))))
                                (overloaded-inst? e env) (let [vs (map (fn [e'] (meaning e' env)) e's)]
                                                           (loop [insts (find-inst e env)]
                                                              (if (or (= 0 (count insts)) (wrong? (first insts)))
                                                               wrong
                                                               (let [vts (get-inst-type* vs)]
                                                                 (if (= vts (nth (first insts) 2))
                                                                   (meaning (concat (list (nth (first insts) 3)) vs) env)
                                                                   (recur (rest insts)))))))
                                (fn-constructor? mf) (let [id (nth mf 1)
                                                           ip (nth mf 2)
                                                           l (nth term 1)
                                                           us (to-list (nth l 1))]
                                                       (if (lambda? l)
                                                         (if (= (count ip) (count us))
                                                           (make-data-fn-inst id l)
                                                           (throw (Exception. (str "Mismatched arguments passed to " id))))
                                                         (throw (Exception. (str "Function constructor must be initialized with a function " id)))))
                                (constructor? mf) (let [id (nth mf 1)
                                                        args (nth mf 2)]
                                                    (if (= (count e's) (count args))
                                                      (make-data-inst id (zipmap args (map (fn [m] (meaning m env)) e's)))
                                                      (throw (Exception. (str "Mismatched arguments passed to " id)))))
                                (inst-accessor? mf) (let [t (nth mf 1)
                                                          a (nth mf 2)
                                                          vs (map (fn [e'] (meaning e' env)) e's)]
                                                      (if (= 1 (count vs))
                                                        (if (= (get-inst-type (first vs)) t)
                                                          (get (nth (first vs) 2) a)
                                                          (throw (Exception. "Record accessor applied to invalid type.")))
                                                        (throw (Exception. "Record accessor applied to too many arguments."))))
                                (inst-predicate? mf) (let [t (nth mf 1)
                                                           vs (map (fn [e'] (meaning e' env)) e's)]
                                                       (if (= 1 (count vs))
                                                         (= (get-inst-type (first vs)) t)
                                                         (throw (Exception. "Record accessor applied to too many arguments."))))
                                :else (throw (Exception. "No overload found"))))))

(defn init-env [] (let [initial-env (extend-environment* (list '+ '- '* '/ '= 'item 'str 'get)
                                                          (list (list '+) (list '-) (list '*) (list '/) (list '=) (list 'item) (list 'str) (list 'get))
                                                          (global-environment))]
                     initial-env))
