(ns csci-909.transformer2
  (:use [csci-909.util])
  (:use [csci-909.term])
  (:use [csci-909.env])
  (:use [clojure.set])
  (:use [csci-909.primitive]))

(defn gen-inst-name
  [tc-name t-name f-name]
  (symbol (str "*" tc-name "-" t-name "-" f-name)))


(def wrong '(wrong))

(defn wrong? [a] (and (tagged-list? a) (= (first a) 'wrong)))

(defn overloaded-inst?
  [o env]
  (> (count (filter overload? (lookup-environment* o env))) 0))

(defn find-inst
  [o env]
  (filter prog-inst? (lookup-environment* o env)))

(defn lookup-type
  [t env]
  (cond (primitive-type? t) t
        :else (let [t' (lookup-environment t env)]
                (if (constructor? t')
                  (nth t' 1)
                  (throw (Exception. (str "Data type not found " t)))))))

(defn create-overloaded-fn
  [f tc t env]
  (let [tc-var (nth tc 2)
        fs (nth tc 3)
        f-defs (filter (fn [f'] (= (first f') (first f))) fs)]
    (if (and (= 1 (count f-defs)) (overloaded-inst? (first f) env))
      (let [f-def (first f-defs)
            args (map (fn [a] (if (= tc-var a) t a)) (nth f-def 1))]
        (make-prog-inst (first f) args (make-lambda (nth f 1) (nth f 2))))
      (throw (Exception. (str "No suitable function found to overload " (str f)))))))

(defn meaning
  [term env]
  ;; (println "") (println (pr-str "meanig " term " | env " (environment-keys env)))
  (cond
    (const? term)        [term, term]
    (data-inst? term)    [term, term]
    (data-fn-inst? term) [term, term]
    (variable? term) [(lookup-environment term env), term]
    (decl? term)     [term, term]
    (lambda? term)   (let
                      [us (to-list (nth term 1))
                       e (nth term 2)]
                       [(make-closure e us env), term])
    (overload? term)     (do
                           (define-variable! (nth term 1) (make-overload (nth term 1)) env)
                           [(nth term 1), term])
    (let? term)      (let
                      [x  (nth term 1)
                       e  (nth term 2)
                       e' (nth term 3)]
                       [(first (meaning e' (extend-environment x (first (meaning e env)) env))), term])
    (prog-inst? term)   (let
                         [o (nth term 1)
                          e (nth term 3)
                          me (first (meaning e env))]
                          (if (func? me)
                            (do
                              (overload-variable! o term env)
                              [o, term])
                            [wrong, term]))
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
                           [id, term])
    (data-fn? term)      (let [id   (nth term 1)
                               args (to-list (nth term 2))]
                           (if (= 0 (count args))
                             (throw (Exception. "At least one type argument expected for the type " (str id)))
                             (do
                               (define-variable! id (make-fn-constructor id args) env)
                               [id, term])))
    (define? term)       (let [v (nth term 1)
                               e (first (meaning (nth term 2) env))]
                           (define-variable! v e env)
                           [v, (make-define v e)])
    (if-expr? term)      (let [c (nth term 1)
                               t (nth term 2)
                               f (nth term 3)]
                           (if (first (meaning c env))
                             [(first (meaning t env)), term]
                             [(first (meaning f env)), term]))
    (typeclass? term)    (if (= 0 (count (filter typeclass-def? (lookup-environment* (nth term 1) env))))
                           (do
                             (define-variable! (nth term 1) (make-typeclass-def (nth term 1) (nth term 2) (drop 3 term)) env)
                             (loop [fs (drop 3 term)]
                               (define-variable! (first (first fs)) (make-overload (first (first fs))) env)
                               (or (empty? (rest fs)) (recur (rest fs))))
                             [(nth term 1), term])
                           (throw (Exception. (str "A type class with the same name already exists. " (nth term 1)))))
    (typeclass-inst? term) (let [tc (lookup-environment (nth term 1) env)
                                 t  (lookup-type (nth term 2) env)
                                 fs (drop 3 term)]
                             (loop [fs fs]
                               (meaning (create-overloaded-fn (first fs) tc t env) env)
                               (or (empty? (rest fs)) (recur (rest fs))))
                             (nth term 2))
    ;;; everything else is function application
    :else                (let
                          [e  (nth term 0)
                           e's (drop 1 term)
                           mf (meaning e env)]
                           (cond
                             (primitive-procedure? mf) (let [a (nth mf 2)
                                                             vs (map (fn [e'] (meaning e' env)) e's)]
                                                         (a vs))
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
                                                            ; wrong
                                                            (throw (Exception. (str "No overload found with the given arguments for " (str e))))
                                                            (let [vts (get-inst-type* vs)]
                                                              (if (= vts (take (- (count (nth (first insts) 2)) 1) (nth (first insts) 2)))
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
                             :else (throw (Exception. (str "Function not found " (if (seq? term) (first term) nil))))))))

(defn init-env [] (let [init-env (global-environment)] (extend-environment* primitives primitive-actions init-env)))
