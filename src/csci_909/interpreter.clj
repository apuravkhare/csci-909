(ns csci-909.interpreter
  (:use [csci-909.term])
  (:use [csci-909.util]))

(def wrong '(wrong))

(defn wrong? [a] (and (seq? a) (not (empty? a)) (= (first a) 'wrong)))

(defn make-func
  [id f]
  (list 'func id f))

(defn make-data-cons
  [id f]
  (list 'data-cons id f))

(defn func?
  [a]
  (and (seq? a) (not (empty? a)) (= (first a) 'closure)))

(defn data-cons?
  [a]
  (and (seq? a) (not (empty? a)) (= (first a) 'data-cons)))

(defn find-data-constructor
  [env v]
  (filter (fn [e] (= v (nth e 2))) (filter data-cons? env)))

(defn extend-env
  [env u v]
  (reset! env (cons (list [u] [v]) @env))
  env)

(defn extend-env-mult
  [env us vs]
  (if (= (count us) (count vs))
    (do
      (reset! env (cons (list us vs) @env))
      env)
    (throw (Exception. "Mismatching arguments to environment"))))

(defn extend-env-func
  [env f g]
  (fn [v]
    (if (lookup env v)
      (f v)
      (g v))))

(defn extend-env-datacons
  [env f g]
  (fn [v]
    (if (find-data-constructor env v)
      (f v)
      (g v))))

(defn make-closure
  [f args]
  (list 'closure f args))

(defn primitive-procedure?
  [a]
  (and (seq? a) (not (empty? a))
       (or (= (first a) '+)
           (= (first a) '-)
           (= (first a) '*)
           (= (first a) '/)
           (= (first a) 'item)
           (= (first a) 'str))))

(defn define-var
  [v e env]
  (let [keys (first (last @env))
        vals (second (last @env))]
    (reset! env (concat (take (- (count @env) 1) @env) (list (list (cons v keys) (cons e vals)))))))

(defn overloaded-inst?
  [o env]
  (loop [keys (first (last @env))
        vals (second (last @env))]
    (if (empty? keys)
      false
      (if
       (and (= (first keys) o) (overload? (first vals)))
        true
        (recur (rest keys) (rest vals))))))

(defn find-inst
  [o env]
  (let [vals (second (last @env))]
    (filter (fn [val] (and (prog-inst? val) (= o (nth val 1)))) vals)))

(defn get-inst-type
  [vs]
  (map (fn [v] (cond (const? v)     (const-type v)
                     (data-inst? v) (nth v 1)
                     :else          wrong))
       vs))

(defn apply-primitive
  [f args]
  (cond
    (resolve f) (apply (resolve f) args)
    (= f 'item) (if (and (= 2 (count args)) (data-inst? (first args)) (integer? (second args)))
                  (nth (nth (first args) 2) (second args))
                  (throw (Exception. "Invalid input to function 'item'")))))

(defn meaning
  [term env]
  ;; (println "meaning " term)
  (cond
    (const? term)    term
    (data-inst? term)    term
    (variable? term) (lookup @env term)
    (lambda? term)   (let
                      [us (to-list (nth term 1))
                       e (nth term 2)]
                       (make-closure e us))
    (overload? term)     (do
                           (define-var (nth term 1) (make-overload (nth term 1)) env)
                           (define-var (nth term 1) wrong env)
                           (nth term 1))
    (let? term)      (let
                      [x  (nth term 1)
                       e  (nth term 2)
                       e' (nth term 3)]
                       (meaning e' (extend-env env x (meaning e env))))
    (constructor? term) (let
                         [k  (lookup @env (nth term 1))
                          id (nth k 1)
                          args (nth k 2)
                          ms (to-list (nth term 2))]
                          (if (= (count ms) (count args))
                           (make-data-inst id (map (fn [m] (meaning m env)) ms))
                            (throw (Exception. (str "Mismatched arguments passed to " id)))))
    (prog-inst? term)   (let
                         [o (nth term 1)
                          ts (to-list (nth term 2))
                          e (nth term 3)
                          me (meaning e env)]
                          (if (func? me)
                            (do
                              (define-var o term env)
                              term)
                            wrong))
    (data? term)         (let [id   (nth term 1)
                               args (to-list (nth term 2))]
                           (extend-env env id (make-constructor id args))
                           id)
    (define? term)       (let [v (nth term 1)
                               e (meaning (nth term 2) env)]
                             (define-var v e env)
                             e)
    (if-expr? term)      (let [c (nth term 1)
                               t (nth term 2)
                               f (nth term 3)]
                           (if (meaning c env)
                             (meaning t env)
                             (meaning f env)))
    :else                (let
                             [e  (nth term 0)
                              e's (drop 1 term)
                              mf (meaning e env)]
                              (cond
                                (primitive-procedure? mf) (let [vs (map (fn [e'] (meaning e' env)) e's)
                                                                ext-env (extend-env-mult env e's vs)]
                                                            ; (println "args " (map (fn [a] (meaning a ext-env)) e's))
                                                            (apply-primitive (first mf)
                                                                   (map (fn [a] (meaning a ext-env)) e's)))
                                (func? mf) (let [args (to-list (nth mf 2))
                                                 exp (nth mf 1)
                                                 vs (map (fn [e'] (meaning e' env)) e's)]
                                             (meaning exp (extend-env-mult env args vs)))
                                (overloaded-inst? e env) (let [vs (map (fn [e'] (meaning e' env)) e's)]
                                                          ;;  (println (str "vs " (str (first (get-inst-type vs)))))
                                                           (loop [insts (find-inst e env)]
                                                             (if (or (= 0 (count insts)) (wrong? (first insts)))
                                                               wrong
                                                               (let [vts (get-inst-type vs)]
                                                                 (if (= vts (nth (first insts) 2))
                                                                   (meaning (concat (list (nth (first insts) 3)) vs) env)
                                                                   (recur (rest insts)))))))
                                :else (throw (Exception. "No overload found"))))))
    ; :else (throw (Exception. (str "Unknown input: " (str term))))

(defn init-env [] (list (list (list '+ '- '* '/ '= 'item 'str)
                              (list (list '+) (list '-) (list '*) (list '/) (list '=) (list 'item) (list 'str)))))
