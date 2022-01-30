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
  (and (seq? a) (not (empty? a)) (= (first a) 'func)))

(defn data-cons?
  [a]
  (and (seq? a) (not (empty? a)) (= (first a) 'data-cons)))

(defn find-data-constructor
  [env v]
  (filter (fn [e] (= v (nth e 2))) (filter data-cons? env)))

(defn extend-env
  [env u v]
  (conj env (list v u)))

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

(defn meaning
  [term env]
  (cond
    (boolean? term)  (lookup env 'boolean)
    (integer? term)  (lookup env 'integer)
    (double? term)   (lookup env 'double)
    (char? term)     (lookup env 'char)
    (variable? term) (lookup env term)
    (op? term)       (lookup env (second term))
    (lambda? term)   (let
                      [u (nth term 1)
                       e (nth term 2)
                       v (lookup env u)]
                       (make-lambda v (meaning e (extend-env env u v))))
    (app? term)      (let 
                      [e  (nth term 1)
                       e' (nth term 2)
                       mf (meaning e env)]
                       (if (func? mf)
                         (make-app mf (meaning e' env))
                         wrong))
    (let? term)      (let
                      [x  (nth term 1)
                       e  (nth term 2)
                       e' (nth term 3)]
                       (meaning e' (extend-env env x (meaning e env))))
    (constructor? term) (let
                         [k  (nth term 1)
                          ms (nth term 2)]
                          (make-constructor k (map (fn [m] (meaning m env) ms)) (nth term 3)))
    (prog-inst? term)   (let
                         [o (nth term 1)
                          t (nth term 2)
                          e (nth term 3)
                          p (nth term 4)
                          me (meaning e env)]
                          (if (func? me)
                            (if (func? t)
                              (meaning p (extend-env-func env (meaning e env) (lookup env o)))
                              (meaning p (extend-env-datacons env (meaning e env) (lookup env o))))
                            wrong))
    :else (throw (Exception. (str "Unknown input: " (str term))))))

(defn init-env
  "env is a map of uniq-var -> type and overloaded-var -> (type -> type)"
  []
  (list
   (make-data-cons 'boolean boolean)
   (make-data-cons 'int int)
   (make-data-cons 'double double)
   (make-data-cons 'char char)))

