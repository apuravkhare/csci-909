(ns csci-909.checker
  (:use [csci-909.util])
  (:use clojure.set)
  (:use [csci-909.term])
  (:use [csci-909.unification :only (unifyTerm4 failure extend-history theta-identity logic-variable? applyUnifier)]))

;;; universalize functions

(defn free-variables-from-type [t] ; return a set of type variables
  (cond (const-type t) #{}
        (variable? t) (set (list t))
        ;; (tuple-type? t)
        ;; (reduce union #{} (map free-variables-from-type (arg1 t)))
        (vector? t)
        (union (set (map free-variables-from-type t)))
        ;; (named-type? t) (free-variables-from-type (arg2 t))
        :else (throw (Exception. (str "Unexpected type:  " (str t))))))


(defn universalize-type [t]
  (let [ids (free-variables-from-type t)]
    (make-forall-type (into '() ids) t)))

;;; generate constructor types

; types-from-data goes here
; ak
(defn types-from-data
  ""
  [l1]
  (loop [rem-l1 l1
         acc '()]
    (if (empty? rem-l1)
      acc
      (recur (rest rem-l1)
       (let [[i xs cs] (first l1)]
        (reduce concat acc
                (map (fn [c] (if (subset? (arg2 c) (xs))
                               (list (arg1 c) (universalize-type (arg2 c)))
                               (throw (Exception. (str "Invalid arguments for constructor " (arg1 c))))))
                     cs)))))))


;;; built-in/primitive type environment

(def init-con-env
  '((*Cons* (forall (a) (ArrowType (TupleType (a (NamedType *List* a))) (NamedType *List* a))))))

(def init-type-env
  '((*prim+i (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType))))
    (*prim-i (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType))))
    (*prim*i (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType))))
    (*primdivi (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType))))
    (*prim=i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
    (*prim<i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
    (*prim>i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
    (*prim<=i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
    (*prim>=i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
    (*prim+d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))))
    (*prim-d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))))
    (*prim*d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))))
    (*primdivd (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))))
    (*prim<d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
    (*prim>d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
    (*prim=d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
    (*prim<=d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
    (*prim>=d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
    (*prim+str (forall () (ArrowType (TupleType ((StringType) (StringType))) (StringType))))
    (*prim=str (forall () (ArrowType (TupleType ((StringType) (StringType))) (BoolType))))
    (*prim-and (forall () (ArrowType (TupleType ((BoolType) (BoolType))) (BoolType))))
    (*prim-or (forall () (ArrowType (TupleType ((BoolType) (BoolType))) (BoolType))))))



(def empty-dec-env '())

;;; transformation functions

(defn gen-formal [] (gensym "x*"))

(defn gen-formals [n]
  (if (= n 0)
    '()
    (cons (gen-formal) (gen-formals (- n 1)))))


;;; process defn and transform to regular function definition

; process-funs goes here
; ak
; flat list of functions -> find all functions with the same name
; -> check args across all -> make case expressions of args to exps (case def has fresh args)
; -> make lambda1 from right to left (case expression is rightmost)
; -> wrap in function

;;; reduce forms --- eliminate if, let, lambda, and, or 


; reduce-forms goes here

;;; rename local variables

(def empty-rename-env '())

(defn extend-rename-env [env x y] (conj env (list x y)))

(defn extend-rename-env* [env xs ys] ; assumes xs and ys are same length
  (if (empty? xs)
    env
    (recur (extend-rename-env env (first xs) (first ys)) (rest xs) (rest ys))))

(defn gen-rename [var-name] (gensym (str (str var-name) "*")))

; rename-locals goes here
; ak
(defn rename-locals-with-context [e env gen-new]
  (cond (const? e) e
        (variable? e) (if gen-new
                        (let [existing (rename-lookup e (deref env))]
                          (if (= existing e)
                            (let [rn (gen-rename e)]
                              (reset! env (extend-rename-env (deref env) e rn))
                              rn)
                            existing))
                        (rename-lookup e (deref env)))
        (if-expr? e) (make-if-expr (rename-locals-with-context (arg1 e) env false) (rename-locals-with-context (arg2 e) env false) (rename-locals-with-context (arg3 e) env false))
        (lambda? e) (make-lambda
                      (map (fn [a] (rename-locals-with-context a env true)) (arg1 e))
                      (rename-locals-with-context (arg2 e) env false))
        (lambda1? e) (make-lambda1
                      (rename-locals-with-context (arg1 e) env true)
                      (rename-locals-with-context (arg2 e) env false))
        ; (call? e) (make-call (arg1 e) (rename-locals-with-context (arg2 e) env false))
        ; (call-pat? e) (make-call-pat (arg1 e) (rename-locals-with-context (arg2 e) env false))
        ; :else e))
        ; everything else is function application
        :else e))


(defn rename-locals [e] (rename-locals-with-context e (atom empty-rename-env) false))

;;; transform declaration tree to simplified five tuple

; split-decl goes here
; ak
; l1 is collected
; l2 comes from l1
; l3 is (init-con-env) and (init-type-env) with universalize called on it
; l4 is derived from type declarations with universalize called
; l5 starts by collecting all functions, then process-funs and simplify-exp is called once everything is collected
; 04/04/20 - bug - global variables/functions inside a function are also renamed.
(defn split-decl
  ""
  [tree]
  (loop [children tree
         l1 '()
         l4 '()
         l5 '()
         l5-2 '()]
    (if (empty? children)
      (list
       l1
       init-con-env
       init-type-env
       l4
       (concat
        (map (fn [f] (list (first f) (second f))) l5)
        (map (fn [f] (list (first f) (second f))) l5-2)))
      (let [node (first children)]
        (cond (data? node) (recur
                            (rest children)
                            (conj l1 (list (arg1 node) (arg2 node) (arg3 node)))
                            l4 l5 l5-2)
              (type-decl? node) (recur
                                 (rest children)
                                 l1
                                 (conj l4 (list (arg1 node) (universalize-type (arg2 node))))
                                 l5 l5-2)
              (define? node) (let [e (nth node 2)]
                               (cond (lambda? e) (recur
                                                  (rest children)
                                                  l1 l4
                                                  (conj l5 (list (arg1 node) (arg2 node))) l5-2)
                                     :else (recur
                                            (rest children)
                                            l1 l4 l5
                                            (conj l5-2 (list (arg1 node) (arg2 node))))))
              ; (fun-def? node) (recur
              ;                  (rest children)
              ;                  l1 l4
              ;                  (conj l5 node) l5-2)
              ; (csci-909.term/def? node) (recur
              ;                                 (rest children)
              ;                                 l1 l4 l5
              ;                                 (conj l5-2 (list (arg1 node) (arg2 node))))
              ; (decls? node) (recur (concat (arg1 node) (rest children)) l1 l4 l5 l5-2)
              ; :else (throw (Exception. (str "Invalid node type: " node))))))))
              :else (recur (rest children) l1 l4 l5 l5-2))))))

;;;;; type checker

(defn gen-type-var [] (gensym "?t"))

;;; gamma functions

;;; judge-type

(declare judge-type)

(defn make-judge-type [fixed-type]
  (fn [type theta history]
    (unifyTerm4 type fixed-type theta (extend-history history type fixed-type))))

(def judge-type-boolean (make-judge-type (make-boolean-type)))

; ak
(def judge-type-integer (make-judge-type (make-integer-type)))
(def judge-type-double (make-judge-type (make-double-type)))
(def judge-type-string (make-judge-type (make-string-type)))
; (def judge-type-unit (make-judge-type (make-unit-type)))
; (def judge-type-empty-list (make-judge-type (make-empty-list)))

(defn judge-type-var [gamma-con gamma-prim gamma-dec exp type theta history]
  (let [t (gen-type-var)]
   (unifyTerm4 type type theta (extend-history history type type))))

(defn judge-type-lambda [gamma-con gamma-prim gamma-dec exp type theta history]
  (let [args (arg1 exp)
        e (arg2 exp)
        theta-id (map
                  (fn [id]
                    (judge-type gamma-con gamma-prim gamma-dec id (arg1 type) theta (extend-history history id (arg1 type))))
                  args)
        theta-e (judge-type gamma-con gamma-prim gamma-dec e (last type) theta (extend-history history e (arg1 type)))]
    theta))

(defn judge-type-call [gamma-con gamma-prim gamma-dec exp type theta history]
  (let [rator (first exp)
        theta-rator (judge-type gamma-con gamma-prim gamma-dec rator type theta (extend-history history type type))
        rands (rest exp)
        theta-rands (judge-type gamma-con gamma-prim gamma-dec rands type theta (extend-history history type type))]
    theta))


(defn judge-type-let [gamma-con gamma-prim gamma-dec exp type theta history]
  (let []))

(defn judge-type [gamma-con gamma-prim gamma-dec exp type theta history]
  (let [new-history (extend-history history exp type)]
    (cond (boolean? exp)    (judge-type-boolean type theta new-history)
          (integer? exp)    (judge-type-integer type theta new-history)
          (double? exp)     (judge-type-double type theta new-history)
          (string? exp)     (judge-type-string type theta new-history)
          (variable? exp)   (judge-type-var gamma-con gamma-prim gamma-dec exp type theta new-history)
          ; (tuple? exp)      (judge-type-tuple gamma-con gamma-prim gamma-dec exp type theta new-history)
          ; (case? exp)       (judge-type-case gamma-con gamma-prim gamma-dec exp type theta new-history)
          ; (lambda1? exp)    (judge-type-lambda1 gamma-con gamma-prim gamma-dec exp type theta new-history)
          ; (error? exp)      (judge-type-error gamma-con gamma-prim gamma-dec exp type theta new-history)
          ; (call? exp)       (judge-type-call gamma-con gamma-prim gamma-dec exp type theta new-history)
          (lambda? exp)     (judge-type-lambda gamma-con gamma-prim gamma-dec exp type theta new-history)
          (let? exp)        ()
          :else             (judge-type-call gamma-con gamma-prim gamma-dec exp type theta new-history))))
          ;; :else (throw (Exception. (str "In judge-type, unexpected form " (str exp)))))))

(defn no-repeats? [l] (= (count l) (count (set l))))

;;; interface to program representation

(defn check-expression [gamma-con gamma-prim gamma-dec exp type]
  (let [simple-type (if (forall-type? type) (arg2 type) type)
        history (extend-history '() exp type)
        theta (judge-type gamma-con gamma-prim gamma-dec exp simple-type theta-identity history)]
    theta))

(defn check-program-5tuple [lst]
  (let [gamma-con  (nth lst 1)
        gamma-prim (nth lst 2)
        gamma-dec  (nth lst 3)
        code-defs  (nth lst 4)
        type-names (for [p gamma-dec] (first p))
        code-names (for [p code-defs] (first p))]
    (or (and (no-repeats? type-names)
             (no-repeats? code-names)
             (= (count type-names) (count code-names)))
        (throw (Exception. (str "Incompatible number of type declarations and variable definitions."))))
    (loop [code-defs code-defs]
      (if (empty? code-defs)
        'type-table
        (let [id+e (first code-defs)
              id   (first id+e)
              exp  (second id+e)
              type (lookup-gamma gamma-dec id)]
          (let [theta (check-expression gamma-con gamma-prim gamma-dec exp type)]
            (recur (rest code-defs))))))))