(ns csci-909.checker
  (:use [csci-909.util])
  (:use clojure.set)
  (:use [csci-909.term])
  (:use [csci-909.env])
  (:use [csci-909.primitive])
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


;; (defn universalize-type [t]
;;   (let [ids (free-variables-from-type t)]
;;     (make-forall-type (into '() ids) t)))

(defn universalize-type [t]
  (cond (vector? t) (seq t)
        (seq? t)    t
        :else       t))



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
  ; '((*Cons* (forall (a) (ArrowType (TupleType (a (NamedType *List* a))) (NamedType *List* a)))))
  (global-environment))

(def init-typeclass-env
  (global-environment))

;; (def init-type-env
;;   '((*prim+i (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType))))
;;     (*prim-i (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType))))
;;     (*prim*i (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType))))
;;     (*primdivi (forall () (ArrowType (TupleType ((IntType) (IntType))) (IntType))))
;;     (*prim=i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
;;     (*prim<i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
;;     (*prim>i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
;;     (*prim<=i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
;;     (*prim>=i (forall () (ArrowType (TupleType ((IntType) (IntType))) (BoolType))))
;;     (*prim+d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))))
;;     (*prim-d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))))
;;     (*prim*d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))))
;;     (*primdivd (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (DoubleType))))
;;     (*prim<d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
;;     (*prim>d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
;;     (*prim=d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
;;     (*prim<=d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
;;     (*prim>=d (forall () (ArrowType (TupleType ((DoubleType) (DoubleType))) (BoolType))))
;;     (*prim+str (forall () (ArrowType (TupleType ((StringType) (StringType))) (StringType))))
;;     (*prim=str (forall () (ArrowType (TupleType ((StringType) (StringType))) (BoolType))))
;;     (*prim-and (forall () (ArrowType (TupleType ((BoolType) (BoolType))) (BoolType))))
;;     (*prim-or (forall () (ArrowType (TupleType ((BoolType) (BoolType))) (BoolType))))))

(def init-type-env (extend-environment* primitives primitive-action-types (global-environment)))


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

(defn split-decl
  ""
  [tree]
  (loop [children tree
         l1 (global-environment) ; data decl
         l2 (global-environment) ; type class
         l4 (global-environment) ; type decl
         l5 (global-environment) ; definitions
         l6 (global-environment) ; type class instances
         ] 
    (if (empty? children)
      (list l1 l2 init-type-env l4 l5 l6)
      (let [node (first children)]
        (cond (data? node) (recur
                            (rest children)
                            (extend-environment (arg1 node) (universalize-type (arg2 node)) l1)
                            l2 l4 l5 l6)
              (type-decl? node) (recur
                                 (rest children)
                                 l1
                                 l2
                                 ; (conj l4 (list (arg1 node) (universalize-type (arg2 node))))
                                 (extend-environment (arg1 node) (universalize-type (arg2 node)) l4)
                                 l5
                                 l6)
              (define? node) (let [e (nth node 2)]
                               (recur
                                (rest children)
                                l1 l2 l4
                                (extend-environment (arg1 node) (universalize-type (arg2 node)) l5)
                                l6))
              (typeclass? node) (let [tc (nth node 1)
                                      a (nth node 2)
                                      fs (drop 3 node)]
                                  (recur
                                   (rest children)
                                   l1
                                   ; (extend-environment tc node l2)
                                   ; (extend-environment* (map first fs) (map (fn [f] (make-overload-type tc a (universalize-type (second f)))) fs) l4)
                                   (extend-environment* (map first fs) (map (fn [f] (make-overload-type tc a (universalize-type (second f)))) fs) l4)
                                   l4
                                   l5
                                   l6))
              (typeclass-inst? node) (let [tc-name (nth node 1)
                                           ; tc (lookup-environment tc-name l2)
                                           t (nth node 2)
                                           fs (filter (fn [f] (not (type-decl? f))) (drop 3 node))
                                           fts (filter (fn [f] (type-decl? f)) (drop 3 node))]
                                       (recur
                                        (rest children)
                                        l1 l2 l4 l5
                                        (extend-environment*
                                         (map first fs)
                                         (map (fn [f] (make-overload-inst tc-name t (first f)
                                                                          (make-lambda (nth f 1) (nth f 2))
                                                                          (universalize-type (nth (find-first (fn [ft] (= (second ft) (first f))) fts) 2))))
                                              fs)
                                         l6)))
              :else (recur (rest children) l1 l2 l4 l5 l6))))))

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
  (let [t (lookup-environments (list gamma-dec gamma-con gamma-prim) exp)]
   (unifyTerm4 t type theta (extend-history history type type))))

(defn judge-type-lambda [gamma-con gamma-prim gamma-dec exp type theta history]
  (let [args (arg1 exp)
        e (arg2 exp)
        type-args (if (seq? type) (take (- (count type) 1) type) '())
        type-op (if (seq? type) (last type) type)
        no (do (println (str "type " type " args " args " type-args " type-args)) '())
        theta-args (loop [args args
                          type-args type-args]
                     (if (= (count args) (count type-args))
                       (if (empty? args)
                         theta
                         (do
                           (judge-type gamma-con gamma-prim (extend-environment (first args) (gen-type-var) gamma-dec) (first args) (first type-args) theta (extend-history history (first args) (first type-args)))
                           (recur (rest args) (rest type-args))))
                       (throw (Exception. (str "Mismatched arguments in type declaration for lambda")))))
        theta-e (judge-type gamma-con gamma-prim gamma-dec e type-op theta (extend-history history e type-op))]
    theta))

(defn judge-type-call [gamma-con gamma-prim gamma-dec exp type theta history]
  (let [rator (first exp)
        theta-rator (lookup-environments (list gamma-dec gamma-con gamma-prim) rator)
        type-op (if (seq? theta-rator) (last theta-rator) theta-rator)
        rands (rest exp)
        type-args (if (seq? theta-rator) (take (- (count theta-rator) 1) theta-rator) '())
        theta-rands (loop [args rands
                           type-args type-args]
                      (if (= (count args) (count type-args))
                        (if (empty? args)
                          theta
                          (do
                            (judge-type gamma-con gamma-prim (extend-environment (first args) (gen-type-var) gamma-dec) (first args) (first type-args) theta history)
                            (recur (rest args) (rest type-args))))
                        (throw (Exception. (str "Mismatched arguments in type declaration for call")))))]
    ; (println "type " type)
    ; (println (str "theta-rator " rator " "  theta-rator))
    ; (println (str "theta-rands " rands " " theta-rands))
    (unifyTerm4 type-op type theta (extend-history history type type))))

(defn replace-typeclass-type
  [tc f t]
  (let [a (nth tc 2)
        tc-fs (drop 3 tc)
        tcf (find-first (fn [ft] (= (first f) (second ft))) tc-fs)]
    (find-replace a t tcf)))

;; (defn judge-type-typeclass-inst [gamma-tc gamma-prim gamma-dec exp type theta history]
;;   (let [tc-name (nth exp 1)
;;         tc (lookup-environment tc-name gamma-tc)
;;         tc-fs (drop 3 tc)
;;         t (nth exp 2)
;;         fs (filter (fn [f] (not (type-decl? f))) (drop 3 exp))
;;         fts (filter (fn [f] (type-decl? f)) (drop 3 exp))]
;;     (if (= (count tc-fs) (count fs))
;;       (loop [fs fs]
;;         (let [f (first fs)
;;               ft (find-first (fn [ft] (= (first f) (second ft))) fts)
;;               rt (replace-typeclass-type tc (first f) t)]
;;           (judge-type gamma-tc gamma-prim gamma-dec (make-lambda (arg1 f) (arg2 f)) rt theta (extend-history history f ft))))
;;       (throw (Exception. (str "Mismatched number of functions defined for typeclass."))))))

(defn judge-type-let [gamma-con gamma-prim gamma-dec exp type theta history]
  (let []))


(defn judge-type [gamma-con gamma-prim gamma-dec exp type theta history]
  (let [new-history (extend-history history exp type)]
    (println (str "judge " exp " " type))
    (cond (boolean? exp)    (judge-type-boolean type theta new-history)
          (integer? exp)    (judge-type-integer type theta new-history)
          (double? exp)     (judge-type-double type theta new-history)
          (string? exp)     (judge-type-string type theta new-history)
          (variable? exp)   (judge-type-var gamma-con gamma-prim gamma-dec exp type theta new-history)
          (lambda? exp)     (judge-type-lambda gamma-con gamma-prim gamma-dec exp type theta new-history)
          ; (typeclass-inst? exp) (judge-type-typeclass-inst gamma-con gamma-prim gamma-dec exp type theta new-history)
          ; (let? exp)        ()
          :else             (judge-type-call gamma-con gamma-prim gamma-dec exp type theta new-history))))
          ;; :else (throw (Exception. (str "In judge-type, unexpected form " (str exp)))))))

(defn no-repeats? [l] (= (count l) (count (set l))))

;;; interface to program representation

(defn check-expression [gamma-con gamma-prim gamma-dec exp type]
  (let [simple-type (if (forall-type? type) (arg2 type) type)
        history (extend-history '() exp type)
        theta (judge-type gamma-con gamma-prim gamma-dec exp simple-type theta-identity history)]
    theta))

(defn check-inst [gamma-tc gamma-prim gamma-dec inst-name insts]
  (let [tc-fn (lookup-environment inst-name gamma-tc)]
    (loop [insts insts]
      (if (empty? insts)
        true
        (let [inst (first insts)
              dec-type (nth inst 5)]
         (check-expression gamma-tc gamma-prim gamma-dec (nth inst 4) dec-type)
          (recur (rest insts)))))))

(defn check-program-5tuple [lst]
  (let [gamma-tc  (nth lst 1)
        gamma-prim (nth lst 2)
        gamma-dec  (nth lst 3)
        code-defs  (nth lst 4)
        tc-insts   (nth lst 5)
        type-names (environment-keys gamma-dec)
        code-names (environment-keys code-defs)]
    ; (println "code defs " gamma-dec)
    (or (and (no-repeats? type-names)
             (no-repeats? code-names)
             (= (count type-names) (count code-names)))
        (throw (Exception. (str "Incompatible number of type declarations and variable definitions."))))
    (loop [tc-inst-keys (unique (environment-keys tc-insts))]
      (if (empty? tc-inst-keys)
        true
        (do
          (check-inst gamma-tc gamma-prim gamma-dec (first tc-inst-keys) (lookup-environment* (first tc-inst-keys) tc-insts))
          (recur (rest tc-inst-keys)))))
    (loop [code-defs-keys (environment-keys code-defs)]
      (if (empty? code-defs-keys)
        'type-table
        (let [id   (first code-defs-keys)
              exp  (lookup-environment id code-defs)
              type (lookup-environment id gamma-dec)]
          (let [theta (check-expression gamma-tc gamma-prim gamma-dec exp type)]
            (recur (rest code-defs-keys))))))))

