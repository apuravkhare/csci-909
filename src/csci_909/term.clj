(ns csci-909.term
  (:use [csci-909.util]))

(defn make-app
  [e e']
  (list e e'))

(defn make-lambda
  [a e]
  (list 'lambda a e))

(defn make-constructor
  [k ms]
  (list 'constructor k ms))

(defn make-fn-constructor
  [k ip]
  (list 'fn-constructor k ip))

(defn make-prog-inst
  [o t e]
  (list 'inst o t e))

(defn make-data
  [k args ts]
  (list 'record k args ts))

(defn make-data-inst
  [k args]
  (list 'data-inst k args))

(defn make-data-fn-inst
  [k l]
  (list 'data-fn-inst k l))


(defn make-op
  [op args]
  (list 'op op args))

(defn make-overload
  [v]
  (list 'overload v))

(defn make-closure
  [f args env]
  (list 'closure f args env))

(defn make-inst-accessor
  [t a]
  (list 'inst-accessor t a))

(defn make-inst-predicate
  [t]
  (list 'inst-predicate t))

(defn make-adt-inst-predicate
  [k t]
  (list 'inst-predicate k t))

(defn make-primitive-action
  [p a]
  (list 'primitive-action p a))

(defn make-typeclass-def
  [t a fs]
  (list 'typeclass-def t a fs))

(defn make-if-expr
  [c t f]
  (list 'if c t f))

(defn make-lambda1 [id e] (list 'lambda1 id e)) ; new

(defn make-let [x e e'] (list 'let x e e'))

(defn make-overload-type [tc a args] (list 'tc-overload tc a args))

(defn make-define [v e] (list 'define v e))

(defn make-cond [cs] (concat (list 'cond) cs))

; (defn make-overload-inst [tc t f-name f dec-type] (list 'tc-inst tc t f-name f dec-type))
(defn make-overload-inst [tc t f-name f] (list 'tc-inst tc t f-name f))

(defn make-adt-constructor [k t args ts] (list 'adt-constructor k t args ts))

; boolean?, integer?, double?, string? are defined in clojure.core

(defn lambda? [a] (and (tagged-list? a) (= (first a) 'lambda)))

(defn let? [a] (and (tagged-list? a) (= (first a) 'let)))

(defn prog-inst? [a] (and (tagged-list? a) (= (first a) 'inst)))

(defn constructor? [a] (and (tagged-list? a) (= (first a) 'constructor)))

(defn fn-constructor? [a] (and (tagged-list? a) (= (first a) 'fn-constructor)))

(defn data-inst? [a] (and (tagged-list? a) (= (first a) 'data-inst)))

(defn data-fn-inst? [a] (and (tagged-list? a) (= (first a) 'data-fn-inst)))

(defn typeclass? [a] (and (tagged-list? a) (= (first a) 'typeclass)))

(def variable? symbol?)

(defn func?
  [a]
  (and (tagged-list? a) (= (first a) 'closure)))

(defn const? [a]
  (or (boolean? a)
      (integer? a)
      (double? a)
      (char? a)
      (string? a)))

(defn data? [a] (and (tagged-list? a) (= (first a) 'record)))

(defn data-adt? [a] (and (tagged-list? a) (= (first a) 'data)))

(defn data-adt-constructor? [a] (and (tagged-list? a) (= (first a) 'adt-constructor)))

(defn data-fn? [a] (and (tagged-list? a) (= (first a) 'data-fn)))

(defn define? [a] (and (tagged-list? a) (= (first a) 'define)))

(defn overload? [a] (and (tagged-list? a) (= (first a) 'overload)))

(defn if-expr? [a] (and (tagged-list? a) (= (first a) 'if)))

(defn inst-accessor? [a] (and (tagged-list? a) (= (first a) 'inst-accessor)))

(defn inst-predicate? [a] (and (tagged-list? a) (= (first a) 'inst-predicate)))

(defn primitive-action? [a] (and (tagged-list? a) (= (first a) 'primitive-action)))

(defn typeclass-def? [a] (and (tagged-list? a) (= (first a) 'typeclass-def)))

(defn typeclass-inst? [a] (and (tagged-list? a) (= (first a) 'typeclass-inst)))

(defn overloaded-type? [a] (and (tagged-list? a) (= (first a) 'tc-overload)))

(defn const-type
  [v]
  (cond (boolean? v) 'boolean
        (integer? v) 'integer
        (double? v)  'double
        (char? v)    'char
        (string? v)  'string))

(defn forall-type? [a] (and (tagged-list? a) (= (first a) 'forall)))

(defn type-decl? [a] (and (tagged-list? a) (= (first a) 'type)))

(defn lambda1? [a] (and (tagged-list? a) (= (first a) 'lambda1)))

(defn decl? [a] (or (type-decl? a)))

(defn cond? [a] (and (tagged-list? a) (= (first a) 'cond)))

; selectors

(defn arg1 [a] (nth a 1))

(defn arg2 [a] (nth a 2))

(defn arg3 [a] (nth a 3))

(defn arg4 [a] (nth a 4))

;;; constructors

(defn make-decls [decls] (list 'decls decls))

(defn make-data-decl [id fmls cs] (list 'decl-data id fmls cs))

(defn make-data-construct [id t] (list 'constructor id t))

(defn make-type-decl [id t] (list 'decl-type id t))

(defn make-boolean-type [] 'boolean)

(defn make-integer-type [] 'integer)

(defn make-double-type [] 'double)

(defn make-string-type [] 'string)

(defn make-named-type [id t] (list 'NamedType id t))

(defn make-list-type [t] (make-named-type '*List* t))

(defn make-arrow-type [ts] (list 'arrowType ts))

(defn make-forall-type [ids t] (list 'forall ids t)) ; new

(defn get-inst-type
  [v]
  (cond (const? v)     (const-type v)
        (data-inst? v) (nth v 1)
        :else          (throw (Exception. (pr-str "Unknown type for input " v)))))

(defn get-inst-type*
  [vs]
  (map get-inst-type vs))