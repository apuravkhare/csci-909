(ns csci-909.checker
  (:use [csci-909.util])
  (:use clojure.set)
  (:use [csci-909.term])
  (:use [csci-909.env])
  (:use [csci-909.primitive])
  (:use [csci-909.unification :only (unifyTerm4 unifyTerm5 failure extend-history theta-identity logic-variable? applyUnifier)]))

(defn universalize-type [t]
  (cond (vector? t) (seq t)
        (seq? t)    t
        :else       t))

(def init-type-env (extend-environment* primitives primitive-action-types (global-environment)))

(defn split-decl
  "splits the input code form into separate lists of declarations and types."
  [tree]
  (loop [children tree
         l1 (global-environment) ; data decl
         l2 (global-environment) ; type class
         l4 (global-environment) ; type decl
         l5 (global-environment) ; definitions
         l6 (global-environment) ; type class instances
         l7 (global-environment) ; type constraints
         l8 (global-environment) ; type to type class map
         ] 
    (if (empty? children)
      (list l1 l2 init-type-env l4 l5 l6 l7 l8)
      (let [node (first children)]
        (cond (data? node) (recur
                            (rest children)
                            (extend-environment (arg1 node) (universalize-type (arg2 node)) l1)
                            l2 l4 l5 l6 l7 l8)
              (type-decl? node) (recur
                                 (rest children)
                                 l1
                                 l2
                                 (extend-environment (arg1 node) (universalize-type (arg2 node)) l4)
                                 l5
                                 l6
                                 (if (= (count node) 4)
                                   (extend-environment (arg1 node) (universalize-type (arg3 node)) l7)
                                   l7)
                                 l8)
              (define? node) (recur
                                (rest children)
                                l1 l2 l4
                                (extend-environment (arg1 node) (arg2 node) l5)
                                l6 l7 l8)
              (typeclass? node) (let [tc (nth node 1)
                                      a (nth node 2)
                                      fs (drop 3 node)]
                                  (recur
                                   (rest children)
                                   l1
                                   (extend-environment* (map first fs) (map (fn [f] (make-overload-type tc a (universalize-type (second f)))) fs) l2)
                                   l4 l5 l6 l7 l8))
              (typeclass-inst? node) (let [tc-name (nth node 1)
                                           ; tc (lookup-environment tc-name l2)
                                           t (nth node 2)
                                           fs (filter (fn [f] (not (type-decl? f))) (drop 3 node))
                                           ; fts (filter (fn [f] (type-decl? f)) (drop 3 node))
                                           ]
                                       (recur
                                        (rest children)
                                        l1 l2 l4 l5
                                        (extend-environment*
                                         (map first fs)
                                         (map (fn [f] (make-overload-inst tc-name t (first f)
                                                                          (make-lambda (nth f 1) (nth f 2))))
                                              fs)
                                         l6) l7
                                        (extend-environment t tc-name l8)))
              :else (recur (rest children) l1 l2 l4 l5 l6 l7 l8))))))

;;;;; type checker

(defn gen-type-var [] (gensym "?t"))

;;; gamma functions

;;; judge-type

(declare judge-type)

(defn make-judge-type [fixed-type]
  (fn [type theta history constraints type-tc-map]
    (unifyTerm5 type fixed-type theta (extend-history history type fixed-type) constraints type-tc-map)))

(def judge-type-boolean (make-judge-type (make-boolean-type)))

; ak
(def judge-type-integer (make-judge-type (make-integer-type)))
(def judge-type-double (make-judge-type (make-double-type)))
(def judge-type-string (make-judge-type (make-string-type)))

(defn judge-type-var [gamma-con gamma-prim gamma-dec exp type theta history]
  (let [t (lookup-environments (list gamma-dec gamma-con gamma-prim) exp)]
   (unifyTerm4 t type theta (extend-history history type type))))

(defn judge-type-lambda [gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  (let [args (arg1 exp)
        e (arg2 exp)
        type-args (if (seq? type) (take (- (count type) 1) type) '())
        type-op (if (seq? type) (last type) type)
        ; no (do (println (pr-str "type " type " args " args " type-args " type-args)) '())
        theta-args (loop [args args
                          type-args type-args]
                     (if (= (count args) (count type-args))
                       (if (empty? args)
                         theta
                         (do
                           (judge-type gamma-con gamma-prim (extend-environment (first args) (gen-type-var) gamma-dec) tc-insts (first args) (first type-args) theta (extend-history history (first args) (first type-args)) constraints type-tc-map)
                           (recur (rest args) (rest type-args))))
                       (throw (Exception. (str "Mismatched arguments in type declaration for lambda")))))
        theta-e (judge-type gamma-con gamma-prim (extend-environment* args type-args gamma-dec) tc-insts e type-op theta (extend-history history e type-op) constraints type-tc-map)]
    theta))

(defn judge-overloaded-call [gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  (let [rator (first exp)
        theta-rator (lookup-environments (list gamma-dec gamma-con gamma-prim) rator)
        overloads (lookup-environment* rator tc-insts)
        rands (rest exp)
        constraint-classes (set (map first constraints))
        type-args (take (- (count theta-rator) 1) (nth theta-rator 3))
        theta-rands (loop [overloads overloads
                           matched '()]
                        (if (empty? overloads)
                          (if (empty? matched)
                           (throw (Exception. (str "No suitable overload found " rator)))
                            matched)
                         (let [res'
                               (loop [args rands
                                      type-args type-args]
                                 (if (empty? args)
                                   (first overloads)
                                   (let [res (try
                                               (judge-type gamma-con gamma-prim (extend-environment (first args) (gen-type-var) gamma-dec) tc-insts (first args)
                                                           (if (= (first type-args) (nth theta-rator 2))
                                                             (nth (first overloads) 2)
                                                             (first type-args))
                                                           theta history constraints type-tc-map)
                                               (catch Exception _ false))]
                                     (if res (recur (rest args) (rest type-args)) false))))]
                           (if res'
                             (recur (rest overloads) (cons (first overloads) matched))
                             (recur (rest overloads) matched)))))
        valid-trs (filter (fn [tr] (subset? constraint-classes (set (lookup-environment* (nth tr 2) type-tc-map)))) theta-rands)]
    ; (println (pr-str " type " type " theta-rands " theta-rands ))
    (if (empty? constraint-classes)
      (if (contains? (set (map (fn [vt] (if
                                         (= (nth theta-rator 2) (last (nth theta-rator 3)))
                                          (arg2 vt)
                                          (last (nth theta-rator 3)))) valid-trs)) type)
        type
        (throw (Exception. (pr-str "Invalid arguments to call " exp))))
      valid-trs)))

(defn judge-type-call [gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  (let [rator (first exp)
        theta-rator (lookup-environments (list gamma-dec gamma-con gamma-prim) rator)
        type-op (if (seq? theta-rator) (last theta-rator) theta-rator)
        rands (rest exp)
        type-args (if (seq? theta-rator) (take (- (count theta-rator) 1) theta-rator) '())]
    (if (overloaded-type? theta-rator)
      (judge-overloaded-call gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map)
      (do
        (if (= (count rands) (count type-args))
                      (loop [args rands
                             type-args type-args]
                        (if (empty? args)
                          theta
                          (do
                            (judge-type gamma-con gamma-prim (extend-environment (first args) (gen-type-var) gamma-dec) tc-insts (first args) (first type-args) theta history constraints type-tc-map)
                            (recur (rest args) (rest type-args)))))
                        (throw (Exception. (str "Mismatched arguments in type declaration for call"))))
        (unifyTerm4 type-op type theta (extend-history history type type))))))

(defn judge-type-let [gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  (let []))


(defn judge-type-if
  [gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  (let [c (arg1 exp)
        t (arg2 exp)
        f (arg3 exp)]
    (judge-type gamma-con gamma-prim gamma-dec tc-insts c 'boolean theta-identity history constraints type-tc-map)
    (judge-type gamma-con gamma-prim gamma-dec tc-insts t type theta-identity history constraints type-tc-map)
    (judge-type gamma-con gamma-prim gamma-dec tc-insts f type theta-identity history constraints type-tc-map)))

(defn judge-type [gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  (let [new-history (extend-history history exp type)]
    (println (str "judge " exp " " type))
    (cond (boolean? exp)    (judge-type-boolean type theta new-history constraints type-tc-map)
          (integer? exp)    (judge-type-integer type theta new-history constraints type-tc-map)
          (double? exp)     (judge-type-double type theta new-history constraints type-tc-map)
          (string? exp)     (judge-type-string type theta new-history constraints type-tc-map)
          (variable? exp)   (judge-type-var gamma-con gamma-prim gamma-dec exp type theta new-history)
          (lambda? exp)     (judge-type-lambda gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map)
          (if-expr? exp)    (judge-type-if gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map)
          (let? exp)        (judge-type-let gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map)
          :else             (judge-type-call gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map))))

(defn no-repeats? [l] (= (count l) (count (set l))))

;;; interface to program representation

(defn check-expression [gamma-con gamma-prim gamma-dec tc-insts exp type constraints type-tc-map]
  (let [simple-type (if (forall-type? type) (arg2 type) type)
        history (extend-history '() exp type)
        theta (judge-type gamma-con gamma-prim gamma-dec tc-insts exp simple-type theta-identity history constraints type-tc-map)]
    theta))

(defn replace-typeclass-type
  [tc-fn t]
  (let [a (nth tc-fn 2)]
    (find-replace a t (nth tc-fn 3))))

(defn check-inst [gamma-tc gamma-prim gamma-dec tc-insts inst-name insts type-tc-map]
  (let [tc-fn (lookup-environment inst-name gamma-tc)]
    (loop [insts insts]
      (if (empty? insts)
        true
        (let [inst (first insts)
              dec-type (replace-typeclass-type tc-fn (nth inst 2))]
          (check-expression gamma-tc gamma-prim gamma-dec tc-insts (nth inst 4) dec-type nil type-tc-map)
          (recur (rest insts)))))))

(defn check-program-5tuple [lst]
  (let [gamma-tc  (nth lst 1)
        gamma-prim (nth lst 2)
        gamma-dec  (nth lst 3)
        code-defs  (nth lst 4)
        tc-insts   (nth lst 5)
        constraint-decl (nth lst 6)
        type-tc-map (nth lst 7)
        type-names (environment-keys gamma-dec)
        code-names (environment-keys code-defs)]
    ; (println "code defs " gamma-dec)
    (or (and (no-repeats? type-names)
             (no-repeats? code-names)
             (= (count type-names) (count code-names)))
        (throw (Exception. (str "Incompatible number of type declarations and variable definitions."))))
    (println (pr-str "gamma-tc " gamma-tc))
    (loop [tc-inst-keys (unique (environment-keys tc-insts))]
      (if (empty? tc-inst-keys)
        true
        (do
          (check-inst gamma-tc gamma-prim gamma-dec tc-insts (first tc-inst-keys) (lookup-environment* (first tc-inst-keys) tc-insts) type-tc-map)
          (recur (rest tc-inst-keys)))))
    (loop [code-defs-keys (environment-keys code-defs)]
      (if (empty? code-defs-keys)
        'type-table
        (let [id   (first code-defs-keys)
              exp  (lookup-environment id code-defs)
              type (lookup-environment id gamma-dec)
              constraints (try-lookup-environment id constraint-decl)]
          (let [theta (check-expression gamma-tc gamma-prim gamma-dec tc-insts exp type constraints type-tc-map)]
            (recur (rest code-defs-keys))))))))

