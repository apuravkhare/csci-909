(ns csci-909.checker
  (:use [csci-909.util])
  (:use clojure.set)
  (:use [csci-909.term])
  (:use [csci-909.env])
  (:use [csci-909.primitive])
  (:use [csci-909.transformer])
  (:use [csci-909.unification :only (unifyTerm4 unifyTerm5 failure extend-history theta-identity logic-variable? applyUnifier)]))

(defn universalize-type [t]
  (cond (vector? t) t
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
                            (extend-environment (arg1 node) (make-data
                                                             (arg1 node)
                                                             (universalize-type (arg2 node))
                                                             (if (= (count node) 4) (universalize-type (arg3 node)) '())) l1)
                            l2
                            (if (= (count node) 4)
                              (extend-environment*
                               (map (fn [arg] (symbol (str (arg1 node) "-" arg))) (nth node 2))
                               (map (fn [t] [(arg1 node) t]) (arg3 node))
                               l4)
                              l4)
                            (extend-environment*
                             (map (fn [arg] (symbol (str (arg1 node) "-" arg))) (nth node 2))
                             (map (fn [arg] (make-inst-accessor (arg1 node) arg)) (nth node 2))
                             ; (extend-environment (arg1 node) node l5)
                             l5)
                            l6 l7 l8)
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
  (fn [type theta history constraints gamma-dt gamma-prim type-tc-map]
    (unifyTerm5 type fixed-type theta (extend-history history type fixed-type) constraints gamma-dt gamma-prim type-tc-map)))

(def judge-type-boolean (make-judge-type (make-boolean-type)))

; ak
(def judge-type-integer (make-judge-type (make-integer-type)))
(def judge-type-double (make-judge-type (make-double-type)))
(def judge-type-string (make-judge-type (make-string-type)))

(defn judge-type-var [gamma-con gamma-prim gamma-dec exp type theta history]
  (let [t (lookup-environments (list gamma-dec gamma-con gamma-prim) exp)]
    (unifyTerm4 t type theta (extend-history history type type))))

(defn judge-type-lambda [gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
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
                           (judge-type gamma-dt gamma-con gamma-prim (extend-environment (first args) (gen-type-var) gamma-dec) tc-insts (first args) (first type-args) theta (extend-history history (first args) (first type-args)) constraints type-tc-map)
                           (recur (rest args) (rest type-args))))
                       (throw (Exception. (str "Mismatched arguments in type declaration for lambda")))))
        theta-e (judge-type gamma-dt gamma-con gamma-prim (extend-environment* args type-args gamma-dec) tc-insts e type-op theta (extend-history history e type-op) constraints type-tc-map)]
    theta))

(defn judge-overloaded-call [gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  ; (println "overloaded " exp)
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
                                              (judge-type gamma-dt gamma-con gamma-prim (extend-environment (first args) (gen-type-var) gamma-dec) tc-insts (first args)
                                                          (if (= (first type-args) (nth theta-rator 2))
                                                            (nth (first overloads) 2)
                                                            (first type-args))
                                                          theta history constraints type-tc-map)
                                              (catch Exception e (do (println (str "Inner error " e)) false)))]
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

(defn declared-type?
  [gamma-dt gamma-prim t]
  (or (in? (environment-keys gamma-dt) t)
      (in? (environment-keys gamma-prim) t)))

(defn remap-types
  [decl-types actual-types gamma-dt gamma-prim]
  (loop [decl-types decl-types
         actual-types actual-types
         remapped '()]
    (if (empty? actual-types) (reverse remapped)
        (if (declared-type? gamma-dt gamma-prim (first actual-types))
          (recur (rest decl-types) (rest actual-types) (cons (first actual-types) remapped))
          (recur (rest decl-types) (rest actual-types) (cons (first decl-types) remapped))))))

(defn judge-type-constructor [gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  ; (println (str "constructor " exp))
  (let [rator (first exp)
        theta-rator (lookup-environment rator gamma-dt)
        rands (rest exp)
        type-args (arg3 theta-rator)]
    (cond
      (= (count type-args) 0) type-args
      (= (count rands) (count type-args))
      (let [remapped-types (remap-types (if (seq? type) (rest type) '()) type-args gamma-dt gamma-prim)]
        (loop [args rands
               type-args remapped-types]
          (if (empty? args)
            theta
            (do
              (judge-type gamma-dt gamma-con gamma-prim (extend-environment (first args) (gen-type-var) gamma-dec) tc-insts (first args) (first type-args) theta history constraints type-tc-map)
              (recur (rest args) (rest type-args))))))
      :else (throw (Exception. (str "Mismatched arguments in type declaration for constructor"))))
    type))

(defn judge-type-call [gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  ; (println (str "call " exp))
  (let [rator (first exp)
        theta-rator (lookup-environments (list gamma-dec gamma-con gamma-prim) rator)
        ; theta-rator (if (empty? constraints) theta-rator (reduce (fn [acc a] (find-replace a type acc)) theta-rator (map second constraints)))
        theta-rator (if (seq? type) (remap-types type theta-rator gamma-dt gamma-prim) theta-rator)
        ; type-op (if (seq? theta-rator) (last theta-rator) theta-rator)
        rands (rest exp)
        type-args (if (seq? theta-rator) (take (- (count theta-rator) 1) theta-rator) '())]
    (cond
      (overloaded-type? theta-rator) (judge-overloaded-call gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map)
      ; (data? theta-rator) (judge-type-constructor gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map)
      :else (do
              (if (= (count rands) (count type-args))
                (loop [args rands
                       type-args type-args]
                  (if (empty? args)
                    theta
                    (do
                      (judge-type gamma-dt gamma-con gamma-prim (extend-environment (first args) (gen-type-var) gamma-dec) tc-insts (first args) (first type-args) theta history constraints type-tc-map)
                      (recur (rest args) (rest type-args)))))
                (throw (Exception. (str "Mismatched arguments in type declaration for call"))))
        ; (unifyTerm5 type type theta (extend-history history type type) constraints gamma-dt gamma-prim type-tc-map)
              type))))

(defn judge-inst-accessor [gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  ; (println (str "inst-accessor " exp))
  true)

(defn judge-type-let [gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  (let []))


(defn judge-type-if
  [gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  (let [c (arg1 exp)
        t (arg2 exp)
        f (arg3 exp)]
    (judge-type gamma-dt gamma-con gamma-prim gamma-dec tc-insts c 'boolean theta-identity history constraints type-tc-map)
    (judge-type gamma-dt gamma-con gamma-prim gamma-dec tc-insts t type theta-identity history constraints type-tc-map)
    (judge-type gamma-dt gamma-con gamma-prim gamma-dec tc-insts f type theta-identity history constraints type-tc-map)))

(defn judge-type [gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta history constraints type-tc-map]
  (let [new-history (extend-history history exp type)]
    ; (println (str "judge " exp " " type))
    (cond (boolean? exp)    (judge-type-boolean type theta new-history constraints gamma-dt gamma-prim type-tc-map)
          (integer? exp)    (judge-type-integer type theta new-history constraints gamma-dt gamma-prim type-tc-map)
          (double? exp)     (judge-type-double type theta new-history constraints gamma-dt gamma-prim type-tc-map)
          (string? exp)     (judge-type-string type theta new-history constraints gamma-dt gamma-prim type-tc-map)
          (variable? exp)   (judge-type-var gamma-con gamma-prim gamma-dec exp type theta new-history)
          (data-inst? exp)  (arg1 exp)
          (lambda? exp)     (judge-type-lambda gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map)
          (if-expr? exp)    (judge-type-if gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map)
          (let? exp)        (judge-type-let gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map)
          (inst-accessor? exp) theta
          (data? exp)       theta
          :else             (cond
                              (in? (environment-keys gamma-dt) (first exp)) (judge-type-constructor gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map)

                              (and (in? (environment-keys gamma-dec) (first exp)) (inst-accessor? (lookup-environment (first exp) gamma-dec))) (judge-inst-accessor gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map)

                              :else (judge-type-call gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp type theta new-history constraints type-tc-map)))))
(defn defined-type?
  [k gamma-dt gamma-tc type-tc-map]
  (or ; (in? (environment-keys gamma-dt) k)
   (in? (environment-vals type-tc-map) k)
   (primitive-type? k)))

(defn extract-type-constructor ; var | k | (k var)
  [k gamma-dt gamma-tc type-tc-map]
  (if (seq? k)
    (if (defined-type? (first k) gamma-dt gamma-tc type-tc-map)
      (first k)
      (throw (Exception. (str "Invalid type declaration " k))))
    (if (defined-type? k gamma-dt gamma-tc type-tc-map)
      k
      nil)))

(defn extract-type-var ; var | k | (k var)
  [k gamma-dt gamma-tc type-tc-map]
  (if (seq? k)
    (if (defined-type? (first k) gamma-dt gamma-tc type-tc-map)
      (if (= (count k) 2)
        (set (rest k))
        (throw (Exception. (str "Invalid type declaration " k))))
      (throw (Exception. (str "Invalid type declaration " k))))
    (if (defined-type? k gamma-dt gamma-tc type-tc-map)
      #{}
      #{k})))

(defn compare-types-2
  [k1 k2 gamma-dt gamma-tc type-tc-map constraints] ; consider k1 <= k2 here
  (if (empty? constraints)
    (if (defined-type? k1 gamma-dt gamma-tc type-tc-map)
      (if (defined-type? k2 gamma-dt gamma-tc type-tc-map)
        (= k1 k2) ; k k
        true) ; k a
      (if (defined-type? k2 gamma-dt gamma-tc type-tc-map)
        false ; a k
        true)) ; a a
    (if (defined-type? k1 gamma-dt gamma-tc type-tc-map)
      (if (defined-type? k2 gamma-dt gamma-tc type-tc-map)
        (= k1 k2) ; k k
        (let [matched-constraint-names (map first (filter (fn [c] (= k2 (second c))) constraints))
              matched-types            (lookup-environment* k1 type-tc-map)]
          ; (println (str "Matched types " matched-types))
          ; (println (pr-str "matched-constraint-names " matched-constraint-names))
          (clojure.set/subset? (set matched-constraint-names) (set matched-types)))) ; k a
      (if (defined-type? k2 gamma-dt gamma-tc type-tc-map)
        (let [matched-constraint-names (map first (filter (fn [c] (= k1 (second c))) constraints))
              matched-types            (lookup-environment* k2 type-tc-map)]
          (clojure.set/subset? (set matched-constraint-names) (set matched-types))) ; a k
        true)))) ; a a

(defn compare-types
  [decl-type actual-type gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints]
  (if (or (and (vector? decl-type) (= (count decl-type) 1))
          (seq? decl-type)
          (variable? decl-type))
    (let [decl-k  (if (seq? decl-type) (first decl-type) decl-type)
          actual-k (if (seq? actual-type) (first actual-type) actual-type)]
      (if (or (compare-types-2 decl-k actual-k gamma-dt gamma-tc type-tc-map constraints)
              (compare-types-2 actual-k decl-k gamma-dt gamma-tc type-tc-map constraints))
        decl-type
        (throw (Exception. (str "Type check failed " decl-type " - " actual-type " - " constraints)))))
    (throw (Exception. (str "Invalid type comparison " decl-type " - " actual-type)))))

(defn lookup-environment-type
  [v gamma-dec gamma-dt gamma-tc gamma-prim tc-insts]
  (cond (const? v)     (const-type v)
        (variable? v)  (lookup-environment v gamma-dec)
        (lambda? v)    (let [args (arg1 v)
                             e    (arg2 v)]
                         (cond (and (seq? e) (lambda? e)) (lookup-environment-type e gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)
                               (seq? e)           (lookup-environment-type (first e) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)
                               :else              (lookup-environment-type e gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)))
        :else          (let [rator (first v)
                             rands (rest v)]
                         (cond (in? (environment-keys gamma-prim) rator) (last (lookup-environment rator gamma-prim))
                               (in? (environment-keys gamma-dt) rator)   rator
                               (in? (environment-keys tc-insts) rator)   (last (arg3 (lookup-environment rator gamma-tc)))
                               (lambda? rator) (let [args (arg1 rator)
                                                     e    (arg2 rator)]
                                                 (lookup-environment-type e (extend-environment* args (map (fn [r] (lookup-environment-type r gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) rands) gamma-dec) gamma-dt gamma-tc gamma-prim tc-insts))
                               :else (lookup-environment-type rator gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)))))

(defn check-type
  [exp decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints]
  (println (str "Check type " exp " " decl-type))
  (cond (boolean? exp) (compare-types 'boolean decl-type gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
        (integer? exp) (compare-types 'integer decl-type gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
        (double? exp)  (compare-types 'double decl-type gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
        (char? exp)    (compare-types 'char decl-type gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
        (string? exp)  (compare-types 'string decl-type gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
        (variable? exp) (compare-types (lookup-environment-type exp gamma-dec gamma-dt gamma-tc gamma-prim tc-insts) decl-type gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
        (lambda? exp)  (let [args (arg1 exp)
                             e    (arg2 exp)]
                         (if (vector? decl-type)
                           (check-type e (last decl-type) (extend-environment* args (take (- (count decl-type) 1) decl-type) gamma-dec) gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                           (check-type e decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)))
        (let? exp)     (let [x  (arg1 exp)
                             e  (arg2 exp)
                             e' (arg3 exp)
                             e-type (lookup-environment-type e gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)]
                         (check-type e' decl-type (extend-environment x e-type gamma-dec) gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                         decl-type)
        (define? exp)  (let [v (arg1 exp)
                             e (arg2 exp)]
                         (check-type e decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints))
        (if-expr? exp) (let [c (arg1 exp)
                             t (arg2 exp)
                             f (arg3 exp)]
                         (check-type c 'boolean gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                         (check-type t decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                         (check-type f decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints))
        (inst-accessor? exp) decl-type
        :else (let [rator (first exp)
                    rands (rest exp)]
                (cond (in? (environment-keys gamma-prim) rator) (let [prim-types (lookup-environment rator gamma-prim)]
                                                                  (loop [rands      rands
                                                                         rand-types (take (- (count prim-types) 1) prim-types)]
                                                                    (if (empty? rands)
                                                                      decl-type
                                                                      (do
                                                                        (check-type (first rands) (first rand-types) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                                                                        (recur (rest rands) (rest rand-types))))))
                      (in? (environment-keys gamma-dt) rator) (let [arg-types (arg3 (lookup-environment rator gamma-dt))]
                                                                (loop [rands      rands
                                                                       rand-types (take (- (count arg-types) 1) arg-types)]
                                                                  (if (empty? rands)
                                                                    decl-type
                                                                    (do
                                                                      (check-type (first rands) (first rand-types) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                                                                      (recur (rest rands) (rest rand-types))))))
                      (in? (environment-keys tc-insts) rator) (let [arg-types (arg3 (lookup-environment rator gamma-tc))]
                                                                (loop [rands      rands
                                                                       rand-types (take (- (count arg-types) 1) arg-types)]
                                                                  (if (empty? rands)
                                                                    decl-type
                                                                    (do
                                                                      (check-type (first rands) (first rand-types) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                                                                      (recur (rest rands) (rest rand-types))))))
                      (lambda? rator) (let [args (arg1 exp)
                                            e    (arg2 exp)]
                                        (check-type e decl-type (extend-environment* args (map (fn [r] (lookup-environment-type  r gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) rands) gamma-dec) gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints))
                      :else (let [rand-types (lookup-environment rator gamma-dec)]
                              (loop [rands      rands
                                     rand-types (take (- (count rand-types) 1) rand-types)]
                                (if (empty? rands)
                                  decl-type
                                  (do
                                    (check-type (first rands) (first rand-types) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                                    (recur (rest rands) (rest rand-types))))))))))

(defn no-repeats? [l] (= (count l) (count (set l))))

;;; interface to program representation

(defn check-expression [gamma-dt gamma-tc gamma-prim gamma-dec tc-insts exp type constraints type-tc-map]
  (let [simple-type (if (forall-type? type) (arg2 type) type)
        ; history (extend-history '() exp type)
        ; theta (judge-type gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp simple-type theta-identity history constraints type-tc-map)
        theta (check-type exp simple-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)]
    theta))

(defn replace-typeclass-type
  [tc-fn t]
  (let [a (nth tc-fn 2)]
    (vec (find-replace a t (nth tc-fn 3)))))

(defn check-inst [gamma-dt gamma-tc gamma-prim gamma-dec tc-insts inst-name insts type-tc-map]
  (let [tc-fn (lookup-environment inst-name gamma-tc)]
    (loop [insts insts]
      (if (empty? insts)
        true
        (let [inst (first insts)
              dec-type (replace-typeclass-type tc-fn (nth inst 2))]
          (check-expression gamma-dt gamma-tc gamma-prim gamma-dec tc-insts (nth inst 4) dec-type nil type-tc-map)
          (recur (rest insts)))))))

(defn check-program-5tuple [lst]
  (let [gamma-dt   (nth lst 0)
        gamma-tc   (nth lst 1)
        gamma-prim (nth lst 2)
        gamma-dec  (nth lst 3)
        code-defs  (nth lst 4)
        tc-insts   (nth lst 5)
        constraint-decl (nth lst 6)
        type-tc-map (nth lst 7)
        type-names (environment-keys gamma-dec)
        code-names (environment-keys code-defs)]
    ; (println (pr-str "code defs " gamma-dec))
    (or (and (no-repeats? type-names)
             (no-repeats? code-names)
             (= (count type-names) (count code-names)))
        (throw (Exception. (str "Incompatible number of type declarations and variable definitions."))))
    ; (println (pr-str "gamma-tc " gamma-tc))
    (loop [tc-inst-keys (unique (environment-keys tc-insts))]
      (if (empty? tc-inst-keys)
        true
        (do
          (check-inst gamma-dt gamma-tc gamma-prim gamma-dec tc-insts (first tc-inst-keys) (lookup-environment* (first tc-inst-keys) tc-insts) type-tc-map)
          (recur (rest tc-inst-keys)))))
    (loop [code-defs-keys (environment-keys code-defs)]
      (if (empty? code-defs-keys)
        'type-table
        (let [id   (first code-defs-keys)
              exp  (lookup-environment id code-defs)
              type (lookup-environment id gamma-dec)
              constraints (try-lookup-environment id constraint-decl)]
          (let [theta (check-expression gamma-dt gamma-tc gamma-prim gamma-dec tc-insts exp type constraints type-tc-map)]
            (recur (rest code-defs-keys))))))))
