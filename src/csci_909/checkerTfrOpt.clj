(ns csci-909.checkerTfrOpt
  (:use [csci-909.util])
  (:use [clojure.set])
  (:use [clojure.string :only (index-of)])
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
         l9 '() ; unbound vars
         l10 '() ; all data declarations
         ] 
    (if (empty? children)
      (list l1 l2 init-type-env l4 l5 l6 l7 l8 l9 l10)
      (let [node (first children)]
        (cond (data? node) (recur
                            (rest children)
                            (extend-environment (arg1 node) (make-data
                                                             (arg1 node)
                                                             (universalize-type (arg2 node))
                                                             (if (= (count node) 4) (universalize-type (arg3 node)) '()))l1)
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
                            l6 l7 l8 l9
                            (cons node l10))
              (data-adt? node) (let [dt-name (if (seq? (arg1 node)) (first (arg1 node)) (arg1 node))
                                      type-vars (if (seq? (arg1 node)) (rest (arg1 node)) '())
                                      ks      (drop 2 node)]
                                 ; (println (pr-str "ks " ks))
                                  (recur
                                   (rest children)
                                   (extend-environment*
                                    (map first ks)
                                    (map (fn [k] (make-adt-constructor
                                                  (first k)
                                                  dt-name
                                                  (if (> (count k) 1) (universalize-type (nth k 1)) '())
                                                  (if (> (count k) 2) (universalize-type (nth k 2)) '()))) ks)
                                    (extend-environment dt-name (make-data dt-name type-vars type-vars) l1))
                                   l2
                                   (loop [ks ks
                                          l4 (extend-environment*
                                              (map (fn [k] (symbol (str (first k) "?"))) ks)
                                              (map (fn [k] [(first k) 'boolean]) ks)
                                              l4)]
                                     (if (empty? ks)
                                       l4
                                       (recur (rest ks) (if (= (count (first ks)) 3)
                                                          (extend-environment*
                                                           (map (fn [arg] (symbol (str (first (first ks)) "-" arg))) (nth (first ks) 1))
                                                           (map (fn [t] [(first (first ks)) t]) (nth (first ks) 2))
                                                           l4)
                                                          l4))))
                                   (loop [ks ks
                                          l5 (extend-environment*
                                              (map (fn [k] (symbol (str (first k) "?"))) ks)
                                              (map (fn [k] (make-inst-predicate (first k))) ks)
                                              l5)]
                                     (if (empty? ks)
                                       l5
                                       (recur (rest ks) (if (= (count (first ks)) 3)
                                                          (extend-environment*
                                                           (map (fn [arg] (symbol (str (first (first ks)) "-" arg))) (nth (first ks) 1))
                                                           (map (fn [arg] (make-inst-accessor (first (first ks)) arg)) (nth (first ks) 1))
                                                           l5)
                                                          l5))))
                                   l6 l7 l8 l9
                                   (cons node l10)))
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
                                 l8
                                 l9 l10)
              (define? node) (recur
                                (rest children)
                                l1 l2 l4
                                (extend-environment (arg1 node) (arg2 node) l5)
                                l6 l7 l8 l9 l10)
              (typeclass? node) (let [tc (nth node 1)
                                      a (nth node 2)
                                      fs (drop 3 node)]
                                  (recur
                                   (rest children)
                                   l1
                                   (extend-environment* (map first fs) (map (fn [f] (make-overload-type tc a (universalize-type (second f)))) fs) l2)
                                   l4 l5 l6 l7 l8 l9 l10))
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
                                        (extend-environment t tc-name l8) l9 l10))
              :else (recur (rest children) l1 l2 l4 l5 l6 l7 l8 (cons node l9) l10))))))

;;;;; type checker

(defn gen-type-var [] (gensym "?t"))

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

(defn replace-typeclass-type
  [tc-fn t]
  (let [a (nth tc-fn 2)]
    (vec (find-replace a t (nth tc-fn 3)))))

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
        (if-expr? v)    (let [c (arg1 v)
                              t (arg2 v)
                              f (arg3 v)]
                          (lookup-environment-type t gamma-dec gamma-dt gamma-tc gamma-prim tc-insts))
        :else          (let [rator (first v)
                             rands (rest v)]
                         (cond (in? (environment-keys gamma-prim) rator) (last (lookup-environment rator gamma-prim))
                               (in? (environment-keys gamma-dt) rator)   (let [rator-def (lookup-environment rator gamma-dt)
                                                                               rator-type-name (if (data-adt-constructor? rator-def) (arg2 rator-def) rator)]
                                                                           rator-type-name)
                               (in? (environment-keys tc-insts) rator)   (last (arg3 (lookup-environment rator gamma-tc)))
                               (lambda? rator) (let [args (arg1 rator)
                                                     e    (arg2 rator)]
                                                 (lookup-environment-type e (extend-environment* args (map (fn [r] (lookup-environment-type r gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) rands) gamma-dec) gamma-dt gamma-tc gamma-prim tc-insts))
                               :else (lookup-environment-type rator gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)))))

(defn try-lookup-environment-type
  [v decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts]
  (cond (const? v)     (const-type v)
        (variable? v)  (if (in? (environment-keys gamma-dec) v) (lookup-environment v gamma-dec) nil)
        (lambda? v)    (let [args (arg1 v)
                             e    (arg2 v)]
                         (cond (and (seq? e) (lambda? e)) (try-lookup-environment-type e decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)
                               (seq? e)           (try-lookup-environment-type (first e) decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)
                               :else              (try-lookup-environment-type e decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)))
        :else          (let [rator (first v)
                             rands (rest v)]
                         (cond (in? (environment-keys gamma-prim) rator) (last (lookup-environment rator gamma-prim))
                               (in? (environment-keys gamma-dt) rator)   (let [rator-def (lookup-environment rator gamma-dt)
                                                                               rator-type-name (if (data-adt-constructor? rator-def) (arg2 rator-def) rator)]
                                                                           (if (data-adt-constructor? rator-def)
                                                                             (let [adt-type (lookup-environment rator-type-name gamma-dt)
                                                                                   rator-ts  (arg4 rator-def)
                                                                                   rator-ts-rands (map vector rator-ts rands)
                                                                                   rand-ts (map (fn [[t r]] (try-lookup-environment-type r t gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) rator-ts-rands)
                                                                                   mapped-ts (zipmap rator-ts rand-ts)]
                                                                               ; (concat (list rator-type-name) (map (fn [t] (if (nil? (get mapped-ts t)) t (get mapped-ts t))) (arg3 adt-type)))
                                                                               (concat
                                                                                (list rator-type-name)
                                                                                (concat (map (fn [t] (if (nil? (get mapped-ts t)) t (get mapped-ts t))) (arg3 adt-type))
                                                                                        rand-ts)))
                                                                             (concat (list rator-type-name) (map (fn [r] (try-lookup-environment-type r decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) rands))))
                               (in? (environment-keys tc-insts) rator)   (last (arg3 (lookup-environment rator gamma-tc)))
                               (lambda? rator) (let [args (arg1 rator)
                                                     e    (arg2 rator)]
                                                 (try-lookup-environment-type e decl-type (extend-environment* args (map (fn [r] (try-lookup-environment-type r decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) rands) gamma-dec) gamma-dt gamma-tc gamma-prim tc-insts))
                               :else (if (seq? (try-lookup-environment-type rator decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) (last (try-lookup-environment-type rator decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) (try-lookup-environment-type rator decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts))))))

(defn transform-call-tc-inst
  [exp decl-type]
  (let [rator          (first exp)
        rands          (rest exp)
        rator-arg-name (symbol (str rator ":" decl-type))]
    (make-lambda [rator-arg-name] (concat (list rator-arg-name) rands))))

(defn gen-tc-inst-arg-name
  [arg decl-type]
  (symbol (str arg ":" decl-type)))

(defn gen-overloaded-func-name
  [f-name type-name]
  (symbol (str "*" f-name ":" type-name "*")))

(defn overloaded-arg?
  [arg]
  (if (re-matches #".*:.*" (str arg))
    true
    false))

(defn parse-arg
  [arg]
  (let [colon-index (clojure.string/index-of (str arg) ":")]
   [(read-string (subs (str arg) 0 colon-index)) (read-string (subs (str arg) (+ colon-index 1)))])) ; [func type]

(defn collect-overloaded-args
  [exp code-defs]
  (cond (lambda? exp) (let [args (arg1 exp)]
                        (vec (unique (filter overloaded-arg? args))))
        (and (seq? exp)
             (in? (environment-keys code-defs) (first exp))) (collect-overloaded-args (lookup-environment (first exp) code-defs) code-defs)
        (and (variable? exp)
             (in? (environment-keys code-defs) exp)) (collect-overloaded-args (lookup-environment exp code-defs) code-defs)
        :else         '()))

(declare transform-exp)

(defn transform-rands
  [rands rand-types gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs]
  ; (map (fn [r] (transform-exp r decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map)) rands)
  (loop [rands rands
         rand-types rand-types
         tr-rands '()
         ov-args-all '()]
    (if (empty? rands)
      [(reverse tr-rands) (reverse ov-args-all)]
      (let [tr-rand (transform-exp (first rands) (first rand-types) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
            ov-args (collect-overloaded-args tr-rand code-defs)]
        (if (empty? ov-args)
          (recur (rest rands) (rest rand-types) (cons tr-rand tr-rands) ov-args-all)
          (recur (rest rands) (rest rand-types) (cons (arg2 tr-rand) tr-rands) (concat ov-args ov-args-all)))))))

(defn find-matching-inst
  [rator type rand-arg-types rand-actual-types gamma-dt gamma-tc type-tc-map]
  (let [matching (filter (fn [[arg-type rand]] (if (seq? arg-type) (in? arg-type type) (= type arg-type))) rand-arg-types)
        matching-types (map first matching)
        matching-args (map second matching)
        actual-type (unique (map second (filter (fn [[rand type]] (in? matching-args rand)) rand-actual-types)))
        matching-actual (unique (map second (filter (fn [[rand arg-type]] (if (seq? arg-type) (in? arg-type type) (= type arg-type))) rand-actual-types)))]
    ; (println "")
    ; (println (pr-str "rand-arg-types " rand-arg-types " rand-actual-types " rand-actual-types))
    ; (println (pr-str "matching " matching " actual-type " actual-type))
    ; (println (pr-str "type " type " actual-type " actual-type))
    ; (println (pr-str "matching-types " matching-types))
    (cond
      (defined-type? type gamma-dt gamma-tc type-tc-map) (gen-overloaded-func-name rator type)
      ; (> (count matching-actual) 0) (gen-overloaded-func-name rator type)
      (= (count actual-type) 1) (if (seq? (first actual-type))
                                  (if (seq? (first matching-types))
                                   (gen-overloaded-func-name rator (nth (first actual-type) (.indexOf (first matching-types) type)))
                                    (gen-overloaded-func-name rator type))
                                  (gen-overloaded-func-name rator (first actual-type)))
      (= (count matching-actual) 1) (gen-overloaded-func-name rator type)
      ; :else (throw (Exception. (pr-str "Invalid call to function " rator "(" rand-actual-types ")")))
      :else (gen-overloaded-func-name rator (first (first rand-arg-types))))))

; ([(List a) (Cons 1 (Cons 2 (Empty)))])
; ([(Cons 1 (Cons 2 (Empty))) (List integer)])
; (define fact (lambda [-:integer *:integer ==:integer] (lambda [x] (if (==:integer 0 x) 1 (*:integer x ((fact -:integer *:integer ==:integer) (-:integer x 1)))))))

(defn gen-fn-sub-name
  [f args]
  (loop [args args
         f-name (str f)]
    (if (empty? args)
      (symbol (str "*" (apply str f-name) "*"))
      (recur (rest args) (concat f-name (str (first args)))))))

(defn get-or-create-fn-instance
  [f args sub-fs]
  (let [f-name (gen-fn-sub-name f args)]
    ; (println (str "grn-fn " f-name))
    (if (in? (environment-keys sub-fs) f-name)
      f-name
      (do
        (define-variable! f-name (make-define f-name (concat (list f) args)) sub-fs)
        f-name))))

(defn transform-exp
  [exp decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs]
  (println (pr-str "Transform exp " exp " Type " decl-type))
  (cond (const? exp)    exp
        (variable? exp) exp
        (lambda? exp)   (let [args    (arg1 exp)
                              e       (arg2 exp)
                              e-type  (if (vector? decl-type) (last decl-type) decl-type)
                              e-type  (if (nil? (try-lookup-environment-type e e-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) e-type (try-lookup-environment-type e e-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts))
                              e'      (transform-exp e e-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
                              ov-args (unique (concat (collect-overloaded-args e' code-defs) (collect-overloaded-args e code-defs)))]
                          ; (println (pr-str "transformed lambda " e' " ov-args " ov-args))
                          (if (empty? ov-args)
                            exp
                            (make-lambda (vec ov-args) (make-lambda args (if (lambda? e') (arg2 e') e')))))
        (let? exp)      (let [x  (arg1 exp)
                              e  (arg2 exp)
                              e' (arg3 exp)
                              e-ov-args  (collect-overloaded-args e code-defs)
                              e (if (empty? e-ov-args) e (arg2 e))
                              e'-ov-args (collect-overloaded-args e' code-defs)
                              e' (if (empty? e'-ov-args) e' (arg2 e'))
                              ov-args (vec (clojure.set/union (set e-ov-args) (set e'-ov-args)))]
                          (if (empty? ov-args)
                            exp
                            (make-lambda ov-args (make-let x e e'))))
        (define? exp)   (let [v (arg1 exp)
                              e (arg2 exp)
                              ov-args (collect-overloaded-args e code-defs)]
                          (make-define v (transform-exp e decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)))
                          ; (if (empty? ov-args)
                          ;   exp
                          ;   (make-lambda ov-args (make-define v (arg2 e)))))
        (if-expr? exp)  (let [c (transform-exp (arg1 exp) decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
                              t (transform-exp (arg2 exp) decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
                              f (transform-exp (arg3 exp) decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
                              c-ov-args  (collect-overloaded-args c code-defs)
                              c (if (empty? c-ov-args) c (arg2 c))
                              t-ov-args  (collect-overloaded-args t code-defs)
                              t (if (empty? t-ov-args) t (arg2 t))
                              f-ov-args  (collect-overloaded-args f code-defs)
                              f (if (empty? f-ov-args) f (arg2 f))
                              ov-args (vec (clojure.set/union (set c-ov-args) (set t-ov-args) (set f-ov-args)))]
                          (if (empty? ov-args)
                            exp
                            (make-lambda ov-args (make-if-expr c t f))))
        (cond? exp)      (loop [cases    (rest exp)
                                tr-cases '()
                                ov-args-all '()]
                           (if (empty? cases)
                             (if (empty? ov-args-all)
                               exp
                               (make-lambda ov-args-all (make-cond tr-cases)))
                             (let [c (transform-exp (first cases) 'boolean gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
                                   e (transform-exp (second cases) decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
                                   c-ov-args  (collect-overloaded-args c code-defs)
                                   c (if (empty? c-ov-args) c (arg2 c))
                                   e-ov-args  (collect-overloaded-args e code-defs)
                                   e (if (empty? e-ov-args) e (arg2 e))
                                   ov-args (clojure.set/union (set c-ov-args) (set e-ov-args))]
                               (recur (rest (rest cases))
                                      (concat tr-cases (list c e))
                                      (unique (concat ov-args-all ov-args))))))
        (inst-accessor? exp) exp
        (inst-predicate? exp) exp
        :else (let [rator (first exp)
                    rands (rest exp)]
                ; (println (pr-str "transform call " exp " type " decl-type))
                (cond (in? (environment-keys gamma-prim) rator)
                      (let [prim-types (lookup-environment rator gamma-prim)
                            [tr-rands ov-args] (transform-rands rands (take (- (count prim-types) 1) prim-types) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)]
                        (if (empty? ov-args)
                          ; exp
                          (concat (list rator) tr-rands)
                          (make-lambda ov-args (concat (list rator) tr-rands))))
                      (in? (environment-keys gamma-dt) rator)
                      (let [arg-types (if (data-adt-constructor? (lookup-environment rator gamma-dt)) (arg4 (lookup-environment rator gamma-dt)) (arg3 (lookup-environment rator gamma-dt)))
                            [tr-rands ov-args] (transform-rands rands arg-types gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)]
                        (if (empty? ov-args)
                          ; exp
                          (concat (list rator) tr-rands)
                          (make-lambda ov-args (concat (list rator) tr-rands))))
                      (in? (environment-keys tc-insts) rator) (let [arg-types (if (data-adt-constructor? (lookup-environment rator gamma-tc)) (arg4 (lookup-environment rator gamma-tc)) (arg3 (lookup-environment rator gamma-tc)))
                                                                    new-rator (gen-tc-inst-arg-name rator decl-type)
                                                                    [tr-rands ov-args] (transform-rands rands (take (- (count arg-types) 1) arg-types) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
                                                                    ov-args (vec (unique (cons new-rator ov-args)))
                                                                    tr (make-lambda ov-args (concat (list new-rator) tr-rands))
                                                                    rand-ts (map (fn [r] (try-lookup-environment-type r decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) tr-rands)]
                                                                (println (pr-str "rand-ts " rand-ts))
                                                                
                                                                (if 
                                                                 ; (every? (fn [r] (and (not (nil? r)) (not (vector? r)) (= (count (unique rand-ts)) 1))) rand-ts)
                                                                 (every? (fn [r] (and (not (nil? r)) (not (vector? r)))) rand-ts)
                                                                  ; (transform-exp (concat (list (gen-overloaded-func-name rator (if (seq? (first rand-ts)) (first (first rand-ts)) (first rand-ts)))) rands) decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
                                                                  (loop [insts (lookup-environment* rator tc-insts)]
                                                                    (if (empty? insts)
                                                                      (throw (Exception. (pr-str "No matching instance for " rator)))
                                                                      (let [res (loop [dec-ts    (take (count rand-ts) (replace-typeclass-type (lookup-environment rator gamma-tc) (arg2 (first insts))))
                                                                                       actual-ts rand-ts]
                                                                                  (println (pr-str "dec-ts " dec-ts " actual-ts " actual-ts))
                                                                                  (if (empty? dec-ts)
                                                                                    (arg2 (first insts))
                                                                                   (if (seq? (first actual-ts))
                                                                                    (if (= (first dec-ts) (first (first actual-ts)))
                                                                                      (recur (rest dec-ts) (rest actual-ts))
                                                                                      false)
                                                                                    (if (= (first dec-ts) (first actual-ts))
                                                                                      (recur (rest dec-ts) (rest actual-ts))
                                                                                      false))))]
                                                                        (if res (transform-exp (concat (list (gen-overloaded-func-name rator res)) rands) decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs) (recur (rest insts))))))
                                                                 tr)
                                                                ; tr
                                                                )
                      (lambda? rator) (let [args (arg1 rator)
                                            ; e    (arg2 exp)
                                            ; e-type (lookup-environment-type e gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)
                                            e-type decl-type]
                                        (if (every? overloaded-arg? args)
                                          (let [rand-types (map (fn [r] (try-lookup-environment-type r decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)) rands)]
                                            (if (some nil? rand-types)
                                              (let [[tr-rands ov-args] (transform-rands rands (take (- (count e-type) 1) e-type) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)]
                                                   (if (empty? ov-args)
                                                     ; exp
                                                     (concat (list rator) tr-rands)
                                                     (make-lambda ov-args (concat (list rator) tr-rands))))
                                              (let [overloaded-args (map parse-arg args)
                                                    ; no (println (pr-str "e-type " e-type))
                                                    ; rand-arg-types (map vector (take (- (count e-type) 1) e-type) rands)
                                                    rand-arg-types (map vector e-type rands)
                                                    rand-actual-types (map vector rands rand-types)]
                                                ; (println (pr-str "rand-arg-types " rand-arg-types "rand-actual-types " rand-actual-types))
                                                (loop [overloaded-args overloaded-args
                                                       matched-operands '()]
                                                  ; (println (pr-str "overloaded-arg " (first overloaded-args)))
                                                  (if (empty? overloaded-args)
                                                    ; optimization: replace below with a function name
                                                    (concat (list rator) (reverse matched-operands))
                                                    ; (get-or-create-fn-instance rator (reverse matched-operands) sub-fs)
                                                    (recur (rest overloaded-args)
                                                           (cons
                                                            (find-matching-inst
                                                             (first (first overloaded-args))
                                                             (second (first overloaded-args))
                                                             rand-arg-types rand-actual-types
                                                             gamma-dt gamma-tc type-tc-map) matched-operands)))))))
                                          (let [[tr-rands ov-args] (transform-rands rands (take (- (count e-type) 1) e-type) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)]
                                            (if (empty? ov-args)
                                              ; exp
                                              (concat (list rator) tr-rands)
                                              (make-lambda ov-args (concat (list rator) tr-rands))))))
                      (inst-accessor? rator) (let [accessor-name (symbol (str (arg1 rator) "-" (arg2 rator)))
                                                   arg-types (lookup-environment accessor-name gamma-dec)
                                                   [tr-rands ov-args] (transform-rands rands (take (- (count arg-types) 1) arg-types) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)]
                                               (if (empty? ov-args)
                                                 ; exp
                                                 (concat (list accessor-name) tr-rands)
                                                 (make-lambda ov-args (concat (list accessor-name) tr-rands))))
                      (inst-predicate? rator) (let [pred-name (symbol (str (arg1 rator) "?"))
                                                    arg-types (lookup-environment pred-name gamma-dec)
                                                    [tr-rands ov-args] (transform-rands rands (take (- (count arg-types) 1) arg-types) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)]
                                                (if (empty? ov-args)
                                                 ; exp
                                                  (concat (list pred-name) tr-rands)
                                                  (make-lambda ov-args (concat (list pred-name) tr-rands))))
                      :else (let [f-def (lookup-environment rator code-defs)
                                  transformed (transform-exp (concat (list f-def) rands) (lookup-environment rator gamma-dec) gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)]
                              ; (println (pr-str "transformed " transformed))
                              (if (every? overloaded-arg? (rest transformed))
                                ; optimization: replace below with function name called with the passed rands
                                ; (concat (list (concat (list rator) (rest transformed))) rands)
                                (concat (list (get-or-create-fn-instance rator (rest transformed) sub-fs)) rands)
                                (if (= exp transformed)
                                  exp
                                  (let [arg-types (lookup-environment-type rator gamma-dec gamma-dt gamma-tc gamma-prim tc-insts)
                                        [tr-rands ov-args] (transform-rands rands arg-types gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)
                                        rator-ov-args (collect-overloaded-args rator code-defs)]
                                    ; (println (pr-str "rator-ov-args " rator-ov-args " ov-args " ov-args " rator " rator))
                                    (if (empty? ov-args)
                                      ; exp
                                      (if (empty? rator-ov-args)
                                        (concat (list rator) tr-rands)
                                        (make-lambda (vec rator-ov-args) (concat (list (concat (list rator) rator-ov-args)) tr-rands)))
                                      (make-lambda ov-args (concat (list rator) tr-rands))))))
                              ; (if (or (not (every? overloaded-arg? (rest transformed))) (= exp transformed))
                              ;   exp
                              ;   (concat (list (concat (list rator) (rest transformed))) rands))
                              )))))

(defn transform-rec-call
  [exp f-name args]
  (cond (seq? exp) (if (= (first exp) f-name)
                     (concat (list (concat (list f-name) args)) (map (fn [a] (transform-rec-call a f-name args)) (rest exp)))
                     (map (fn [a] (transform-rec-call a f-name args)) exp))
        :else exp))

(defn transform-rec-call2
  [id exp code-defs]
  (let [args (collect-overloaded-args exp code-defs)]
    (if (empty? args)
      exp
      (transform-rec-call exp id args))))

(defn check-type
  [exp decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints]
  ; (println (str "Check type " exp " " decl-type))
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
        (cond? exp)    (let [cases (rest exp)]
                         (if (= (mod (count cases) 2) 0)
                           (loop [cases cases]
                             (if (empty? cases)
                               decl-type
                              (do
                                (check-type (first cases) 'boolean gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                                (check-type (second cases) decl-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
                                (recur (rest (rest cases))))))
                           (throw (Exception. (str "Even number of arguments expected in cond ")))))
        (inst-accessor? exp) decl-type
        (inst-predicate? exp) decl-type
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
                                                                    ; (transform-call-tc-inst exp decl-type)
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

(defn check-expression [gamma-dt gamma-tc gamma-prim gamma-dec tc-insts exp type constraints type-tc-map code-defs sub-fs]
  (let [simple-type (if (forall-type? type) (arg2 type) type)
        ; history (extend-history '() exp type)
        ; theta (judge-type gamma-dt gamma-con gamma-prim gamma-dec tc-insts exp simple-type theta-identity history constraints type-tc-map)
        theta (check-type exp simple-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map constraints)
        transformed (transform-exp exp simple-type gamma-dec gamma-dt gamma-tc gamma-prim tc-insts type-tc-map code-defs sub-fs)]
    ; (println (pr-str "Transformed " transformed))
    transformed))

(defn check-inst [gamma-dt gamma-tc gamma-prim gamma-dec tc-insts inst-name insts type-tc-map code-defs sub-fs]
  (let [tc-fn (lookup-environment inst-name gamma-tc)]
    (loop [insts insts
           transformed-insts '()]
      (if (empty? insts)
        transformed-insts
        (let [inst (first insts)
              dec-type (replace-typeclass-type tc-fn (nth inst 2))
              new-name (gen-overloaded-func-name (arg3 inst) (arg2 inst))
              mod-exp (make-define new-name (arg4 inst))
              theta (check-expression gamma-dt gamma-tc gamma-prim gamma-dec tc-insts mod-exp dec-type nil type-tc-map code-defs sub-fs)
              theta (transform-rec-call2 (arg3 inst) (arg2 theta) code-defs)]
          (recur (rest insts) (cons (make-define new-name theta) transformed-insts)))))))

(defn check-inst2 [gamma-dt gamma-tc gamma-prim gamma-dec tc-insts inst-name inst type-tc-map code-defs sub-fs t]
  (let [tc-fn (lookup-environment inst-name gamma-tc)
        dec-type (replace-typeclass-type tc-fn t)
        new-name (gen-overloaded-func-name inst-name t)
        lambda-form (make-lambda (arg1 inst) (arg2 inst))
        ; mod-exp (make-define new-name lambda-form)
        theta (check-expression gamma-dt gamma-tc gamma-prim gamma-dec tc-insts lambda-form dec-type nil type-tc-map code-defs sub-fs)
        theta (make-define new-name (transform-rec-call2 new-name theta code-defs))
        ; no (println (pr-str "theta " theta))
        ]
    [theta, dec-type]))

(defn check-and-transform-code
  [lst forms]
  (let [gamma-dt   (nth lst 0)
        gamma-tc   (nth lst 1)
        gamma-prim (nth lst 2)
        gamma-dec  (nth lst 3)
        code-defs  (nth lst 4)
        tc-insts   (nth lst 5)
        constraint-decl (nth lst 6)
        type-tc-map (nth lst 7)
        unbound-vars (nth lst 8)
        data-defs    (nth lst 9)
        type-names (environment-keys gamma-dec)
        code-names (environment-keys code-defs)
        sub-fs     (extend-environment* '() '() (global-environment))]
    (or (and (no-repeats? type-names)
             (no-repeats? code-names)
             (= (count type-names) (count code-names)))
        (throw (Exception. (str "Incompatible number of type declarations and variable definitions."))))
    (let [transformed-code '() ; (loop [tc-inst-keys (unique (environment-keys tc-insts))
                           ;        transformed-code' ()]
                           ;   (if (empty? tc-inst-keys)
                           ;     transformed-code'
                           ;     (recur (rest tc-inst-keys) (concat transformed-code' (check-inst gamma-dt gamma-tc gamma-prim gamma-dec tc-insts (first tc-inst-keys) (lookup-environment* (first tc-inst-keys) tc-insts) type-tc-map code-defs sub-fs)))))
          ]
      (loop [forms forms
             transformed-code' (vec transformed-code)
             sub-fs-keys '()
             code-defs' code-defs
             gamma-dec' gamma-dec]
        (if (empty? forms)
          transformed-code'
          (let [form (first forms)]
            ; (println (str "transform " form))
            (cond (or (const? form) (variable? form) (overload? form) (prog-inst? form) (data? form) (data-fn? form) (data-adt? form)) (recur (rest forms) (conj transformed-code' form) sub-fs-keys code-defs' gamma-dec')
              (or (type-decl? form) (typeclass? form)) (recur (rest forms) transformed-code' sub-fs-keys code-defs' gamma-dec')
                  (typeclass-inst? form) (let [tr-insts (map (fn [inst] (check-inst2 gamma-dt gamma-tc gamma-prim gamma-dec' tc-insts (first inst) inst type-tc-map code-defs' sub-fs (arg2 form))) (drop 3 form))]
                                          (recur (rest forms)
                                                (vec (concat transformed-code' (map first tr-insts)))
                                                (vec (environment-keys sub-fs))
                                                (extend-environment* (map arg1 (map first tr-insts)) (map arg2 (map first tr-insts)) code-defs')
                                                 (extend-environment* (map arg1 (map first tr-insts)) (map second tr-insts) gamma-dec')))
                  (define? form)   (let [id (arg1 form)
                                         exp  (lookup-environment id code-defs')
                                         type (if (in? (environment-keys gamma-dec') id) (lookup-environment id gamma-dec') (gen-type-var))
                                         constraints (try-lookup-environment id constraint-decl)
                                         transformed (check-expression gamma-dt gamma-tc gamma-prim gamma-dec' tc-insts exp type constraints type-tc-map code-defs' sub-fs)
                                         transformed (transform-rec-call2 id transformed code-defs')]
                                     (recur (rest forms)
                                            (conj (vec (concat transformed-code'
                                                               (map (fn [sf] (lookup-environment sf sub-fs)) (filter (fn [sf] (not (in? sub-fs-keys sf))) (environment-keys sub-fs)))))
                                                  (make-define id transformed))
                                            (vec (environment-keys sub-fs))
                                            (extend-environment id transformed code-defs')
                                            gamma-dec'))
                  :else            (let [transformed (check-expression gamma-dt gamma-tc gamma-prim gamma-dec' tc-insts form (gen-type-var) nil type-tc-map code-defs' sub-fs)]
                                     (recur (rest forms)
                                            (conj (vec (concat transformed-code'
                                                               (map (fn [sf] (lookup-environment sf sub-fs)) (filter (fn [sf] (not (in? sub-fs-keys sf))) (environment-keys sub-fs)))))
                                                  transformed)
                                            (vec (environment-keys sub-fs))
                                            code-defs' gamma-dec')))))))))

