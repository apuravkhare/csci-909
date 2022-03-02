(ns csci-909.transformer
  (:use [csci-909.util])
  (:use [csci-909.term])
  (:use [csci-909.env])
  (:use [clojure.set]))

(defn gen-inst-name
  [tc-name t-name f-name]
  (symbol (str "*" tc-name "-" t-name "-" f-name)))

(declare transform-expression)

(defn check-lambda-tc-rec? [all-decl l]
  (let [args (nth l 1)
        e    (nth l 2)]
    (or
     (subset? (set args) (set (environment-keys (nth all-decl 5))))
     (and (lambda? e) (check-lambda-tc-rec? all-decl e)))))

(defn is-tc-inst? [all-decl f]
  (in? (environment-keys (nth all-decl 5)) f))

(defn all-vals? [vs]
  (reduce (fn [acc v] (and acc (or (const? v) (data-inst? v)))) true vs))

(defn create-typeclass-inst-type
  [tc-fn t]
  (let [a (nth tc-fn 2)]
    (find-replace a t (nth tc-fn 3))))

;; (defn match-tc-inst [rator rands all-decl]
;;     (if (all-vals? rands)
;;      (loop [insts (lookup-environment* rator (nth all-decl 5))]
;;       (if (= 0 (count insts))
;;         (throw (Exception. (str "No overload found with the given arguments for " (str exp))))
;;         (let [inst (first insts)
;;               inst-type (create-typeclass-inst-type (lookup-environment rator (nth all-decl 1)) (nth inst 2))
;;               vts  (get-inst-type* rands)]
;;           (if (= vts (take (- (count inst-type) 1) inst-type))
;;             (list (make-lambda [rator] (concat (list rator) (map (fn [rand] transform-expression rand) rands))) (gen-inst-name (nth inst 1) (nth inst 2) rator))
;;             (recur (rest insts))))))
;;         (concat (list rator) rands)))

;; (defn transform-call [exp all-decl env]
;;   (let [rator (first exp)
;;         rands (rest exp)]
;;    (cond (is-tc-inst? all-decl rator) (match-tc-inst exp all-decl env)
;;          (lambda? rator) (let [args (second (rator))
;;                                rev-args (if (all-vals? rands)
;;                                           (map (fn [arg] (if (is-tc-inst? all-decl rator)
;;                                                            (last (match-tc-inst (list arg rands) all-decl env))
;;                                                            arg)) args)
;;                                           args)]
;;                            (list exp rev-args))
;;          :else (concat (list rator) (map (fn [rand] transform-expression rand) rands)))))

(defn extend-prog [extend? prog val] (if extend? (atom (swap! prog concat (list val))) prog))

(defn extend-prog* [extend? prog vals] (if extend? (atom (swap! prog concat vals)) prog))

(defn create-data-inst
  [p all-decl]
  (let [k    (first p)
        args (rest p)
        c (lookup-environment k (nth all-decl 0))]
    (make-data-inst k (zipmap (nth c 2) args))))

(defn exec-accessor 
  [ac p]
  (let [t (nth ac 1)
        a (nth ac 2)]
      (if (= (get-inst-type p) t)
        (get (nth p 2) a)
        (throw (Exception. "Record accessor applied to invalid type.")))))

(defn transform-expression
  [exp all-decl env prog extend? history]
  ; (println (pr-str "Transform " exp))
  (cond (const? exp)     (do (extend-prog extend? prog exp) exp)
        (data-inst? exp) (do (extend-prog extend? prog exp) exp)
        (variable? exp)  (do (extend-prog extend? prog exp) exp)
        (data? exp)      (do (extend-prog extend? prog exp) exp)
        (typeclass? exp) nil
        (type-decl? exp) nil
        (let? exp)      (let [x  (arg1 exp)
                              e  (arg2 exp)
                              e' (arg3 exp)
                              exp' (make-let x (transform-expression e all-decl env prog false history) (transform-expression e' all-decl env prog false history))]
                          (extend-prog extend? prog exp')
                          exp')
        (if-expr? exp)       (let [c (arg1 exp)
                              t (arg2 exp)
                              f (arg3 exp)
                              exp' (make-if-expr (transform-expression c all-decl env prog false history) (transform-expression t all-decl env prog false history) (transform-expression f all-decl env prog false history))]
                               (extend-prog extend? prog exp')
                               exp')
        (typeclass-inst? exp) (let [tc-name (arg1 exp)
                                    t (arg2 exp)
                                    fs (filter (fn [f] (not (type-decl? f))) (drop 3 exp))
                                    exp' (map (fn [f] (make-define (gen-inst-name tc-name t (first f)) (make-lambda (arg1 f) (arg2 f)))) fs)]
                                (extend-prog* extend? prog exp')
                                exp')
        (lambda? exp)         (let [args (arg1 exp)
                                    e    (arg2 exp)
                                    exp' (make-lambda args e)]
                                (extend-prog extend? prog exp')
                                exp')
        (define? exp)         (let [v (arg1 exp)
                                    e (arg2 exp)
                                    exp' (make-define v (transform-expression e all-decl env prog false history))]
                                (define-variable! v (arg2 exp') env)
                                (extend-prog extend? prog exp')
                                exp')
        :else                 (let [e   (nth exp 0)
                                    e's (drop 1 exp)]
                                (cond
                                  (and (in? (environment-keys env) e) (primitive-action? (lookup-environment e env))) (do (extend-prog extend? prog exp) exp)
                                  (is-tc-inst? all-decl e) (let [vs (map (fn [e'] (transform-expression e' all-decl env prog false history)) (map (fn [e'] (if (in? (environment-keys env) e') (lookup-environment e' env) e')) e's))
                                                                 vs (map (fn [v] (if (seq? v)
                                                                                   (if (inst-accessor? (lookup-environment (first v) (nth all-decl 4)))
                                                                                     (exec-accessor (lookup-environment (first v) (nth all-decl 4)) (create-data-inst (lookup-environment (second v) env) all-decl))
                                                                                     v)
                                                                                   v)) vs)]
                                                             (loop [insts (lookup-environment* e (nth all-decl 5))]
                                                               (if (= 0 (count insts))
                                                                 (throw (Exception. (str "No overload found with the given arguments for " (str e))))
                                                                 (let [vts (get-inst-type* vs)
                                                                       inst (first insts)
                                                                       inst-type (create-typeclass-inst-type (lookup-environment e (nth all-decl 1)) (nth inst 2))]
                                                                   (if (= vts (take (- (count inst-type) 1) inst-type))
                                                                     (let
                                                                       [exp' (concat (list (gen-inst-name (nth inst 1) (nth inst 2) e)) vs)]
                                                                       (extend-prog extend? prog exp')
                                                                       exp')
                                                                     (recur (rest insts)))))))
                                  (lambda? e) (let [args (arg1 e)
                                                    b    (arg2 e)]
                                                (if (seq? b)
                                                  (let [exp' (transform-expression b all-decl (extend-environment* args (map (fn [e'] (transform-expression e' all-decl env prog false history)) e's) env) prog false history)]
                                                    (extend-prog extend? prog exp')
                                                    exp')
                                                  (do (extend-prog extend? prog exp) exp)))
                                  (in? (environment-keys (nth all-decl 0)) e) (let [exp' (concat (list e) (map (fn [e'] (transform-expression e' all-decl env prog false history)) e's))]
                                                                                ; (define-variable! exp' (make-data-inst e (zipmap (nth (lookup-environment e (nth all-decl 0)) 2) (map (fn [e'] (transform-expression e' all-decl env prog false history)) e's))) env)
                                                                                (extend-prog extend? prog exp')
                                                                                exp')
                                  :else (let [f (lookup-environment e (nth all-decl 4))]
                                          (if (inst-accessor? f)
                                            (do
                                              ; (define-variable! exp (exec-accessor f (if (in? (environment-keys env) (first e's)) (lookup-environment (first e's) env) (first e's))) env)
                                              (extend-prog extend? prog exp)
                                              exp)
                                            (if (in? history e)
                                              (let [exp' (concat (list e (map (fn [e'] (transform-expression e' all-decl env prog false history)) (map (fn [e'] (if (in? (environment-keys env) e') (lookup-environment e' env) e')) e's))))]
                                              (extend-prog extend? prog exp')
                                              exp')
                                              (let [exp' (transform-expression (concat (list f) e's) all-decl env prog false (cons e history))]
                                                (extend-prog extend? prog exp')
                                                exp')))))
                                )))
