(ns csci-909.transformer
  (:use [csci-909.util])
  (:use [csci-909.term]))

(defn gen-inst-name
  [tc-name t-name f-name]
  (symbol (str tc-name "-" t-name "-" f-name)))

(defn transform-expression
  [exp]
  (cond (const? exp)    exp
        (variable? exp) exp
        (lambda? exp)   (let [args (arg1 exp)
                              e    (arg2 exp)]
                          (make-lambda args (transform-expression e)))
        (let? exp)      (let [x  (arg1 exp)
                              e  (arg2 exp)
                              e' (arg3 exp)]
                          (make-let x (transform-expression e) (transform-expression e')))
        (if-expr? exp)       (let [c (arg1 exp)
                              t (arg2 exp)
                              f (arg3 exp)]
                          (make-if-expr (transform-expression c) (transform-expression t) (transform-expression f)))
        (typeclass-inst? exp) (let [tc-name (arg1 exp)
                                    t (arg2 exp)
                                    fs (filter (fn [f] (not (type-decl? f))) (drop 3 exp))]
                                (map (fn [f] (make-define (gen-inst-name tc-name t (first f)) (make-lambda (arg1 f) (transform-expression (arg2 f))))) fs))
        :else                 exp))