(ns csci-909.primitive
  (:use [csci-909.term])
  (:use [csci-909.util]))

(def primitive-types
  (list 'boolean 'integer 'double 'char 'string))

(def primitives
  (list
   '*prim+i
   '*prim-i
   '*prim*i
   '*primdivi
   '*prim=i
   '*prim+d
   '*prim-d
   '*prim*d
   '*primdivd
   '*prim=d
   ; '*item
   '*prim+str))

(def primitive-actions
  (list
   (make-primitive-action '*prim+i (fn [args] (apply + args)))
   (make-primitive-action '*prim-i (fn [args] (apply - args)))
   (make-primitive-action '*prim*i (fn [args] (apply * args)))
   (make-primitive-action '*primdivi (fn [args] (apply / args)))
   (make-primitive-action '*prim=i (fn [args] (apply = args)))
   (make-primitive-action '*prim+d (fn [args] (apply + args)))
   (make-primitive-action '*prim-d (fn [args] (apply - args)))
   (make-primitive-action '*prim*d (fn [args] (apply * args)))
   (make-primitive-action '*primdivd (fn [args] (apply / args)))
   (make-primitive-action '*prim=d (fn [args] (apply = args)))
   ; (make-primitive-action '*item (fn [args] (if (and (= 2 (count args)) (data-inst? (first args)) (integer? (second args)))
   ;                                           (nth (nth (first args) 2) (second args))
   ;                                           (throw (Exception. "Invalid input to function 'item'")))))
   (make-primitive-action '*prim+str (fn [args] (apply str args)))))

(defn primitive-procedure? [p] (and (tagged-list? p) (in? primitives (nth p 1))))

(defn primitive-type? [t] (in? primitive-types t))