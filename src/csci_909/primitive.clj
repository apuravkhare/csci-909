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
   '*prim<i
   '*prim>i
   '*prim<=i
   '*prim>=i
   '*prim+d
   '*prim-d
   '*prim*d
   '*primdivd
   '*prim=d
   '*prim<d
   '*prim>d
   '*prim<=d
   '*prim>=d
   '*prim+str
   '*prim=str
   '*prim!bool
   '*prim-and
   '*prim-or
   '*prim-i-str
   '*prim-d-str
   '*prim=bool
   '*prim!=bool))

(def primitive-actions
  (list
   (make-primitive-action '*prim+i (fn [args] (apply + args)))
   (make-primitive-action '*prim-i (fn [args] (apply - args)))
   (make-primitive-action '*prim*i (fn [args] (apply * args)))
   (make-primitive-action '*primdivi (fn [args] (apply / args)))
   (make-primitive-action '*prim=i (fn [args] (apply = args)))
   (make-primitive-action '*prim<i (fn [args] (apply < args)))
   (make-primitive-action '*prim>i (fn [args] (apply > args)))
   (make-primitive-action '*prim<=i (fn [args] (apply <= args)))
   (make-primitive-action '*prim>=i (fn [args] (apply >= args)))
   (make-primitive-action '*prim+d (fn [args] (apply + args)))
   (make-primitive-action '*prim-d (fn [args] (apply - args)))
   (make-primitive-action '*prim*d (fn [args] (apply * args)))
   (make-primitive-action '*primdivd (fn [args] (apply / args)))
   (make-primitive-action '*prim=d (fn [args] (apply = args)))
   (make-primitive-action '*prim<d (fn [args] (apply < args)))
   (make-primitive-action '*prim>d (fn [args] (apply > args)))
   (make-primitive-action '*prim<=d (fn [args] (apply <= args)))
   (make-primitive-action '*prim>=d (fn [args] (apply >= args)))
   (make-primitive-action '*prim+str (fn [args] (apply str args)))
   (make-primitive-action '*prim=str (fn [args] (apply = args)))
   (make-primitive-action '*prim!bool (fn [args] (apply not args)))
   (make-primitive-action '*prim-and (fn [args] (reduce (fn [acc a] (and acc a)) true args)))
   (make-primitive-action '*prim-or (fn [args] (reduce (fn [acc a] (or acc a)) false args)))
   (make-primitive-action '*prim-i-str (fn [args] (apply str args)))
   (make-primitive-action '*prim-d-str (fn [args] (apply str args)))
   (make-primitive-action '*prim=bool (fn [args] (apply = args)))
   (make-primitive-action '*prim!=bool (fn [args] (apply = args)))))

(defn primitive-procedure? [p] (and (tagged-list? p) (in? primitives (nth p 1))))

(defn primitive-type? [t] (in? primitive-types t))

(def primitive-action-types
  (list 
   (list 'integer 'integer 'integer)
   (list 'integer 'integer 'integer)
   (list 'integer 'integer 'integer)
   (list 'integer 'integer 'integer)
   (list 'integer 'integer 'boolean)
   (list 'integer 'integer 'boolean)
   (list 'integer 'integer 'boolean)
   (list 'integer 'integer 'boolean)
   (list 'integer 'integer 'boolean)
   (list 'double 'double 'double)
   (list 'double 'double 'double)
   (list 'double 'double 'double)
   (list 'double 'double 'double)
   (list 'double 'double 'boolean)
   (list 'double 'double 'boolean)
   (list 'double 'double 'boolean)
   (list 'double 'double 'boolean)
   (list 'double 'double 'boolean)
   (list 'string 'string 'string)
   (list 'string 'string 'boolean)
   (list 'boolean 'boolean)
   (list 'boolean 'boolean 'boolean)
   (list 'boolean 'boolean 'boolean)
   (list 'integer 'string)
   (list 'double 'string)
   (list 'boolean 'boolean 'boolean)
   (list 'boolean 'boolean 'boolean)))