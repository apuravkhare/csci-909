(ns csci-909.typeclassEnv)

(def typeclassEnv
  '((typeclass Num a
               (+ [a a a])
               (- [a a a])
               (* [a a a])
               (neg [a a]))
    (typeclass Eq a
               (== [a a boolean])
               (!= [a a boolean]))
    (typeclass Ord a
               (< [a a boolean])
               (> [a a boolean])
               (<= [a a boolean])
               (>= [a a boolean]))
    (typeclass-inst Num integer
                    (+ [x y] (*prim+i x y))
                    (- [x y] (*prim-i x y))
                    (* [x y] (*prim*i x y))
                    (neg [x] (*prim*i -1 x)))
    (typeclass-inst Eq integer
                    (== [x y] (*prim=i x y))
                    (!= [x y] (*prim!bool (*prim=i x y))))
    (typeclass-inst Ord integer
                    (< [x y] (*prim<i x y))
                    (> [x y] (*prim>i x y))
                    (<= [x y] (*prim<=i x y))
                    (>= [x y] (*prim>=i x y)))
    (typeclass-inst Num double
                    (+ [x y] (*prim+d x y))
                    (- [x y] (*prim-d x y))
                    (* [x y] (*prim*d x y))
                    (neg [x] (*prim*d -1.0 x)))

    (typeclass-inst Eq double
                    (== [x y] (*prim=d x y))
                    (!= [x y] (*prim!bool (*prim=d x y))))

    (typeclass-inst Ord double
                    (< [x y] (*prim<d x y))
                    (> [x y] (*prim>d x y))
                    (<= [x y] (*prim<=d x y))
                    (>= [x y] (*prim>=d x y)))))