(ns csci-909.typeclassEnv)

(def typeclassEnv
  '((typeclass Num a
               (+ [a a])
               (- [a a])
               (* [a a])
               (neg [a]))
    (typeclass Eq a
               (== [a a])
               (!= [a a]))
    (typeclass Ord a
               (< [a a])
               (> [a a])
               (<= [a a])
               (>= [a a]))
    (typeclass-inst Num integer
                    (+ [x y] (*prim+i x y))
                    (- [x y] (*prim-i x y))
                    (* [x y] (*prim*i x y))
                    (neg [x] (*prim*i -1 x)))
    (typeclass-inst Eq integer
                    (== [x y] (*prim=i x y))
                    (!= [x y] (*prim!bool (== x y))))
    (typeclass-inst Ord integer
                    (< [x y] (*prim<i x y))
                    (> [x y] (*prim>i x y))
                    (<= [x y] (*prim<=i x y))
                    (>= [x y] (*prim>=i x y)))
    (typeclass-inst Num double
                    (+ [x y] (*prim+d x y))
                    (- [x y] (*prim-d x y))
                    (* [x y] (*prim*d x y))
                    (neg [x] (*prim*d -1 x)))

    (typeclass-inst Eq double
                    (== [x y] (*prim=d x y))
                    (!= [x y] (*prim!bool (== x y))))

    (typeclass-inst Ord double
                    (< [x y] (*prim<d x y))
                    (> [x y] (*prim>d x y))
                    (<= [x y] (*prim<=d x y))
                    (>= [x y] (*prim>=d x y)))))