(data Color [char] (fn [c] c))

(data Point [double double] (fn [x y] (list x y)))

(data CPoint [double double Color] (fn [x y c] (list x y c)))

(inst xcoord [Point double] (lambda p (op * p 2)))

(inst xcoord [CPoint double] (lambda p (op * p 2)))

