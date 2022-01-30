(ns csci-909.util)

(defn lookup
  [pairs key]
  (loop [ps (reverse pairs)]
    (if (empty? ps)
      ; (throw (Exception. (str "Element not found: " key)))
      nil
      (if (= (first (first ps)) key)
        (second ps)
        (recur (rest ps))))))