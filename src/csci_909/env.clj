(ns csci-909.env)

(def empty-environment (list 'empty))

(defn empty-environment? [e] (= 'empty e))

(defn make-frame
  [vars vals]
  (list (java.util.LinkedList. vars) (java.util.LinkedList. vals)))

(defn extend-environment*
  "add a new layer to the environment containing the given variables and their values."
  [vars vals base-env]
  (if (= (count vars) (count vals))
    (conj base-env (make-frame vars vals))
    (throw (Exception. "Mismatching arguments passed to extend-environment"))))

(defn extend-environment
  "add a new layer to the environment containing the given variable and its value."
  [var val base-env]
  (extend-environment* [var] [val] base-env))

(defn lookup-environment
  "finds the first occurrence of the key in the environment.
   throws an exception if the key is not found."
  [var env]
  (loop [pairs env]
    (if (empty-environment? (first pairs))
      (throw (Exception. (str "Unbound variable " var)))
      (let
        [res (loop [keys (first (first pairs))
                    vals (second (first pairs))]
               (if (empty? keys)
                 nil
                 (if (= var (first keys))
                   (first vals)
                   (recur (rest keys) (rest vals)))))]
        (if res res (recur (rest pairs)))))))

(defn try-lookup-environment
  "finds the first occurrence of the key in the environment.
   returns nil if the key is not found."
  [var env]
  (loop [pairs env]
    (if (empty-environment? (first pairs))
      nil
      (let
        [res (loop [keys (first (first pairs))
                    vals (second (first pairs))]
               (if (empty? keys)
                 nil
                 (if (= var (first keys))
                   (first vals)
                   (recur (rest keys) (rest vals)))))]
        (if res res (recur (rest pairs)))))))

(defn lookup-environment*
  "finds all instances of the key in the environment."
  [var env]
  (loop [pairs env
         insts '()]
    (if (empty-environment? (first pairs))
      (reverse (filter (fn [v] (and (seq? v) (not (empty? v)))) insts))
       (let [res (loop [keys (first (first pairs))
                        vals (second (first pairs))
                        insts' insts]
                   (if (empty? keys)
                       insts'
                     (if (= var (first keys))
                       (recur (rest keys) (rest vals) (cons (first vals) insts'))
                       (recur (rest keys) (rest vals) insts'))))]
         (recur (rest pairs) res)))))

(defn lookup-environments
  "finds the first occurrence of the key across multiple environments.
   throws an exception if the key is not found."
  [envs var]
  (loop [envs envs]
    (if (empty? envs)
      (throw (Exception. (str "Unbound variable " var)))
   (let [res' (loop [pairs (first envs)]
    (if (empty-environment? (first pairs))
      nil
      (let
       [res (loop [keys (first (first pairs))
                   vals (second (first pairs))]
              (if (empty? keys)
                nil
                (if (= var (first keys))
                  (first vals)
                  (recur (rest keys) (rest vals)))))]
        (if res res (recur (rest pairs))))))]
     (if res' res' (recur (rest envs)))))))

(defn define-variable!
  "add a variable definition to the topmost environment/redefine if one already exists."
  [var val env]
  (loop [pairs env]
    (if (empty-environment? (first pairs))
      (let [keys (first (first env))
            vals (second (first env))]
        (.addFirst keys var)
        (.addFirst vals val)
        env)
      (let
        [res (loop [keys (first (first pairs))
                    vals (second (first pairs))
                    index 0]
                (if (empty? keys)
                  nil
                  (if (= var (first keys))
                    (do
                      (.set vals index val)
                      env)
                    (recur (rest keys) (rest vals) (+ index 1)))))]
        (if res res (recur (rest pairs)))))))

(defn define-variable*!
  [vars vals env]
  (if (= (count vars) (count vals))
   (loop [vars vars
         vals vals]
     (if (empty? vars)
       env
    (do (define-variable! (first vars) (first vals) env)
        (recur (rest vars) (rest vals)))))
    (throw (Exception. "Mismatched variables passed to define-variable*!"))))

(defn overload-variable!
  "add a variable definition to the topmost environment, doesn't rewrite if any already exist."
  [var val env]
  (let [keys (first (first env))
        vals (second (first env))]
    (.addFirst keys var)
    (.addFirst vals val)
    env))

(defn global-environment [] empty-environment)

(defn environment-keys
  [env]
  (loop [pairs env
         ks    '()]
    (if (empty-environment? (first pairs))
      ks
      (recur (rest pairs) (concat (first (first pairs)) ks)))))