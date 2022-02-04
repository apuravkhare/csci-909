(ns csci-909.textIo
  (:use [csci-909.interpreter]))

(import java.io.PushbackReader)
(require '[clojure.edn :as edn])
(require '[clojure.java.io :as io])

(defn read-forms
  [file]
  (let [rdr (-> file io/file io/reader PushbackReader.)
        sentinel (Object.)]
    (loop [forms []]
      (let [form (edn/read {:eof sentinel} rdr)]
        (if (= sentinel form)
          forms
          (recur (conj forms form)))))))

(defn process-file
  [filepath]
  (let
   [forms (read-forms filepath)
    env (init-env)]
    (reduce (fn [acc f]
                (conj acc (try
                            (println (meaning f env))
                            (catch Exception e (println (str "Error: " e)))
                               )))
            '()
            forms)))

(defn run-repl
  []
  (println "Enter an expression. Type 'quit' to exit")
  (loop [env (init-env)]
    (print "> ")
    (flush)
    (let [in (read)]
      (if (= "quit" (str in))
        ()
        (if (> (count (str in)) 0)
         (let [res (try (meaning in env)
                        (catch Exception e (str "Error: " e)))]
          (println (str res))
          (recur env))
          (recur env))))))
