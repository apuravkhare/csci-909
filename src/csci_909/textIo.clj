(ns csci-909.textIo
  ;; (:use [csci-909.interpreter])
  (:use [csci-909.interpreterTc])
  (:use [csci-909.checkerTfrOpt])
  (:use [csci-909.typeclassEnv])
  (:use [csci-909.transformer])
  (:use [csci-909.term]))

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
   [forms    (concat typeclassEnv (read-forms filepath))
    env      (init-env)
    all-decl (split-decl forms)
    ; type-check (try
    ;               (check-program-5tuple all-decl)
    ;               (catch Exception e (println (str "Type check failed: " e))))
    type-check (try
                 (check-and-transform-code all-decl forms)
                 (catch Exception e (println (str "Type check failed: " e)))
                 )
    ]
    ; (println (pr-str type-check))
    (reduce (fn [acc f]
              (conj acc (try
                          (println (str "> " (pr-str f)))
                          (let [m (meaning f env)]
                           (if (func? m)
                             (println 'closure)
                             (println m)))
                          ; (catch Exception e (println (str "Error: " e)))
                          )))
               '()
               ; @prog
            type-check)
    ))

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

