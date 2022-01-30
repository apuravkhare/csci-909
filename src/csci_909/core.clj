(ns csci-909.core
  (:gen-class)
  (:use [csci-909.interpreter])
  (:use [csci-909.textIo]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (= 1 (count args))
    (process-file (first args))
    (println "Usage: lein run: <filepath>")))
