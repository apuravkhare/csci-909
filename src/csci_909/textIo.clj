(ns csci-909.textIo
  (:import '[java.io.PushbackReader])
  (:require '[clojure.java.io :as io])
  (:require '[clojure.ed :as edn])
  (:use [csci-909.interpreter])
  (:use [csci-909.parser]))

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
    (loop [fs forms
           ms '()]
      (recur (rest forms) (conj ms (meaning (first (parse fs)) env))))))

