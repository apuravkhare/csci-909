(ns csci-909.textIo
  (:use [csci-909.interpreter])
  (:use [csci-909.parser]))

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
    (reduce (fn [acc f] (conj acc (meaning (parse f) env))) '() forms)))

;; (defn process-file
;;   [filepath]
;;   (let
;;    [forms (read-forms filepath)
;;     env (init-env)]
;;     (loop [fs forms
;;            ms '()]
;;       (recur (rest forms) (conj ms (meaning (let [ps (parse (first fs))]
;;                                                 (println "parsed")
;;                                               ps) env))))))

