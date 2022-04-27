(ns csci-909.core-test
  (:require [clojure.test :refer :all]
            [csci-909.core :refer :all]))

(def files-to-test
  ["./test/csci_909/pair.txt"
   "./test/csci_909/maybe.txt"
   "./test/csci_909/list.txt"
   "./test/csci_909/polynomial.txt"
   "./test/csci_909/typeclasses.txt"])

(deftest a-test
  (testing "Testing file exec"
    (is
     (loop [files files-to-test]
       (if (empty? files)
         true
         (do
           (println "Executing: " (first files))
           (csci-909.core/-main (first files))
           (recur (rest files))))))))
