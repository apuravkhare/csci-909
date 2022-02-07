(ns csci-909.core-test
  (:require [clojure.test :refer :all]
            [csci-909.core :refer :all]))

(def files-to-test
  ["./test/csci_909/forms.txt"
   "./test/csci_909/data.txt"
   "./test/csci_909/functions.txt"
   "./test/csci_909/overloading1.txt"
   "./test/csci_909/overloading2.txt"
   "./test/csci_909/overloading3.txt"])

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
