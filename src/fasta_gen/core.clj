(ns fasta-gen.core
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

(def random
  (let [a 69069
        c 1
        m (math/expt 2 32)
        seed (atom 19380110)]
    (fn [new-seed]
      (if (not (nil? new-seed))
        (reset! seed (first new-seed))
        (swap! seed #(mod (+ (* % a) c) m)))
      (float (/ @seed m)))))
      
;; From https://en.wikipedia.org/wiki/Nucleic_acid_notation
;;Full spec includes 
;(def n-bases ["A" "C" "G" "T" "U" "W" "S" "M" "K" "R" "Y" "B" "D" "H" "V" "N"])
(def n-bases ["A" "C" "G" "T"])

(defn random-base []
  (let [r (random nil)]
    (cond
      (< r 0.25) (nth n-bases 0)
      (< r 0.5)  (nth n-bases 1)
      (< r 0.75) (nth n-bases 2)
      :else (nth n-bases 3)
      )))
      
(defn base-sequence [n, seq]
  (if (= n 0)
    seq
    (base-sequence (dec n), (str seq (random-base)))))

(def fasta-unit
  (let [r (rand-int 100)]
    (str ">test_fasta-" (str r) "\n" (base-sequence 20 ""))))
      
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println fasta-unit))
