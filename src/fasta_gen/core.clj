(ns fasta-gen.core
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

; "Implementation of one of Knuth's random number algorithms. Ported from scheme (source: https://stackoverflow.com/questions/14674165/scheme-generate-random)"
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
      
(defn base-sequence
  "Recursively add random nucleotide bases to create a sequence of length n."
  [n, seq]
  (if (= n 0)
    seq
    (base-sequence (dec n), (str seq (random-base)))))

(defn make-seq
  "Represents a graph as a list, first item as a node, with rest of items as children. Items are FASTA sequences of varying lengths"
  [min-seq-len, max-seq-len, lst]
  (let [len (rand-int(- max-seq-len min-seq-len))]
      (base-sequence (+ len min-seq-len) lst)))

(defn make-graph [x, min-seq-len, max-seq-len]
  ;; Define a loop-recur construct with initial values
  (loop [count 0        ;; Initial count is 0
         results []]    ;; Initial list is empty
    ;; Check if the loop should continue
    (if (< count x)
      ;; Perform the operation and append the result to the list
      (let [result (make-seq min-seq-len max-seq-len "")]
        ;; Use recur to continue the loop with updated values
        (recur (inc count) (conj results result)))
      ;; Return the final list of results when the loop ends
      results)))

(defn make-edge [lst kmer]
  ;; Destructure the list to separate the first item and the rest of the items
  (let [[first-item & rest-items] lst]
    ;; Append the kmer to the end of the first item
    (let [modified-first-item (str first-item kmer)]
      ;; Append the kmer to the start of the rest of the items using `map`
      (let [modified-rest-items (map #(str kmer %) rest-items)]
        ;; Combine the modified first item and rest items into a new list
        (cons modified-first-item modified-rest-items)))))

(defn random-graph [num-nodes num-edges]
  (let [nodes (range num-nodes)
        edges (loop [edges #{}]
                (if (< (count edges) num-edges)
                  (let [node1 (rand-nth nodes)
                        node2 (rand-nth nodes)
                        new-edge [node1 node2]]
                    (if (and (not= node1 node2)
                             (not (contains? edges new-edge))
                             (not (contains? edges [node2 node1])))
                      (recur (conj edges new-edge))
                      (recur edges)))
                  edges))]
    {:nodes nodes :edges (vec edges)}))

(defn -main
  "Generates graph of random FASTA sequences with a kmer suffix or prefix. Requires four args: num of sequences in graph; min sequence length; max sequence length; kmer length"
  [& args]
  ;; convert each argument from string to integer
  (let [int-args (map #(Integer/parseInt %) args)]
    (println (make-edge (make-graph (nth int-args 0) (nth int-args 1) (nth int-args 2)) (base-sequence 6 "")))
    (println (make-edge (make-graph (nth int-args 0) (nth int-args 1) (nth int-args 2)) (base-sequence (nth int-args 3) "")))))
