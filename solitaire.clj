(ns solitaire
  (:use algorithm))

(defn print-usage []
  (println "Usage: clj solitaire.clj [-e|-d] <key phrase> <text>"))

(if-let [[encrypt-or-decrypt key-phrase text] *command-line-args*]
  (cond
   (= "-e" encrypt-or-decrypt) (println (encrypt text key-phrase))
   (= "-d" encrypt-or-decrypt) (println (decrypt text key-phrase))
   :else (print-usage))
  (print-usage))
