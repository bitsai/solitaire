(ns solitaire
  (:use algorithm))

(defn print-usage []
  (println "Usage: clj solitaire.clj [-e|-d] <passphrase> <text>"))

(if-let [[encrypt-or-decrypt passphrase text] *command-line-args*]
  (cond
   (= "-e" encrypt-or-decrypt) (println (encrypt text passphrase))
   (= "-d" encrypt-or-decrypt) (println (decrypt text passphrase))
   :else (print-usage))
  (print-usage))
