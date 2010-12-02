(ns tests
  (:use algorithm)
  (:use utils)
  (:use [clojure.test :only (deftest are run-tests)]))

(defn encrypts-to [plaintext passphrase ciphertext]
  (and (= (encrypt plaintext passphrase) ciphertext)
       (= (decrypt ciphertext passphrase) (group (pad plaintext)))))

(deftest tests
  (are [x y] (= x y)
       
       (combine "DONOTUSEPC" [11 4 23 21 16 15 14 15 23 20] +)
       "OSKJJ JGTMW"
       
       (combine "OSKJJJGTMW" [11 4 23 21 16 15 14 15 23 20] -)
       "DONOT USEPC"
       
       (->> [\A 7 2 \B 9 4 1] (move-down \A) (move-down \B) (move-down \B))
       [7 \A 2 9 4 \B 1]
       
       (->> [3 \A \B 8 9 6] (move-down \A) (move-down \B) (move-down \B))
       [3 \A 8 \B 9 6]
       
       (triple-cut [2 4 6 \B 5 8 7 1 \A 3 9])
       [3 9 \B 5 8 7 1 \A 2 4 6]
       
       (triple-cut [\B 5 8 7 1 \A 3 9])
       [3 9 \B 5 8 7 1 \A]
       
       (triple-cut [\B 5 8 7 1 \A])
       [\B 5 8 7 1 \A]
       
       (count-cut [7 11 12 13 14 15 16 17 4 5 6 8 9])
       [5 6 8 7 11 12 13 14 15 16 17 4 9]
       
       (count-cut deck)
       deck
       
       (take 10 (to-keystream deck))
       [4 49 10 24 8 51 44 6 4 33]
       
       (take 15 (to-keystream (order deck "FOO")))
       [8 19 7 25 20 9 8 22 32 43 5 26 17 38 48]
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    ""
		    "EXKYI ZSGEH UNTIQ")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    "f"
		    "XYIUQ BMHKK JBEGY")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    "fo"
		    "TUJYM BERLG XNDIW")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    "foo"
		    "ITHZU JIWGR FARMW")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    "a"
		    "XODAL GSCUL IQNSC")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    "aa"
		    "OHGWM XXCAI MCIQP")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    "aaa"
		    "DCSQY HBQZN GDRUT")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    "b"
		    "XQEEM OITLZ VDSQS")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    "bc"
		    "QNGRK QIHCL GWSCE")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAA"
		    "bcd"
		    "FMUBY BMAXH NQXCJ")
       true
       
       (encrypts-to "AAAAAAAAAAAAAAAAAAAAAAAAA"
		    "cryptonomicon"
		    "SUGSR SXSWQ RMXOH IPBFP XARYQ")
       true
       
       (encrypts-to "SOLITAIRE"
		    "cryptonomicon"
		    "KIRAK SFJAN")
       true))

(run-tests)
