(ns tests
  (:use algorithm)
  (:use utils)
  (:use [clojure.test :only (deftest is run-tests)]))

(defn encrypts-to [plaintext passphrase ciphertext]
  (and (= (encrypt plaintext passphrase) ciphertext)
       (= (decrypt ciphertext passphrase) (group (pad plaintext)))))

(deftest solitaire-tests
  (is (= (combine "DONOTUSEPC" [11 4 23 21 16 15 14 15 23 20] +)
	 "OSKJJ JGTMW"))
  (is (= (combine "OSKJJJGTMW" [11 4 23 21 16 15 14 15 23 20] -)
	 "DONOT USEPC"))
  (is (= (->> [\A 7 2 \B 9 4 1] (move-down \A) (move-down \B) (move-down \B))
	 [7 \A 2 9 4 \B 1]))
  (is (= (->> [3 \A \B 8 9 6] (move-down \A) (move-down \B) (move-down \B))
	 [3 \A 8 \B 9 6]))
  (is (= (triple-cut [2 4 6 \B 5 8 7 1 \A 3 9])
	 [3 9 \B 5 8 7 1 \A 2 4 6]))
  (is (= (triple-cut [\B 5 8 7 1 \A 3 9])
	 [3 9 \B 5 8 7 1 \A]))
  (is (= (triple-cut [\B 5 8 7 1 \A])
	 [\B 5 8 7 1 \A]))
  (is (= (count-cut [7 11 12 13 14 15 16 17 4 5 6 8 9])
	 [5 6 8 7 11 12 13 14 15 16 17 4 9]))
  (is (= (count-cut deck)
	 deck))
  (is (= (take 10 (to-keystream deck))
	 [4 49 10 24 8 51 44 6 4 33]))
  (is (= (take 15 (to-keystream (order deck "FOO")))
	 [8 19 7 25 20 9 8 22 32 43 5 26 17 38 48]))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   ""
		   "EXKYI ZSGEH UNTIQ"))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   "f"
		   "XYIUQ BMHKK JBEGY"))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   "fo"
		   "TUJYM BERLG XNDIW"))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   "foo"
		   "ITHZU JIWGR FARMW"))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   "a"
		   "XODAL GSCUL IQNSC"))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   "aa"
		   "OHGWM XXCAI MCIQP"))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   "aaa"
		   "DCSQY HBQZN GDRUT"))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   "b"
		   "XQEEM OITLZ VDSQS"))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   "bc"
		   "QNGRK QIHCL GWSCE"))
  (is (encrypts-to "AAAAAAAAAAAAAAA"
		   "bcd"
		   "FMUBY BMAXH NQXCJ"))
  (is (encrypts-to "AAAAAAAAAAAAAAAAAAAAAAAAA"
		   "cryptonomicon"
		   "SUGSR SXSWQ RMXOH IPBFP XARYQ"))
  (is (encrypts-to "SOLITAIRE"
		   "cryptonomicon"
		   "KIRAK SFJAN")))

(run-tests 'tests)
