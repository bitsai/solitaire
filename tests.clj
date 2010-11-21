(ns tests
  (:use solitaire)
  (:use [clojure.test :only (deftest is run-tests)]))

(defn encrypts-to [plaintext ciphertext key]
  (let [keyed-deck (key-deck deck key)]
    (and (= (encrypt plaintext keyed-deck) ciphertext)
	 (= (decrypt ciphertext keyed-deck) plaintext))))

(deftest solitaire-tests
  (is (= (combine "DONOTUSEPC" [11 4 23 21 16 15 14 15 23 20] +)
	 "OSKJJJGTMW"))
  (is (= (combine "OSKJJJGTMW" [11 4 23 21 16 15 14 15 23 20] -)
	 "DONOTUSEPC"))
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
  (is (= (take 10 (make-keystream deck))
	 [4 49 10 24 8 51 44 6 4 33]))
  (is (= (take 15 (make-keystream (key-deck deck "FOO")))
	 [8 19 7 25 20 9 8 22 32 43 5 26 17 38 48]))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "EXKYIZSGEHUNTIQ" ""))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "XYIUQBMHKKJBEGY" "f"))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "TUJYMBERLGXNDIW" "fo"))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "ITHZUJIWGRFARMW" "foo"))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "XODALGSCULIQNSC" "a"))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "OHGWMXXCAIMCIQP" "aa"))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "DCSQYHBQZNGDRUT" "aaa"))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "XQEEMOITLZVDSQS" "b"))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "QNGRKQIHCLGWSCE" "bc"))
  (is (encrypts-to "AAAAAAAAAAAAAAA" "FMUBYBMAXHNQXCJ" "bcd"))
  (is (encrypts-to "AAAAAAAAAAAAAAAAAAAAAAAAA"
		   "SUGSRSXSWQRMXOHIPBFPXARYQ"
		   "cryptonomicon"))
  (is (encrypts-to "SOLITAIREX" "KIRAKSFJAN" "cryptonomicon")))

(run-tests 'tests)
