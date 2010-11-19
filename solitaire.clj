(ns solitaire
  (:use utils))

(defn encrypt-decrypt-helper [text keystream f]
  (let [in-nums (map to-num text)
	ks-nums (map to-num keystream)
	f-mod-26 #(mod (f %1 %2) 26)
	out-nums (map f-mod-26 in-nums ks-nums)
	out-letters (map to-letter out-nums)]
    (apply str out-letters)))

(defn encrypt [plaintext keystream]
  (encrypt-decrypt-helper plaintext keystream +))
  
(defn decrypt [ciphertext keystream]
  (encrypt-decrypt-helper ciphertext keystream -))

(defn move-down [card cards]
  (if (not= card (peek cards))
    (let [idx (first (index-filter #{card} cards))]
      (swap idx (inc idx) cards))
    (let [[top-card & middle] (pop cards)]
      (apply vec top-card card middle))))

(defn step-1 [cards]
  (->> cards (move-down \A)))

(defn step-2 [cards]
  (->> cards (move-down \B) (move-down \B)))

(defn triple-cut [cards]
  (let [[idx-1 idx-2] (index-filter #{\A \B} cards)
	joker-1 (cards idx-1)
	joker-2 (cards idx-2)
	top (subvec cards 0 idx-1)
	middle (subvec cards (inc idx-1) idx-2)
	bottom (subvec cards (inc idx-2))]
    (vec (concat bottom [joker-1] middle [joker-2] top))))

(defn step-3 [cards]
  (->> cards triple-cut))

(defn solitaire [cards]
  (->> cards step-1 step-2 step-3))

(def msg "DONOTUSEPC")
(def keystream "KDWUPONOWT")
(println (decrypt (encrypt msg keystream) keystream))

(println (step-2 (step-1 [\A 7 2 \B 9 4 1])))
(println (step-2 (step-1 [3 \A \B 8 9 6])))

(println (step-3 [2 4 6 \B 5 8 7 1 \A 3 9]))
(println (step-3 [\B 5 8 7 1 \A 3 9]))
(println (step-3 [\B 5 8 7 1 \A]))
