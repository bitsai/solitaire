(ns solitaire
  (:use utils))

(defn encrypt-decrypt-helper [text keystream f]
  (let [to-number (fn [letter] (- (int letter) 64))
	to-letter (fn [number] (char (+ number 64)))
	f-mod-26 (fn [x y] (mod (f x y) 26))
	in-nums (map to-number text)
	ks-nums (map to-number keystream)
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

(defn triple-cut [cards]
  (let [[idx-1 idx-2] (index-filter #{\A \B} cards)
	joker-1 (cards idx-1)
	joker-2 (cards idx-2)
	top (subvec cards 0 idx-1)
	middle (subvec cards (inc idx-1) idx-2)
	bottom (subvec cards (inc idx-2))]
    (vec (concat bottom [joker-1] middle [joker-2] top))))

(defn count-cut [cards]
  (let [to-number (fn [card] (if (number? card) card 53))
	last-card (peek cards)
	offset (to-number last-card)
	top (subvec cards 0 offset)
	bottom (pop (subvec cards offset))]
    (vec (concat bottom top [last-card]))))

(defn solitaire [cards]
  (->> cards
       (move-down \A)
       (move-down \B)
       (move-down \B)
       triple-cut
       count-cut))

(def msg "DONOTUSEPC")
(def keystream "KDWUPONOWT")
(println (decrypt (encrypt msg keystream) keystream))

(println (->> [\A 7 2 \B 9 4 1] (move-down \A) (move-down \B) (move-down \B)))
(println (->> [3 \A \B 8 9 6] (move-down \A) (move-down \B) (move-down \B)))

(println (->> [2 4 6 \B 5 8 7 1 \A 3 9] triple-cut))
(println (->> [\B 5 8 7 1 \A 3 9] triple-cut))
(println (->> [\B 5 8 7 1 \A] triple-cut))

(println (->> [7 11 12 13 14 15 16 17 4 5 8 9] count-cut))
(println (->> (vec (concat (range 1 53) [\A \B])) count-cut))
