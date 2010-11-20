(ns solitaire
  (:use utils))

(defn combine [text keystream f]
  (let [to-number (fn [letter] (- (int letter) 64))
	to-letter (fn [number] (char (+ number 64)))
	f-mod-26 (fn [x y] (mod (f x y) 26))
	in-nums (map to-number text)
	ks-nums (map to-number keystream)
	out-nums (map f-mod-26 in-nums ks-nums)]
    (apply str (map to-letter out-nums))))

(defn stream-encrypt [plaintext keystream]
  (combine plaintext keystream +))

(defn stream-decrypt [ciphertext keystream]
  (combine ciphertext keystream -))

(defn move-down [card cards]
  (if-not (= card (peek cards))
    (let [card-idx (first (index-filter #{card} cards))]
      (swap card-idx (inc card-idx) cards))
    (let [[top-card & middle-cards] (pop cards)]
      (vec (concat [top-card] [card] middle-cards)))))

(defn triple-cut [cards]
  (let [[joker-idx-1 joker-idx-2] (index-filter #{\A \B} cards)
	top-cards (subvec cards 0 joker-idx-1)
	middle-cards (subvec cards joker-idx-1 (inc joker-idx-2))
	bottom-cards (subvec cards (inc joker-idx-2))]
    (vec (concat bottom-cards middle-cards top-cards))))

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
