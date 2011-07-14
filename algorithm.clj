(ns algorithm
  (:use utils))

(defn move-down [card deck]
  (if (not= card (peek deck))
    (let [card-idx (first (index-filter #{card} deck))]
      (swap card-idx (inc card-idx) deck))
    (let [[top-card & middle-cards] (pop deck)]
      (vec (concat [top-card] [card] middle-cards)))))

(defn triple-cut [deck]
  (let [[joker-idx-1 joker-idx-2] (index-filter #{\A \B} deck)
	top-cards (subvec deck 0 joker-idx-1)
	middle-cards (subvec deck joker-idx-1 (inc joker-idx-2))
	bottom-cards (subvec deck (inc joker-idx-2))]
    (vec (concat bottom-cards middle-cards top-cards))))

(defn count-cut
  ([deck]
     (let [bottom-card (peek deck)]
       (count-cut (to-number bottom-card) deck)))
  ([offset deck]
     (let [bottom-card (peek deck)
	   top-cards (subvec deck 0 offset)
	   middle-cards (pop (subvec deck offset))]
       (vec (concat middle-cards top-cards [bottom-card])))))

(defn solitaire [deck]
  (->> deck (move-down \A) (move-down \B) (move-down \B) triple-cut count-cut))

(defn solitaire-output [[_ deck]]
  (let [shuffled-deck (solitaire deck)
	top-card (first shuffled-deck)
	output-card (get shuffled-deck (to-number top-card))]
    (if (#{\A \B} output-card)
      (recur [nil shuffled-deck])
      [(to-number output-card) shuffled-deck])))

(defn to-keystream [deck]
  (let [solitaire-outputs (iterate solitaire-output [nil deck])]
    (map first (drop 1 solitaire-outputs))))

(defn combine [in-text keystream f]
  (let [padded-letters (pad (to-letters in-text))
	in-nums (to-numbers padded-letters)
	out-nums (map #(mod (f %1 %2) 26) in-nums keystream)
	out-text (to-text out-nums)]
    (group out-text)))

(defmulti encrypt #(type %2))
(defmethod encrypt clojure.lang.PersistentVector [plaintext deck]
  (combine plaintext (to-keystream deck) +))

(defmulti decrypt #(type %2))
(defmethod decrypt clojure.lang.PersistentVector [ciphertext deck]
  (combine ciphertext (to-keystream deck) -))

(defn order [deck passphrase]
  (let [offsets (map inc (to-numbers (to-letters passphrase)))]
    (reduce (fn [deck offset] (->> deck solitaire (count-cut offset)))
	    deck
	    offsets)))

(def deck (vec (concat (range 1 53) [\A \B])))

(defmethod encrypt String [plaintext passphrase]
  (encrypt plaintext (order deck passphrase)))

(defmethod decrypt String [ciphertext passphrase]
  (decrypt ciphertext (order deck passphrase)))
