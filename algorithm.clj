(ns algorithm
  (:use utils)
  (:use [clojure.string :only (upper-case)]))

(defn to-number [card]
  (if (number? card)
    card
    53))

(defn to-numbers [text]
  (map #(- (int %) 65) text))

(defn to-text [numbers]
  (let [chars (map #(char (+ % 65)) numbers)]
    (apply str chars)))

(defn combine [text keystream f]
  (let [in-nums (to-numbers text)
	out-nums (map #(mod (f %1 %2) 26) in-nums keystream)]
    (to-text out-nums)))

(defn pad [text]
  (if (zero? (mod (count text) 5))
    text
    (recur (apply str (concat text "X")))))

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
  (->> deck
       (move-down \A)
       (move-down \B)
       (move-down \B)
       triple-cut
       count-cut))

(defn solitaire-output [[_ deck]]
  (let [shuffled-deck (solitaire deck)
	top-card (first shuffled-deck)
	output-card (get shuffled-deck (to-number top-card))]
    (if (#{\A \B} output-card)
      (recur [nil shuffled-deck])
      [(to-number output-card) shuffled-deck])))

(defn make-keystream [deck]
  (let [solitaire-outputs (iterate solitaire-output [nil deck])]
    (map first (next solitaire-outputs))))

(defmulti encrypt #(type %2))
(defmethod encrypt clojure.lang.PersistentVector [plaintext deck]
  (combine (upper-case (pad plaintext)) (make-keystream deck) +))

(defmulti decrypt #(type %2))
(defmethod decrypt clojure.lang.PersistentVector [ciphertext deck]
  (combine (upper-case ciphertext) (make-keystream deck) -))

(defn order [deck key]
  (let [offsets (map inc (to-numbers (upper-case key)))]
    (reduce (fn [deck offset] (->> deck solitaire (count-cut offset)))
	    deck
	    offsets)))

(def deck (vec (concat (range 1 53) [\A \B])))

(defmethod encrypt java.lang.String [plaintext key]
  (encrypt plaintext (order deck key)))

(defmethod decrypt java.lang.String [ciphertext key]
  (decrypt ciphertext (order deck key)))
