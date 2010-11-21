(ns solitaire
  (:use utils))

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
  (let [bottom-card (peek cards)
	offset (to-number bottom-card)
	top-cards (subvec cards 0 offset)
	middle-cards (pop (subvec cards offset))]
    (vec (concat middle-cards top-cards [bottom-card]))))

(defn solitaire [[_ cards]]
  (let [shuffled-cards (->> cards
			    (move-down \A)
			    (move-down \B)
			    (move-down \B)
			    triple-cut
			    count-cut)
	top-card (first shuffled-cards)
	output-card (shuffled-cards (to-number top-card))]
    (if (#{\A \B} output-card)
      (recur [nil shuffled-cards])
      [(to-number output-card) shuffled-cards])))

(defn make-keystream [cards]
  (map first (drop 1 (iterate solitaire [nil cards]))))

(defn encrypt [plaintext cards]
  (combine plaintext (make-keystream cards) +))

(defn decrypt [ciphertext cards]
  (combine ciphertext (make-keystream cards) -))

(def cards (vec (concat (range 1 53) [\A \B])))
