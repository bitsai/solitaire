(ns utils
  (:require [clojure.string :as str]))

(defn group [text]
  (let [char-groups (partition-all 5 text)
	strs (map #(apply str %) char-groups)]
    (str/join " " strs)))

(defn indexed [coll]
  (map list (range) coll))

(defn index-filter [pred coll]
  (when pred 
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

(defn pad [letters]
  (if (zero? (mod (count letters) 5))
    (apply str letters)
    (recur (concat letters "X"))))

(defn swap [x y coll]
  (assoc coll x (coll y) y (coll x)))

(defn to-letters [text]
  (filter (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (str/upper-case text)))

(defn to-number [card]
  (if (number? card)
    card
    53))

(defn to-numbers [letters]
  (map #(- (int %) 65) letters))

(defn to-text [numbers]
  (let [chars (map #(char (+ % 65)) numbers)]
    (apply str chars)))
