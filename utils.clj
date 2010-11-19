(ns utils
  (:use [clojure.contrib.seq :only (indexed)]))

(defn to-num [letter]
  (- (int letter) 64))

(defn to-letter [num]
  (char (+ num 64)))

(defn index-filter [pred coll]
  (when pred 
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

(defn swap [x y coll]
  (assoc coll x (coll y) y (coll x)))
