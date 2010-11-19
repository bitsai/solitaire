(ns utils
  (:use [clojure.contrib.seq :only (indexed)]))

(defn index-filter [pred coll]
  (when pred 
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

(defn swap [x y coll]
  (assoc coll x (coll y) y (coll x)))
