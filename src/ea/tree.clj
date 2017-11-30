(ns ea.tree
  (require [clojure.zip :as z]))

(defn- tree 
  [vec]
  (z/vector-zip vec))

(defn- root
  [zipper]
  (z/root zipper))

(defn- zipper?
  [obj]
  (contains? (meta obj) :zip/make-node))

(defn- convert-to-tree [root]
  (if (zipper? root) root (tree root)))

(defn- walk-to-nth
  [root n]
  (let [zipper (convert-to-tree root)]
    (if (= n 0)
        zipper
        (walk-to-nth (z/next zipper) (- n 1)))))

(def tseq (partial tree-seq vector? identity))

(defn replace-nth-node
  [root n node]
  (z/root (z/replace (walk-to-nth root n) node)))

(defn get-nth-node
  [root n]
  ((walk-to-nth root n) 0))

(defn remove-nth-node
  [root n]
  (z/root (z/remove (walk-to-nth root n))))

(defn count-tree [root]
  (count (tseq root)))

(defn max-depth [root]
  (loop [current (z/zipper coll? seq nil root) height 0]
    (if (z/end? current) height
      (recur (z/next current)
        (if (z/branch? current) height
          (-> current z/path count (max height)))))))

;; Our trees are ususally vectors but they need to 
;; be converted to lists in order to eval
(defn to-list [root]
  (if (vector? root)
    (reverse (into (list) (map to-list root)))
    root))
