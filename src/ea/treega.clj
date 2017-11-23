;; Simple tree based GA
(ns ea.treega
  (require [ea.ga :as ga])
  (require [ea.tree :as t]))

(defn rand-int-between [n m]
  (+ n (rand-int (- m n))))

(def count-chrm t/count-tree)
(def concat-chrm t/replace-nth-node)
(def replace-gene t/replace-nth-node)
(def split-chrm #(identity [(t/remove-nth-node %2 %1)
                            (t/get-nth-node %2 %1)]))

(defn mate 
  [chrm1 chrm2 chrm-fns]
  (let [index1 (rand-int-between 1 ((chrm-fns :count-chrm) chrm1))
        index2 (rand-int-between 1 ((chrm-fns :count-chrm) chrm1))
        node (last ((chrm-fns :split-chrm) index2 chrm2))]
    ((chrm-fns :mutate) (vec ((chrm-fns :concat-chrm) chrm1 index1 node)) chrm-fns)))

(defn evolve 
  [fitness
   generations 
   pop-size
   max-depth
   mutate-prob
   rand-gene
   rand-chrm]
  (let [population (repeatedly pop-size #(rand-chrm rand-gene max-depth true))
        chrm-fns {:mate mate
                  :mutate (ga/create-mutate rand-gene mutate-prob)
                  :count-chrm count-chrm
                  :split-chrm split-chrm
                  :concat-chrm concat-chrm
                  :replace-gene replace-gene}]
    (ga/evolve
      fitness
      population
      generations
      chrm-fns)))