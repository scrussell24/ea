;; Simple tree based GA
(ns ea.treega
  (require [ea.ga :as ga])
  (require [ea.tree :as t]))

(def count-chrm t/count-tree)
(def concat-chrm t/replace-nth-node)
(def replace-gene t/replace-nth-node)
(def split-chrm #(identity [(t/remove-nth-node %2 %1)
                            (t/get-nth-node %2 %1)]))

(defn evolve 
  [fitness
   generations 
   pop-size
   max-depth
   mutate-prob
   rand-gene
   rand-chrm
   mate]
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