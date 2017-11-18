;; Simple Genetic Programming.
(ns ea.gp
  (require [ea.ga :as ga])
  (require [ea.tree :as t]))

(defn- mate 
  [chrm1 chrm2 chrm-fns]
  (let [size1 ((chrm-fns :count-chrm) chrm1)
        index1 (- size1 (rand-int (- size1 1)))
        size2 ((chrm-fns :count-chrm) chrm2)
        index2 (- size2 (rand-int (- size2 1)))
        node (last ((chrm-fns :split-chrm) index2 chrm2))]
    ((chrm-fns :mutate) (vec ((chrm-fns :concat-chrm) chrm1 index1 node)) chrm-fns)))

(defn evolve 
    [fitness
     generations 
     pop-size
     max-depth
     mutate-prob
     functions
     terminals]
    (let [rand-gene #(rand-int 10)                   ;TODO chrm
          rand-chrm #(vec (repeatedly 10 rand-gene)) ;TODO [chrm1 chrm2 ...]
          population (repeatedly pop-size rand-chrm)
          chrm-fns {:mate mate
                    :mutate (ga/create-mutate rand-gene mutate-prob)                  ; fn float chrm -> chrm
                    :count-chrm t/count-tree                                          ; chrm -> n
                    :split-chrm #(identity [(t/remove-nth-node %2 %1)
                                            (t/get-nth-node %2 %1)])                  ; chrm n -> [chrm1 chrm2]
                    :concat-chrm t/replace-nth-node                                   ; chrm1 chrm1 -> chrm
                    :replace-gene t/replace-nth-node}]                                ; chrm n gene -> chrm
      (ga/evolve
        fitness
        population
        generations 
        chrm-fns)))
