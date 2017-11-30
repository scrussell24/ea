(ns ea.core
  (require [ea.ga :as ga])
  (require [ea.treega :as tga])
  (require [ea.gp :as gp])
  (require [ea.tree :as t]))

;; Simple GA
(defn simple-ga []
  (let [generations 100
        pop-size 100
        chrm-size 10
        mutate-prob 0.25
        rand-gene #(rand-int 10)
        rand-chrm #(vec (repeatedly chrm-size rand-gene))
        population (repeatedly pop-size rand-chrm)
        chrm-fns {:mutate (ga/create-mutate rand-gene mutate-prob)}
        fitness #(reduce - %)]
    (ga/evolve
      fitness
      population
      generations 
      chrm-fns)))

;; Simple Tree GA
(defn rand-int-between [n m]
  (+ n (rand-int (- m n))))

(defn mate 
  [chrm1 chrm2 chrm-fns]
  (let [index1 (rand-int-between 1 ((chrm-fns :count-chrm) chrm1))
        index2 (rand-int-between 1 ((chrm-fns :count-chrm) chrm2))
        node (last ((chrm-fns :split-chrm) index2 chrm2))]
    ((chrm-fns :mutate) (vec ((chrm-fns :concat-chrm) chrm1 index1 node)) chrm-fns)))

(defn fitness [max-depth chrm]
  (let [fit (reduce + (flatten chrm))]
    (if (> (t/max-depth chrm) max-depth)
      0
      fit)))

(defn rand-gene [] 
  (identity [(rand-int 10) (rand-int 10)]))
      
(defn rand-chrm [rand-gene max-depth initial]
  (let [gene-type (rand-int max-depth)]
    (if (and (= gene-type 0) (not initial))
        (rand-gene)
        (identity [(rand-chrm rand-gene (- max-depth 1) false)
                   (rand-chrm rand-gene (- max-depth 1) false)]))))

(defn simple-tree-ga []
  (let [max-depth 3
        fitness (partial fitness max-depth)
        generations 100
        pop-size 100
        mutate-prob 0.25]
    (tga/evolve
      fitness
      generations 
      pop-size
      max-depth
      mutate-prob
      rand-gene
      rand-chrm
      mate)))

;; Simple GP
(defn sum-tree [max-depth chrm]
  (println chrm)
  (let [fit (eval (t/to-list chrm))]
    (println fit)
    (if (> (t/max-depth chrm) max-depth)
      0
      fit)))

(defn simple-gp []
  (let [max-depth 1
        fitness (partial sum-tree max-depth)
        generations 3
        pop-size 3
        mutate-prob 0.25
        functions [+ -]
        terminals [#(rand-int 10)]]
    (gp/evolve
      fitness
      generations 
      pop-size
      max-depth
      mutate-prob
      functions
      terminals)))

(defn -main
  [& args]
  (println (first (simple-ga)))
  (println (first (simple-tree-ga))))
  ;(println (first (simple-gp))))
