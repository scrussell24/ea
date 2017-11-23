;; Simple Genetic Programming.
(ns ea.gp
  (require [ea.treega :as tga]))


(defn rand-gene [] 
  (identity [(rand-int 10) (rand-int 10)]))
        
(defn rand-chrm [rand-gene max-depth initial]
  (let [gene-type (rand-int max-depth)]
    (if (and (= gene-type 0) (not initial))
        (rand-gene)
        (identity [(rand-chrm rand-gene (- max-depth 1) false)
                   (rand-chrm rand-gene (- max-depth 1) false)]))))

(defn evolve
  [max-depth
   fitness
   generations
   pop-size
   mutate-prob
   functions
   terminals]
    (tga/evolve
      fitness
      generations 
      pop-size
      max-depth
      mutate-prob
      rand-gene
      rand-chrm))