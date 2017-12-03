;; Simple Genetic Programming.
(ns ea.gp
  (require [ea.treega :as tga])
  (require [ea.tree :as t]))

(defn rand-gene [functions terminals max-depth initial] 
  (if (or initial (> max-depth 0))
    (identity [(rand-nth functions)
               (rand-gene functions terminals (- max-depth 1) false)
               (rand-gene functions terminals (- max-depth 1) false)])
    ((rand-nth terminals))))

(defn rand-chrm [rg max-depth initial] (rg))

(defn evolve
  [fitness
   generations
   pop-size
   max-depth
   mutate-prob
   functions
   terminals]
    (tga/evolve
      fitness
      generations 
      pop-size
      max-depth
      mutate-prob
      (partial rand-gene functions terminals max-depth true)
      rand-chrm))
