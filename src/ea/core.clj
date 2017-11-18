(ns ea.core
  (require [ea.ga :as ga])
  (require [ea.gp :as gp]))

;; Simple GA
(defn simple-ga []
  (let [generations 10
        pop-size 1000
        chrm-size 10
        mutate-prob 0.25
        rand-gene #(rand-int 10)
        rand-chrm #(vec (repeatedly chrm-size rand-gene))
        population (repeatedly pop-size rand-chrm)
        chrm-fns {:mutate (ga/create-mutate rand-gene mutate-prob)}
        fitness #(reduce - %)]
    (first 
      (ga/evolve
        fitness
        population
        generations 
        chrm-fns))))

;; Simple GP
(defn simple-gp []
  (let [fitness #(reduce + %)
        generations 10
        pop-size 10
        max-depth 10
        mutate-prob 0.25
        functions []
        terminals []]
    (println (first 
      (gp/evolve
        fitness
        generations 
        pop-size
        max-depth
        mutate-prob
        functions
        terminals)))))


(defn -main
  [& args]
  (println (simple-ga)))
  ;(println (simple-gp)))
