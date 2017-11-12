(ns ea.core
  (require [ea.ga :as ga])
  (require [ea.gp :as gp]))

;; Simple GA
(defn simple-ga []
  (let [generations 100
        pop-size 100
        chrm-size 20
        mutate-prob 0.25
        rand-gene #(rand-int 1000)
        rand-chrm #(vec (repeatedly chrm-size rand-gene))
        population (repeatedly pop-size rand-chrm)
        chrm-fns {:mutate (ga/create-mutate rand-gene mutate-prob)}
        fitness #(reduce - %)]
    (println (first 
      (ga/evolve
        fitness
        population
        generations 
        chrm-fns)))))

;; Simple GP

(defn -main
  [& args]
  (simple-ga))
