(ns ea.core
  (require [ea.ga :as ga])
  (require [ea.gp :as gp])
  (require [ea.tree :as t]))

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
; (defn simple-gp []
;   (let [fitness #(reduce - %)
;         generations 100
;         pop-size 100
;         max-depth 10
;         mutate-prob 0.25
;         functions '()
;         terminals '()]
;     (println (first 
;       (gp/evolve
;         fitness
;         generations 
;         pop-size
;         max-depth
;         mutate-prob
;         functions
;         terminals)))))


(defn -main
  [& args]
    (println (-> [1 2 4 [4 5]] 
      (t/remove-nth-node 2)
      (t/replace-nth-node 1 [6 7 8])
      (t/count-tree))))
