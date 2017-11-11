;; Simple Genetic Algorithm.
(ns ea.gp
  (require [ea.ga :as ga]))

  (defn evolve
    [fitness pop gen mut]
    (ga/evolve fitness pop gen mut))

  (defn create-mutate
    [rand-gene mutate-prob]
    (ga/create-mutate rand-gene mutate-prob))