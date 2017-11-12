;; Simple Genetic Algorithm.
(ns ea.ga)

  (defn mutate [rand-gene prob chrm chrm-fns]
    (let [mut-prob (rand)]
      (if (> mut-prob prob)
        ((chrm-fns :replace-gene) chrm (rand-int ((chrm-fns :count-chrm) chrm)) (rand-gene))
        chrm)))
  
  (defn create-mutate [rand-gene prob]
    (partial mutate rand-gene prob))

  (defn mate [chrm1 chrm2 chrm-fns]
    (let [break (rand-int ((chrm-fns :count-chrm) chrm1))
          bgn (first ((chrm-fns :split-chrm) break chrm1)) 
          end (last ((chrm-fns :split-chrm) break chrm2))]
      ((chrm-fns :mutate) (vec ((chrm-fns :concat-chrm) bgn end)) chrm-fns)))
  
  (defn sort-pop [pop fitness]
    (sort #(compare (fitness %2) (fitness %1)) pop))

  (defn tournament-select
    [times min size]
    (let [index (rand-int size)]
        (if (= times 0)
          min
          (if (< index min)
            (tournament-select (- times 1) index size)
            (tournament-select (- times 1) min size)))))
  
  (defn get-mate [pop]
    (nth pop (tournament-select (count pop) (count pop) 5)))
  
  (defn mate-pop [pop fitness chrm-fns]
    (let [sorted-pop (sort-pop pop fitness)]
      (sort-pop (repeatedly (count pop) #(mate (get-mate pop) (get-mate pop) chrm-fns)) fitness)))

  (def default-chrm-fns 
    {:count-chrm count
     :split-chrm split-at
     :concat-chrm concat
     :replace-gene assoc
     :mutate (create-mutate #(rand-int 10) 0.25)})
  
  (defn evolve 
    [fitness pop gen chrm-fns]
    (let [chrm-fns (merge default-chrm-fns chrm-fns)]
      (if (= gen 0)
        pop
        (evolve
          fitness
          (mate-pop pop fitness chrm-fns)
          (- gen 1)
          chrm-fns))))
