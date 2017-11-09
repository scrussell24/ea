;; Simple Genetic Algorithm.
(ns ea.ga)

  (defn sptit-chrm [index chrm]
    (split-at index chrm)) ;; this should change

  (defn concat-chrm [chrm1 chrm2] 
    (concat chrm1 chrm2))

  (defn replace-gene [chrm index gene]
    (assoc chrm index gene))
  
  (defn mutate [rand-gene prob chrm]
    (let [mut-prob (rand)]
      (if (> mut-prob prob)
        (replace-gene chrm (rand-int (count chrm)) (rand-gene))
        chrm)))
  
  (defn create-mutate [rand-gene prob]
    (partial mutate rand-gene prob))

  (defn mate [chrm1 chrm2 mut]
    (let [break (rand-int (count chrm1))
          bgn (first (sptit-chrm break chrm1)) 
          end (last (sptit-chrm break chrm2))]
      (mut (vec (concat-chrm bgn end)))))
  
  (defn sort-pop [pop fitness]
    (sort #(compare (fitness %2) (fitness %1)) pop))
  
  (defn rand-log [n] ;; Returns 0.25*log(x) + 1 for random x in [0, n)
    (let [log (+ (* 0.25 (Math/log (rand n))) 1)]
      (if (< log 0) 0.0001 log))) ;; don't want to return anything <= 0 :(
  
  (defn get-index [size]
    (int (Math/floor (* (- 1 (rand-log 1)) size))))
  
  (defn get-mate [pop]
    (nth pop (get-index (count pop))))
  
  (defn mate-pop [pop fitness mut]
    (let [sorted-pop (sort-pop pop fitness)]
      (sort-pop (repeatedly (count pop) #(mate (get-mate pop) (get-mate pop) mut)) fitness)))
  
  (defn evolve [fitness pop gen mut]
    (if (= gen 0)
    pop
    (evolve
      fitness
      (mate-pop pop fitness mut)
      (- gen 1)
      mut)))