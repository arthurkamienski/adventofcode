(defn get-num [nums lst i]
  (let [last-turn (dec i)
        num-turns (get nums lst)
        first-spoken (:first num-turns)]
    (if (= first-spoken last-turn)
      0
      (let [prev-spoken (:bef num-turns)]
        (- last-turn prev-spoken)))))

(defn count-to
  ([nums stop]
   (let [nums-map (apply merge 
                         (map 
                           #(hash-map %1 {
                                          :first (inc %2) 
                                          :last (inc %2)
                                          :bef (inc %2)})
                           nums 
                           (range)))]
     (count-to nums-map (last nums) (inc (count nums)) stop)))
  ([nums lst i stop]
   (let [curr-num (get-num nums lst i)]
   (do 
     (if (= i stop)
       curr-num
       (let [new-map (if (contains? nums curr-num)
                       (let [old-turns (get nums curr-num)
                             bef-updt  (assoc old-turns :bef (:last old-turns))
                             last-updt (assoc bef-updt :last i)]
                         (assoc nums curr-num last-updt))
                       (assoc nums curr-num {:first i :last i :bef i}))]
         (recur new-map curr-num (inc i) stop))))))
)

;(def input [0 3 6]) ; 436 175594
;(def input [1 3 2]) ; 1 2578
;(def input [2 1 3]) ; 10 3544142
;(def input [1 2 3]) ; 27 261214
;(def input [2 3 1]) ; 78 6895259
;(def input [3 2 1]) ; 438 18
;(def input [3 1 2]) ; 1836 362
(def input [1 12 0 20 8 16])

(println (count-to input 2020))
(println (count-to input 30000000))
