(ns bitmask)

(require '[clojure.string :as Str])

(defn apply-mask [m bin]
  (if (> (count m) 0)
    (let [bit-mask (first m)]
      (if (= (second bit-mask) (- 1))
       (apply-mask (rest m) bin)
       (apply-mask (rest m) (apply (partial assoc bin) (first m)))))
    bin))

(defn to-bin [n]
  (let [base-bin (vec (take 36 (repeat 0)))
        bin-val (apply-mask
                  (map
                    (fn [i v] [i (Character/digit v 10)])
                    (range 35 (- 1) (- 1))
                    (reverse (Long/toString n 2)))
                  base-bin)]
    bin-val))

(defn read-input [p]
  (letfn [(read-bin [n]
            (map-indexed (fn [i v]
                           [i (Character/digit v 10)]) n))
          (parse-mask [mask]
            (let [changed-bits (read-bin mask)]
              {:comm :mask :value changed-bits}))
          (parse-mem [comm value]
            (let [mem-pos (Integer. (re-find #"\d+" comm))
                  bin-val (Integer. value)]
              {:comm :mem :pos mem-pos :value bin-val}))
          (to-command [s]
            (let [[comm value] (Str/split s #" = ")]
              (if (= comm "mask")
                (parse-mask value)
                (parse-mem comm value))))]
    (->>
        (slurp p)
        (Str/split-lines)
        (map to-command))))
  
(defn apply-mask-v2 [m bin]
  (if (> (count m) 0)
    (let [bit-mask (first m)]
      (if (= (second bit-mask) 0)
       (apply-mask-v2 (rest m) bin)
       (apply-mask-v2 (rest m) (apply (partial assoc bin) (first m)))))
    bin))

(defn to-dec
  ([bin] (to-dec bin 35))
  ([bin n] 
   (if (>= n 0)
     (let [bin-n (first bin)
           to-sum (if (= bin-n 0)
                    0
                    (Math/pow 2 n))]
       (+ to-sum (to-dec (rest bin) (dec n))))
     0)))

(defn all-sums [sums nums]
  (if (= (count nums) 0)
    sums
    (let [n (first nums)
          others (rest nums)
          newSums (map (partial + n) sums)
          currSums (apply merge sums newSums)]
      (all-sums currSums others))))

(defn all-possible
  ([bin] (all-possible bin 35 0 []))
  ([bin n acc ps] 
   (if (>= n 0)
     (let [bin-n (first bin)
           to-sum (if (= bin-n 1)
                    (Math/pow 2 n)
                    0)
           next-ps (if (= bin-n -1)
                (merge ps (Math/pow 2 n))
                ps)]
       (all-possible (rest bin) (dec n) (+ acc to-sum) next-ps))
     (all-sums [acc] ps))))

(defn update-mem [mem v poss]
  (if (= (count poss) 0)
    mem
    (recur (assoc mem (first poss) v) v (rest poss))))

(defn exec-v2
  ([p] (exec-v2 p nil {}))
  ([p mask mem]
   (if (> (count p) 0)
     (let [currComm (first p)
           others (rest p)
           value (:value currComm)]
       (if (= (:comm currComm) :mask)
         (recur others value mem)
         (let [bin-mem (to-bin (:pos currComm))
               mem-pos (all-possible (apply-mask-v2 mask bin-mem))
               new-mem (update-mem mem value mem-pos)]
           (recur others mask new-mem))))
     mem)))

(defn exec
  ([p] (exec p nil {}))
  ([p mask mem]
   (if (> (count p) 0)
     (let [currComm (first p)
           others (rest p)
           value (:value currComm)]
       (if (= (:comm currComm) :mask)
         (exec others value mem)
         (let [to-add (to-dec (apply-mask mask (to-bin value)))
               mem-pos (:pos currComm)
               new-mem (assoc mem mem-pos to-add)]
           (exec others mask new-mem))))
     mem)))

(def input (read-input "input.txt"))
(println (reduce + (vals (exec input))))
(println (reduce + (vals (exec-v2 input))))
