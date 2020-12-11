(ns seats)

(require '[clojure.string :as s])

(defn read-input [p]
  (letfn [(parse-cols [y row]
            (letfn [(to-col [x c]
                      (let [coord {:x x :y y}
                            state (case c
                                    \L :empty
                                    \. :floor
                                    \# :occupied)]
                        {coord state}))]
              (apply conj (map to-col (range) row))))]
    (->>
      (slurp p)
      (s/split-lines)
      (map parse-cols (range))
      (apply conj))))

(defn neighbors [seat-map seat]
  (if (not= (get seat-map seat) :floor)
    (let [{x :x y :y} seat
          x-neighbors (range (- x 1) (+ x 2))
          y-neighbors (range (- y 1) (+ y 2))
          neighbor-coords (filter 
                            #(not= {:x x :y y} %) 
                            (for [a x-neighbors 
                                  b y-neighbors] 
                              {:x a :y b}))
          in-bounds (filter #(some? (get seat-map %)) neighbor-coords)]
      {seat in-bounds})
    nil))
  
(defn first-in-sight [seat-map curr fs]
  (let [{fx :fx fy :fy} fs
        {x :x y :y} curr
        next-seat {:x (fx x) :y (fy y)}
        next-seat-state (get seat-map next-seat)]
    (case next-seat-state
      :floor (recur seat-map next-seat fs)
      nil    nil
      next-seat)))

(defn in-sight [seat-map seat]
  (if (not= (get seat-map seat) :floor)
    (let [directions [{:fx identity :fy inc} 
                      {:fx identity :fy dec} 
                      {:fx inc :fy identity} 
                      {:fx dec :fy identity} 
                      {:fx inc :fy inc} 
                      {:fx inc :fy dec} 
                      {:fx dec :fy inc} 
                      {:fx dec :fy dec}]
          in-sight-coords (map #(first-in-sight seat-map seat %) directions)
          in-bounds (filter #(some? (get seat-map %)) in-sight-coords)]
      {seat in-bounds})
    nil))

(defn next-state [seat-map nbs-map thr seat]
  (if (not= (get seat-map seat) :floor)
    (let [nbs (map #(get seat-map %) (get nbs-map seat))
          occ-count (count (filter (partial = :occupied) nbs))
          curr-state (get seat-map seat)
          next-state (case curr-state
                       :empty (if (= occ-count 0)
                                :occupied
                                curr-state)
                       :occupied (if (>= occ-count thr)
                                   :empty
                                   curr-state)
                       curr-state)]
        {seat next-state})
    {seat :floor}))

(defn next-map [seat-map nbs thr]
  (let [seats-pos (keys nbs)]
    (apply conj (map (partial next-state seat-map nbs thr) seats-pos))))

(defn stabilized? [bef aft]
  (let [same (fn [k] (= (get bef k) (get aft k)))
        all-seats (keys bef)]
    (every? true? (map same all-seats))))

(defn print-seats [m]
  (let [coords (keys m)
        max-x (reduce max (map #(get % :x) coords))
        max-y (reduce max (map #(get % :y) coords))
        coords-order (for [y (range (+ max-y 1)) x (range (+ max-x 1))] {:x x :y y})
        to-char (fn [s]
                  (let [{x :x y :y} s
                        tile (case (get m s)
                               :empty \L
                               :floor \.
                               :occupied \#)]
                    (if (= x max-x)
                      (str tile \newline)
                      tile)))]
    (apply str (map to-char coords-order))))

(defn stabilized-state
  ([seat-map nbs thr]
   (let [next-seats (next-map seat-map nbs thr)]
     (stabilized-state seat-map next-seats nbs thr)))
  ([prev curr nbs thr]
   (let [next-seats (next-map curr nbs thr)]
     (if (stabilized? prev curr)
       curr
       (stabilized-state curr next-seats nbs thr)))))


(defn occupied-seats [seat-map]
  (count (filter #(= :occupied (second %)) seat-map)))

(def input (read-input "input.txt"))

(def neighbors-map (apply conj (map #(neighbors input %) (keys input))))
(def in-sight-map (apply conj (map #(in-sight input %) (keys input))))


(println (occupied-seats (stabilized-state input neighbors-map 4)))
(println (occupied-seats (stabilized-state input in-sight-map 5)))
