(ns tiles)

(require '[clojure.string :as string])

(defn read-input [path]
  (map #(re-seq #"[sn]?[ew]" %) (string/split (slurp path) #"\n")))

(defn tile-pos
  ([steps] (tile-pos [0 0] steps))
  ([curr steps]
   (if (= (count steps) 0)
     curr
     (let [[x y] curr
           this-step (first steps)
           other-steps (rest steps)
           next-pos (case this-step
                      "e"  [(- x 2) y]
                      "w"  [(+ x 2) y]
                      "ne" [(dec x) (inc y)]
                      "nw" [(inc x) (inc y)]
                      "se" [(dec x) (dec y)]
                      "sw" [(inc x) (dec y)])]
       (recur next-pos other-steps)))))

(defn flip [pattern tile]
  (let [flipped-color (not (get pattern tile true))
        flipped-map (assoc pattern tile flipped-color)]
    flipped-map))

(defn black-tiles [pattern]
  (select-keys pattern (for [[k v] pattern :when (not v)] k)))

(defn flip-tiles
  ([tile-list] (flip-tiles {} tile-list))
  ([pattern tile-list]
   (if (= (count tile-list) 0)
     (black-tiles pattern)
     (let [flipped-tile (tile-pos (first tile-list))
           flipped-map (flip pattern flipped-tile)]
       (recur flipped-map (rest tile-list))))))

(defn neighbors [[x y]]
  #{[(- x 2) y]
   [(+ x 2) y]
   [(dec x) (inc y)]
   [(inc x) (inc y)]
   [(dec x) (dec y)]
   [(inc x) (dec y)]})

(defn flips? [pattern tile]
  (let [neighbs (neighbors tile)
        tile-color (get pattern tile true)
        neighbor-colors (map #(get pattern % true) neighbs)
        n-black (count (filter #(not %) neighbor-colors))]
    (or (and tile-color (= n-black 2)) 
        (and (not tile-color) 
             (or (> n-black 2) 
                 (= n-black 0))))))

(defn daily-flip [pattern]
  (let [current-tiles (keys pattern)
        current-flips (filter #(flips? pattern %) current-tiles)
        all-nbs (apply clojure.set/union (map neighbors current-tiles))
        nbs-flips (filter #(flips? pattern %) all-nbs)
        all-flips (set (concat current-flips nbs-flips))]
    (do
      ;(println current-flips)
      ;(println nbs-flips)
      ;(println all-nbs)
      ;(println all-flips)
    (black-tiles (reduce flip pattern all-flips)))))

(def input (read-input "input.txt"))
(def start-pattern (flip-tiles input))
(println (count start-pattern))
(println (count (nth (iterate daily-flip start-pattern) 100)))
