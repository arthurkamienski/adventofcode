(ns cubes)

(require '[clojure.string :as string])

(defn read-input [path]
  (letfn [(parse-line [i l] 
            (map 
              #(vector (first %) i 0 0) 
              (filter 
                #(= \# (second %)) 
                (map 
                  #(vector %1 %2) 
                  (range) l))))]
    (->>
      (slurp path)
      (string/split-lines)
      (map parse-line (range))
      (reduce concat)
      (set))))

(defn neighbs [coord dimensions]
  (let [[x y z w] coord
        c-range (fn [c] (range (dec c) (+ c 2)))
        all-coords (if (= 3 dimensions)
                     (for [a (c-range x)
                           b (c-range y)
                           c (c-range z)]
                       [a b c 0])
                     (for [a (c-range x)
                           b (c-range y)
                           c (c-range z)
                           d (c-range w)]
                       [a b c d]))]
    (filter #(not= % coord) all-coords)))

(defn to-check [coords dimensions]
  (set (reduce concat (map #(neighbs % dimensions) coords))))

(defn next-active? [active-coords dimensions coord]
  (let [nbs (neighbs coord dimensions)
        active? (fn [c] (contains? active-coords c))
        n-active (count (filter active? nbs))]
    (if (active? coord)
      (or (= 2 n-active) (= 3 n-active))
      (= 3 n-active))))

(defn next-state [coords dimensions]
  (let [candidates (to-check coords dimensions)]
    (set (filter #(next-active? coords dimensions %) candidates))))

(defn cycle-until [coords i dimensions]
  (let [nxt (next-state coords dimensions)]
    (if (= i 1)
      nxt
      (cycle-until nxt (dec i) dimensions))))

(def coords (read-input "input.txt"))
(println (count (cycle-until coords 6 3)))
(println (count (cycle-until coords 6 4)))
