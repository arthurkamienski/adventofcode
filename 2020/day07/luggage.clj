(ns luggage)

(require '[clojure.string :as str])
(require '[clojure.set :as s])

(defn to-map [rule]
  (def split (rest (re-find #"^(.*?) bags? contains? (.*?)\." rule)))
  (def color (first split))
  (def contained (re-seq #"(\d+) ((?:\w\ ?)+) bag" (second split)))
  (letfn [(to-keyword [c]
           (keyword (str/replace c #"\ " "-")))
          (to-counts [s]
            (def n (Integer. (nth s 1)))
            (def c (to-keyword (nth s 2)))
            {c n})]
    {(to-keyword color) (if (nil? contained) {} (apply merge (map to-counts contained)))}))

(defn read-input [p]
  (->>
    (->
      (slurp p)
      (str/split-lines))
    (map to-map)
    (apply merge)))

(defn invert-map [m]
  (letfn [(inverse [k v]
            (def contained-colors (keys v))
            (map #(hash-map % #{k}) contained-colors))]

    (reduce #(merge-with clojure.set/union %1 %2) (reduce concat (map #(apply inverse %) m)))))

(defn contained-count [color m]
  (if (contains? m color)
    (do
      (def contained-in (get m color))
      (s/union contained-in (reduce s/union (map #(contained-count % m) contained-in))))
    (set [])))

(defn containing-count [color m]
  (letfn [
          (cont [k v] 
            (+ v (* v (containing-count k m))))]
    (def contained (get m color))
    (if (> (count contained) 0)
    (reduce + (map #(apply cont %) (get m color)))
    0)))

(def input (read-input "input.txt"))
(def inverted (invert-map input))
(println (count (contained-count :shiny-gold inverted)))
(println (containing-count :shiny-gold input))
