(ns expenses)

(require '[clojure.string :as str])

(defn read-input []
  (->>
    (slurp "input.txt")
    (str/split-lines)
    (map #(Integer. %))
    (vec)
    (sort)))

(defn search-pair-recur [numbers i j]
 (def lower  (nth numbers i))
 (def higher (nth numbers j))
 (def sum (+ lower higher))
 (cond
   (= sum 2020) (* lower higher)
   (> sum 2020) (recur numbers i (- j 1))
   (< sum 2020) (recur numbers (+ i 1) j)))

(defn search-pair [numbers]
  (search-pair-recur numbers 0 (- (count numbers) 1) ))

(defn find-trio [numbers trios]
  (let
    [[t & ts] trios
     [i j k]  t
     x        (nth numbers i)
     y        (nth numbers j)
     z        (nth numbers k)]
    (if (= (+ x y z) 2020)
      (* x y z)
      (recur numbers ts))))

(defn search-trio [numbers]
  (def nRange (range (count numbers)))
  (def combs (for [x nRange y nRange z nRange] (vector x y z)))
  (find-trio numbers combs))

(println (search-pair (read-input)))
(println (search-trio (read-input)))
