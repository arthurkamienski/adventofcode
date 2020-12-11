(ns xmas)

(require '[clojure.string :as str])

(defn read-input [p]
  (->>
       (slurp p)
       (str/split-lines)
       (map #(bigint %))))

(defn first-wrong [numbers preamble-size]
  (def preamble (take preamble-size numbers))
  (def others (drop preamble-size numbers))
  (def curr-num (first others))
  
  (letfn [(is-sum? [n preamble]
           (def order (vec (sort (keys (frequencies preamble)))))
           (def last-n (- (count order) 1))
           (letfn [(find-pair [start end]
                     (def curr-sum (+ (nth order start) (nth order end)))
                     (if (> end start)
                       (cond
                         (> curr-sum n) (find-pair start (dec end))
                         (< curr-sum n) (find-pair (inc start) end)
                         (= curr-sum n) true)
                       false))]
             (find-pair 0 last-n)))]
    (if (is-sum? curr-num preamble)
      (recur (rest numbers) preamble-size)
      curr-num)))

(defn contiguous [numbers invalid]
  (def vec-ns (vec numbers))

  (letfn [
    (from [x]
      (def new-start (drop x vec-ns))
      
      (letfn [
        (up-to [y]
          (def nrange (take y new-start))
          (def sum (reduce + nrange))
          
          (cond
            (> sum invalid) []
            (< sum invalid) (up-to (inc y))
            (= sum invalid) nrange))]

      (def nrange (up-to 1))
      (if (= (count nrange) 0)
        (from (inc x))
        (do
          (def maxn (apply max nrange))
          (def minn (apply min nrange))
          (+ maxn minn)
        )
      ))
    )]

    (from 0)
  ))

(def input (read-input "input.txt"))
(def test-input (read-input "test_input.txt"))
(def invalid (first-wrong input 25))
(println invalid)
(println (contiguous input invalid))
