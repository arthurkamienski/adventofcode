(ns boarding)

(require '[clojure.string :as str])

(defn to-loc [code min-loc max-loc]
  (if (= (count code) 0)
    max-loc
    (do
      (def first-half (contains? #{\F \L} (first code)))
      (def mid-loc (quot (+ min-loc max-loc) 2))
      (if first-half
        (recur (rest code) min-loc mid-loc)
        (recur (rest code) mid-loc max-loc)))))

(defn to-seat [code]
  (def row-code (take 7 code))
  (def col-code (drop 7 code))
  (def row-loc (to-loc row-code 0 127))
  (def col-loc (to-loc col-code 0 7))
  [row-loc col-loc])

(defn read-input [p]
  (map to-seat (->
      (slurp p)
      (str/split-lines))))

(defn to-id [row col]
  (+ (* row 8) col))

(defn find-seat [ids]
  (def sorted (sort ids))
  (def indexes (iterate inc (first sorted)))
  (def zipped (map vector sorted indexes))
  (second (first (filter #(not= (first %) (second %)) zipped))))

(def input (read-input "input.txt"))
(def ids (map #(apply to-id %) input))
(println (apply max ids))
(println (find-seat ids))
