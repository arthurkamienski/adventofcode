(ns slope)

(require '[clojure.string :as str])

(defn read-input [p]
  (->>
    (slurp p)
    (str/split-lines)))

(defn count-trees [m i j]
  (letfn [(count-path [m x y ts]
            (def height (count m))
            (def width  (count (nth m 0)))
            (def currPos (nth (nth m y) (rem x width)))
            (def trees (if (= currPos \.)
                         ts
                         (+ ts 1)))
            (if (>= y (- height 1))
              trees
              (recur m (+ x i) (+ y j) trees)))]
    (count-path m 0 0 0)))

(def slopes ['(1 1) '(3 1) '(5 1) '(7 1) '(1 2)])
(def input (read-input "input.txt"))

(def partial-count (partial apply (partial count-trees input)))

(def results (map partial-count slopes))

(println results)
(println (reduce * results))
