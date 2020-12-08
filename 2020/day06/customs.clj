(ns customs)

(require '[clojure.string :as str])

(defn read-input [p]
  (->>
    (->
      (slurp p)
      (str/split #"\n\n"))
    (map str/split-lines)))

(defn anyone-yes [group]
 (count (frequencies (reduce concat group))))

(defn everyone-yes [group]
 (count (filter #(= (second %) (count group)) (frequencies (reduce concat group)))))

(def input (read-input "input.txt"))
(println (reduce + (map anyone-yes input)))
(println (reduce + (map everyone-yes input)))
