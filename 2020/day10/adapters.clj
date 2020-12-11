(ns adapters)

(require '[clojure.string :as str])

(defn read-input [p] 
  (let [adapters (->>
                   (slurp p)
                   (str/split-lines)
                   (map #(Integer. %)))
        device-adapter (+ (reduce max adapters) 3)]
    (sort (conj (conj adapters device-adapter) 0))))

(defn differences [adapts]
  (letfn [(differences [acc as]
            (if (> (count as) 1)
              (let [diff (- (second as) (first as))]
                (differences (conj acc diff) (rest as)))
              acc))]
    (let [diffs (differences [] adapts)
          freqs (frequencies diffs)
          n_1s (get freqs 1)
          n_3s (get freqs 3)]
    (* n_1s n_3s))))

(defn foldl [f v coll]
  (if (empty? coll) v
    (foldl f (f v (first coll)) (rest coll))))

(defn arrangements [adapts]
  (letfn [
    (possible-connections [m as]
      (let [k (first as)
            others (rest as)
            v (take-while (partial >= (+ k 3)) others)
            new-map (assoc m k v)]
        (if (= (count others) 0)
          (assoc m k v)
          (possible-connections new-map others))))]
    (let [
          start (last adapts)
          rev (rest (reverse adapts))
          conns (possible-connections {} adapts)
          branches (fn [bs k]
                     (let [cs (get conns k)
                           n (reduce + (map #(get bs %) cs))]
                       (assoc bs k n)))
          all-branches (foldl branches {start 1} rev)]
      (get all-branches 0))))

(def test-input-1 (read-input "test_input1.txt"))
(def test-input-2 (read-input "test_input2.txt"))
(def input (read-input "input.txt"))

(println (differences input))
(println (arrangements input))
