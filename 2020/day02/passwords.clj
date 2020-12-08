(ns pass)

(require '[clojure.string :as str])

(defn read-input []
  (->>
       (slurp "input.txt")
       (str/split-lines)
       (map parse-line)))

(defn parse-line [l]
  (def fields [:min :max :char :pass])
  (def matches (zipmap fields (rest (re-find #"(\d+)-(\d+) (\w): (\w+)" l))))
  {:min (Integer. (:min matches))
    :max (Integer. (:max matches))
    :char (first (:char matches))
    :pass (:pass matches)
  })

(defn is-in-range [p]
  (def freqs (frequencies (:pass p)))
  (def char-count (get freqs (:char p) 0))
  (and (>= char-count (:min p)) (<= char-count (:max p))))

(defn is-in-pos [p]
  (let [get-char (fn [c] (= (:char p) (nth (:pass p) (- c 1))))]
  (def min-char (get-char (:min p)))
  (def max-char (get-char (:max p)))
  (and (not (and min-char max-char)) (or min-char max-char))))

(println (count (filter is-in-range (read-input))))
(println (count (filter is-in-pos (read-input))))
