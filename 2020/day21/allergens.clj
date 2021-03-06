(ns allergens)

(require '[clojure.string :as string])

(defn read-input [path]
  (let [split (string/split (slurp path) #"\n")
        parse-ingredients (fn [l]
                            (let [ings (string/split (second (re-find #"(.*?)\ \(" l)) #" ")
                                  allergs (string/split (second (re-find #"\(contains (.*?)\)" l)) #", ")]
                              {:ings (set ings) :allergs (set allergs)}))
        matches (map parse-ingredients split)]
    matches))

(defn allergen-candidates [ing-lists allerg]
  (let [contain (filter #(contains? (:allergs %) allerg) ing-lists)
        ings (map #(:ings %) contain)
        in-common (apply clojure.set/intersection ings)]
  {:allerg allerg :ings in-common}))

(defn with-allergens 
  ([ing-lists]
   (let [allergens (set (apply concat (map #(:allergs %) ing-lists)))
         candidates (map #(allergen-candidates ing-lists %) allergens)]
     (with-allergens candidates (hash-map))))
  ([candidates ing-allergens]
   (if (= 0 (count candidates))
     ing-allergens
     (let [one-ing (first (filter #(= 1 (count (:ings %))) candidates))
           ing (first (:ings one-ing))
           allerg (:allerg one-ing)
           filtered (map #(conj % {:ings (clojure.set/difference (:ings %) #{ing})}) candidates)
           new-candidates (filter #(not= 0 (count (:ings %))) filtered)
           new-ings (conj ing-allergens {allerg ing})]
       (recur new-candidates new-ings)))))

(defn count-occurrences [ing-lists ing-allergens]
  (let [allerg-ings (set (vals ing-allergens))
        all-ings (apply concat (map #(:ings %) ing-lists))]
    (count (filter #(not (contains? allerg-ings %)) all-ings))))

(defn canon-danger-ings [ing-allergens]
  (apply str (interpose "," (map second (sort ing-allergens)))))

(def ing-lists (read-input "input.txt"))
(def ing-allergens (with-allergens ing-lists))
(def no-allergen-count (count-occurrences ing-lists ing-allergens))
(def canonical-dangerous-ings (canon-danger-ings ing-allergens))
(println no-allergen-count)
(println canonical-dangerous-ings)
