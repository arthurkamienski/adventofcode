(ns tickets)

(require '[clojure.string :as string])

(defn read-input [path]
  (let [file (slurp path)
        parse-ticket (fn [t] 
                       (map
                         #(Integer. %)
                         (string/split t #",")))
        parse-rule (fn [rl]
                     (let [[field rules] (string/split rl #":\ ")
                           parse-range (fn [r]
                                         (let [[bot top]
                                         (map #(Integer. %) (string/split r #"-"))]
                                         #(and (>= % bot) (<= % top))))
                           ranges (map parse-range (string/split rules #"\ or\ "))
                           valid-func (fn [v] (reduce #(or (%1 v) (%2 v)) ranges))]
                       {field valid-func}))
        [rules 
         ticket 
         others] (map string/split-lines (string/split file #"\n\n"))
        my-ticket (parse-ticket (second ticket))
        other-tickets (map parse-ticket (rest others))
        value-ranges (apply merge (map parse-rule rules))]
    {:my my-ticket :others other-tickets :rules value-ranges}))

(defn invalid-fields [ticket rules]
  (let [ranges (vals rules)
        valid? (fn [v]
                 (some true? (map #(% v) ranges)))]
    (filter #(not (valid? %)) ticket)))

(defn error-rate [tickets rules]
  (reduce + (flatten (map #(invalid-fields % rules) tickets))))

(defn ticket-valid? [ticket rules]
  (= 0 (count (invalid-fields ticket rules))))

(defn possible-fields [tickets rules]
  (if (= 0 (count (first tickets)))
    []
    (let [fields (map first tickets)
          others (map rest tickets)
          fits? (fn [r]
                  (let [[lab f] r]
                    (every? true? (map f fields))))
          these-rules (keys (filter fits? rules))]
      (conj (possible-fields others rules) these-rules))))

(defn find-fields [rules]
  (if (= 0 (count rules))
    []
  (let [this-rule (first (filter #(= 1 (count (second %))) rules))
        others    (filter #(not= 1 (count (second %))) rules)
        field-num (first this-rule)
        field-name (first (second this-rule))
        new-rules   (into {} (map (fn [l]
                            {(first l)
                            (filter #(not= % field-name) (second l))}) others))]
    (conj (find-fields new-rules) [field-num field-name]))))

(defn depart-mult [tickets rules my-ticket]
  (let [fields  (reverse (possible-fields tickets rules))
        ordered (apply merge (map #(hash-map %1 %2) (range) fields))
        field-order (map second (sort (find-fields ordered)))
        zipped (apply merge (map #(hash-map %1 %2) field-order my-ticket)) 
        departure (filter #(string/includes? % "departure") zipped)]
    (reduce * (map second departure))))

(def input (read-input "input.txt"))
(println (error-rate (:others input) (:rules input)))
(def valid-tickets (filter #(ticket-valid? % (:rules input)) (:others input)))
(println (depart-mult valid-tickets (:rules input) (:my input)))
