(ns messages)

(require '[clojure.string :as string])

(defn read-input [path]
  (let [[rules 
         msgs] (string/split (slurp path) #"\n\n")
        rules-split (string/split-lines rules)
        parse-rule (fn [rule]
                     (let [[n rs] (string/split (string/replace rule "\"" "") #":\ ")
                           rule-list (filter 
                                       #(not= '("|") %) 
                                       (partition-by 
                                         #(= "|" %) 
                                         (string/split rs #"\ ")))]
                      {n rule-list}))
        rules-map (apply conj (map parse-rule rules-split))
        msgs-split (string/split-lines msgs)]
    [rules-map msgs-split]))


(defn to-regex [rules n]
  (if (some #(= n %) ["a" "b"])
    n
    (let [prods (get rules n)]
      (str 
        "(?:" 
        (string/join "|" 
                     (map 
                       (fn [p] 
                         (apply str 
                                (map 
                                  #(to-regex rules %) 
                                  p)))
                       prods))
        ")"))))

(defn part2 [rules msgs]
  (let [first-half (re-pattern (to-regex rules "42"))
        second-half (to-regex rules "31")
        match? (fn [msg]
                 (let [first-matches (count (re-seq first-half msg))]
                   (some
                     some?
                     (map (fn [n]
                            (let
                              [repeats (str "{" n "}")
                               re (re-pattern 
                                    (str 
                                      first-half "+"
                                      first-half repeats 
                                      second-half repeats))]
                              (re-matches re msg)))
                          (range 1 first-matches)))))]
    (count (filter some? (map match? msgs)))))

(def input (read-input "input.txt"))
(def rules (first input))
(def msgs (second input))
(def regex (re-pattern (to-regex rules "0")))
(def matches (count (filter some? (map #(re-matches regex %) msgs))))
(def repeats (part2 rules msgs))
