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
                      (map #(vector n %) rule-list)))
        rules-map (apply concat (map parse-rule rules-split))
        msgs-split (string/split-lines msgs)]
    [rules-map msgs-split]))

(defn matches? [msg rules]
  (let [n (count msg)
        m (apply
            conj
            (map-indexed 
              (fn [i c]
               (let [rs (map 
                          (fn [[r prod]] r)
                          (filter 
                            (fn [[r prod]] 
                              (= prod (conj nil (str c)))) 
                            rules))
                     ks (map #(hash-map [1 (inc i) %] true) rs)]
                 (apply conj ks)))
              msg))
        res (loop [l 2
                   m1 m]
              (let
                [nm1
                 (loop [s 1
                        m2 m1]
                   (let
                     [nm2
                      (loop [p 1
                             m3 m2]
                        (let
                          [nm3
                           (loop [rs rules
                                  m4 m3]
                             (let
                               [[a [b c]] (first rs)
                                k1 [p s b]
                                k2 [(- l p) (+ s p) c]
                                v1 (get m4 k1 false)
                                v2 (get m4 k2 false)
                                nm4 (if (and v1 v2)
                                      (assoc m4 [l s a] true)
                                      m4)]
                               (if (not (empty? rs))
                                 (recur (rest rs) nm4)
                                 nm4)))]
                          (if (<= p (- l 1))
                            (recur (inc p) nm3)
                            nm3)))]
                     (if (<= s (inc (- n l)))
                       (recur (inc s) nm2)
                       nm2)))]
                (if (<= l n)
                  (recur (inc l) nm1)
                  nm1)))]
    (get res [n 1 "0"] false)))
                                          

(def input (read-input "input.txt"))
(def rules (first input))
(def msgs (second input))
(count (filter #(matches? % rules) msgs))
