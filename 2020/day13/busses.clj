(ns busses)
(require '[clojure.string :as Str])

(defn read-input [p]
  (let [[ts ids] (Str/split-lines (slurp p))
        bus-ids (map #(if (= % "x") nil (Integer. %)) (Str/split ids #","))]
    [(Integer. ts) bus-ids]))

(defn min-bus [ts ids]
  (let [non-nils (filter some? ids)
        waiting (fn [t] (- t (mod ts t)))
        waiting-times (map #(hash-map :bus %, :time (waiting %)) non-nils)
        {bus-id :bus t :time} (apply min-key :time waiting-times)]
  (* t bus-id)))

; Chinese remainder code from https://rosettacode.org/wiki/Chinese_remainder_theorem
(defn extended-gcd
  [a b]
  (cond (zero? a) [(Math/abs b) 0 1]
        (zero? b) [(Math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (Math/abs b)
                     r0 (Math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese-remainder
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)
                        egcd (extended-gcd p n_i)
                        inv_p (second egcd)]
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))]
    (mod sum-prod prod)))

(defn remainders [ts]
  (let [time-pos (filter #(some? (:bus %)) (map #(hash-map :bus %1 :pos %2) ts (range)))
        rems (map #(- (:bus %) (:pos %)) time-pos)]
    (apply merge [0] (rest rems))))

(def input (read-input "input.txt"))
(println (apply min-bus input))
(def remains (remainders (second input)))
(def bus-times (filter some? (second input)))
(println (chinese-remainder bus-times remains))
