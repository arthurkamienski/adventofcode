(ns cups)

(defn final-order [cups]
  (apply str (loop [curr 1 
                    order []]
               (let [next-cup (get cups curr)]
                 (if (= next-cup 1)
                   order
                   (recur next-cup (conj order next-cup)))))))

(defn cups-with-stars [cups]
  (let [c1 (get cups 1)
        c2 (get cups c1)]
    (* c1 c2)))

(defn move
  ([cups max-cup n]
   (let [max-input (apply max cups)
         all-cups (concat cups (range (inc max-input) (inc max-cup)))
         linked-cups (assoc (apply conj (map hash-map all-cups (rest all-cups))) (last all-cups) (first cups))]
     (move (transient linked-cups) (first cups) 1 n max-cup)))
  ([cups curr it n max-cup]
   (let [head (get cups curr)
         mid  (get cups head)
         tail (get cups mid)
         end  (get cups tail)

         picked-cups #{head mid tail}

         candidates (concat
                      (range (dec curr) (max (- curr 4) 0) -1)   ; next 3 decreasing
                      (range max-cup (- max-cup 4) -1)) ; loop around

         dest (first (filter #(not (contains? (set picked-cups) %)) candidates))
         after-dest (get cups dest)

         new-cups (conj! cups {curr end, dest head, tail after-dest})]
     (do
       (if (= 0 (mod it 1000000))
         (println it))
       (if (>= it n)
         (persistent! new-cups)
         (recur new-cups end (inc it) n max-cup))))))

(def test-input (map #(Integer/parseInt (str %)) "389125467"))
(def input (map #(Integer/parseInt (str %)) "394618527"))
(def part1 (final-order (move input 9 100)))
(println part1)
(def part2 (move input 1000000 10000000))
(println (cups-with-stars part2))
