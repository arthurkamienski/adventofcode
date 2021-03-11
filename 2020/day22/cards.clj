(ns cards)

(require '[clojure.string :as string])

; from https://github.com/danielmiladinov/joy-of-clojure/blob/master/src/joy-of-clojure/chapter5/how_to_use_persistent_queues.clj
(defmethod print-method clojure.lang.PersistentQueue [q, w] 
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(defn to-q [coll]
  (reduce conj clojure.lang.PersistentQueue/EMPTY coll))

(defn read-input [path]
  (let [decks (string/split (slurp path) #"\n\n")
        parse-deck (fn [d]
                     (let [cards (rest (string/split d #"\n"))
                           int-cards (map #(Integer/parseInt %) cards)]
                       (to-q int-cards)))]
    (map parse-deck decks)))

(defn play [[p1 p2]]
  (cond
    (= 0 (count p1)) p2
    (= 0 (count p2)) p1
    :else (let [c1 (peek p1)
                c2 (peek p2)]
            (if (> c1 c2)
              (recur [(reduce conj (pop p1) [c1 c2]) (pop p2)])
              (recur [(pop p1) (reduce conj (pop p2) [c2 c1])])))))

(defn recursive-play
  ([decks] (:deck (recursive-play decks #{})))
  ([[p1 p2] prev-states]
   (cond
     (contains? prev-states [p1 p2]) {:winner "p1" :deck p1}
     (= 0 (count p1)) {:winner "p2" :deck p2}
     (= 0 (count p2)) {:winner "p1" :deck p1}
     :else (let [c1 (peek p1)
                 c2 (peek p2)
                 rest-p1 (pop p1)
                 rest-p2 (pop p2)
                 new-states (conj prev-states [p1 p2])
                 winner (cond
                          (and (>= (count rest-p1) c1) 
                               (>= (count rest-p2) c2))
                          (:winner (recursive-play [(to-q (take c1 rest-p1))
                                           (to-q (take c2 rest-p2))]
                                          new-states))
                          (> c1 c2) "p1"
                          :else "p2")]
             (if (= winner "p1")
               (recur [(reduce conj rest-p1 [c1 c2]) rest-p2] new-states)
               (recur [rest-p1 (reduce conj rest-p2 [c2 c1])] new-states))))))

(defn score [deck]
  (apply + (map * (reverse deck) (iterate inc 1))))

(def decks (read-input "input.txt"))

(def winner-deck (play decks))
(def final-score (score winner-deck))

(def recursive-winner-deck (recursive-play decks))
(def final-score-recursive (score recursive-winner-deck))

(println final-score)
(println final-score-recursive)
