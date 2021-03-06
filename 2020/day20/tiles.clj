(ns tiles)

(require '[clojure.string :as string])

(defn read-input [path]
  (let [images (string/split (slurp path) #"\n\n")
        parse-tile (fn [image]
                     (let [split (string/split image #"\n")
                           tile {
                           :id (Integer/parseInt (second (re-find #"Tile (\d+):" (first split))))
                           :tile (map vec (rest split))
                           :top (second split)
                           :bot (last split)
                           :left (apply str (map first (rest split)))
                           :right (apply str (map last (rest split)))}]
                       tile))]
    (map parse-tile images)))


(defn rotate
  ([image] (rotate (list) image))
  ([rotated image]
   (if (= 0 (count (first image)))
     (vec rotated)
     (let [col (vec (concat (reverse (map first image))))
           others (map rest image)]
       (recur (concat rotated (list col)) others)))))

(defn flip [image]
  (vec (map #(vec (concat (reverse %))) image)))

(defn rotate-tile [tile]
  {:id (:id tile)
   :tile (rotate (:tile tile))
   :bot (apply str (reverse (:right tile)))
   :top (apply str (reverse (:left tile)))
   :left (:bot tile)
   :right (:top tile)
   })

(defn flip-tile [tile]
  {:id (:id tile)
   :tile (flip (:tile tile))
   :bot (apply str (reverse (:bot tile)))
   :top (apply str (reverse (:top tile)))
   :left (:right tile)
   :right (:left tile)
   })

(defn fits-v
  ([tile image] (fits-v tile (:top (first (first image))) (:bot (first (last image))) 0))
  ([tile top bot n]
   (cond
     (= (:bot tile) top) {:tile tile, :pos :top}
     (= (:top tile) bot) {:tile tile, :pos :bot}
     (= n 3) (recur (flip-tile tile) top bot (inc n))
     :else (if (< n 7)
       (recur (rotate-tile tile) top bot (inc n))
       nil))))

(defn fits-h
  ([tile line] (fits-h tile (:left (first line)) (:right (last line)) 0))
  ([tile left right n]
   (cond
     (= (:left tile) right) {:tile tile, :pos :right}
     (= (:right tile) left) {:tile tile, :pos :left}
     (= n 3) (recur (flip-tile tile) left right (inc n))
     :else (if (< n 7)
       (recur (rotate-tile tile) left right (inc n))
       nil))))

(defn make-line [line tiles]
  (let [fits (filter #(some? (fits-h % line)) tiles)]
    (if (empty? fits)
      [line tiles]
      (let [tile (first fits)
            {rotated :tile
             pos :pos} (fits-h tile line)
            others (filter #(not= % tile) tiles)
            new-line (if (= pos :left)
                       (conj line rotated)
                       (concat line (list rotated)))]
       (recur new-line others)))))

(defn order-tiles
  ([tiles]
   (let [[first-line others] (make-line (list (first tiles)) (rest tiles))]
     (order-tiles (list first-line) others)))
  ([image tiles]
   (loop [img image
          ts tiles]
     (if (<= (count ts) 2)
       img
       (let [[tile others1] (loop [ts2 ts
                                   others (list)]
                              (let [fit (fits-v (first ts2) img)]
                                (if (some? fit)
                                  [fit (concat (rest ts2) others)]
                                  (recur (rest ts2) (conj others (first ts2))))))
             [line others2] (make-line (list (:tile tile)) others1)
             new-img (if (= (:pos tile) :bot)
                       (concat img (list line))
                       (conj img line))]
         (recur new-img others2))))))

(defn get-code [image]
  (let [first-line (first image)
        last-line (last image)
        c1 (:id (first first-line))
        c2 (:id (last first-line))
        c3 (:id (first last-line))
        c4 (:id (last last-line))]
    (* c1 c2 c3 c4)))

(defn merge-line
  ([line]
   (let [tiles (map #(butlast (rest (:tile %))) line)]
     (merge-line (vector) tiles)))
  ([merged line]
   (if (= 0 (count (first line)))
     merged
     (let [row (vec (apply str (map #(apply str (butlast (rest (first %)))) line)))
           others (map rest line)]
       (recur (conj merged row) others)))))

(defn make-image [ordered-tiles]
  (vec (apply concat (map merge-line ordered-tiles))))

(defn has-monster [[i j] image]
  (let [head (get (get image (dec j)) (+ i 18))
        body (apply str (subvec (get image j) i (+ i 20)))
        feet (apply str (subvec (get image (inc j)) (inc i) (+ i 17)))]
  (and (= head \#)
       (some? (re-matches #"#....##....##....###" body))
       (some? (re-matches #"#..#..#..#..#..#" feet)))))

(defn find-monsters 
  ([image] (find-monsters image 0))
  ([image n]
   (let [cols (range 1 (dec (count image)))
         rows (range (- (count (first image)) 19))
         coords (for [r rows c cols] (vector r c))
         monsters (filter #(has-monster % image) coords)
         n-monsters (count monsters)]
     (cond
       (> n-monsters 0) monsters
       (= n 3) (recur (flip image) (inc n))
       (< n 7) (recur (rotate image) (inc n))
       :else nil))))

(defn monster-coords [monsters]
  (let [get-coords (fn [[i j]]
                     (concat [[(+ i 18) (dec j)]]
                             (map #(vector (+ i %) j) [0 5 6 11 12 17 18 19])
                             (map #(vector (+ i %) (inc j)) [1 4 7 10 13 16])))]
    (count (set (apply concat (map get-coords monsters))))))

(defn count-hashtags [image]
  (apply + (map
             #(count (filter (fn [c] (= \# c)) %))
             image)))

(defn print-image [image]
  (doseq [i image]
    (println (apply str i))))

(def tiles (read-input "input.txt"))
(def ordered-tiles (order-tiles tiles))
(def code (get-code ordered-tiles))
(def image (make-image ordered-tiles))
(def monsters (find-monsters image))
(def n-monster-coords (monster-coords monsters))
(def n-hashtags (count-hashtags image))
(println code)
(println (- n-hashtags n-monster-coords))
