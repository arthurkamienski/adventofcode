(ns ship)

(require '[clojure.string :as string])

(defn read-input [p]
  (letfn [(to-inst [s]
            {:com (first s) :value (Integer. (subs s 1))})]
  (->>
       (slurp p)
       (string/split-lines)
       (map to-inst))))

(defn state [dir x y]
  {:dir dir :pos {:x x :y y}})

(defn waypoint-state [s wp]
  {:ship s :wayp wp})

(defn rotate [dir angle]
    (let [turns (quot (+ angle 360) 90)
          curr-dir (case dir \N 0 \E 1 \S 2 \W 3)
          next-dir (mod (+ curr-dir turns) 4)]
      (case next-dir
        0 \N
        1 \E
        2 \S
        3 \W)))

(defn move [s inst]
  (let [{com :com
         value :value} inst
        {dir :dir
         pos :pos} s
        {x :x y :y} pos
        move-dir (if (= com \F) dir com)]
    (case move-dir
      \N (state dir x (- y value))
      \S (state dir x (+ y value))
      \E (state dir (+ x value) y)
      \W (state dir (- x value) y)
      \L (state (rotate dir (- value)) x y)
      \R (state (rotate dir value) x y)
      s
      )))

(defn move-to-waypoint [ship wayp moves]
  (if (> moves 0)
    (let [{sx :x sy :y} ship
          {wx :x wy :y} wayp
          dx (+ wx sx)
          dy (+ wy sy)]
    (recur {:x dx :y dy} wayp (dec moves)))
    (waypoint-state ship wayp)))

(defn rotate-waypoint [wayp angle]
  (let [turns (mod (quot (+ angle 360) 90) 4)
        {x :x y :y} wayp
        new-wp (case turns
                 0 wayp
                 1 {:x (- y) :y x}
                 2 {:x (- x) :y (- y)}
                 3 {:x y :y (- x)})]
    new-wp))

(defn move-waypoint [s inst]
  (let [{com :com
         value :value} inst
        {ship :ship
         wayp :wayp} s
        {x :x y :y} ship
        {wx :x wy :y} wayp]
    (case com
      \N (waypoint-state ship {:x wx :y (- wy value)})
      \S (waypoint-state ship {:x wx :y (+ wy value)})
      \E (waypoint-state ship {:x (+ wx value) :y wy})
      \W (waypoint-state ship {:x (- wx value) :y wy})
      \L (waypoint-state ship (rotate-waypoint wayp (- value)))
      \R (waypoint-state ship (rotate-waypoint wayp value))
      \F (move-to-waypoint ship wayp value)
      s
      )))

(defn waypoint-path
  ([insts] (waypoint-path (waypoint-state {:x 0 :y 0} {:x 10 :y -1}) insts))
  ([s insts]
   (if (= (count insts) 0)
     (manhattan (get s :ship))
     (recur (move-waypoint s (first insts)) (rest insts)))))

(defn path
  ([insts] (path (state \E 0 0) insts))
  ([s insts]
   (if (= (count insts) 0)
     (manhattan (get s :pos))
     (recur (move s (first insts)) (rest insts)))))

(defn manhattan [c]
  (let [{x :x y :y} c]
    (+ (Math/abs x) (Math/abs y))))

(def input (read-input "input.txt"))
(println (path input))
(println (waypoint-path input))
