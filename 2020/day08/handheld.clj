(ns handheld)

(require '[clojure.string :as str])

(defn read-input [p]
  (letfn [
          (to-inst [line]
            (def split (str/split line #" "))
            {:op (first split) :value (Integer. (second split))})]
  (->>
       (slurp p)
       (str/split-lines)
       (map to-inst)
       (interleave (range))
       (apply hash-map))))

(defn run [insts]
  (letfn [(recurs-run [ip acc visited]
    (def curr-inst (get insts ip))
    (let [{op :op value :value} curr-inst]
      (if (contains? insts ip)
        (if (contains? visited ip)
          {:loop true :acc acc}
          (case op
            "acc" (recurs-run (+ ip 1) (+ acc value) (conj visited ip))
            "jmp" (recurs-run (+ ip value) acc (conj visited ip))
            "nop" (recurs-run (+ ip 1) acc (conj visited ip))))
        {:loop false :acc acc})))]
    (recurs-run 0 0 #{})))

(defn find-corrupted [insts]
  (def all-change-insts (keys (filter #(not= "acc" (get (second %) :op)) input)))

  (letfn [(find-recurs [change-insts]
    (def change-pos (first change-insts))
    (def to-change (get insts change-pos))
    (def changed (assoc to-change :op (if (= "jmp" (get to-change :op)) "nop" "jmp")))
    (def new-insts (assoc insts change-pos changed))
    (def run-result (run new-insts))
    (if (get run-result :loop)
      (recur (rest change-insts))
      run-result))]
    
    (find-recurs all-change-insts)))


(def input (read-input "input.txt"))
(println (run input))
(println (find-corrupted input))
