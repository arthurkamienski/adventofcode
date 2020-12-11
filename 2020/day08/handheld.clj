(ns handheld)

(require '[clojure.string :as str])

(defn read-input [p]
  (letfn [
          (to-inst [line]
            (let [split (str/split line #" ")]
            {:op (first split) :value (Integer. (second split))}))]
  (->>
       (slurp p)
       (str/split-lines)
       (map to-inst)
       (interleave (range))
       (apply hash-map))))

(defn run [insts]
  (letfn [(recurs-run [ip acc visited]
    (let [curr-inst (get insts ip)
          {op :op value :value} curr-inst]
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
  (let [all-change-insts (keys (filter #(not= "acc" (get (second %) :op)) insts))]
  (letfn [(find-recurs [change-insts]
            (let [
                change-pos (first change-insts)
                to-change (get insts change-pos)
                changed (assoc to-change :op (if (= "jmp" (get to-change :op)) "nop" "jmp"))
                new-insts (assoc insts change-pos changed)
                run-result (run new-insts)]
    (if (get run-result :loop)
      (recur (rest change-insts))
      run-result)))]
    (find-recurs all-change-insts))))


(def input (read-input "input.txt"))
(println (get (run input) :acc))
(println (get (find-corrupted input) :acc))
