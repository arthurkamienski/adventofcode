(ns math)

(require '[clojure.string :as string])

(defn advanced-evaluate [expr]
  (letfn [(get-par [e n-par acc]
            (if (= 0 n-par)
              [(apply str (butlast acc)) (apply str e)]
              (let [nxt-char (first e)
                    nxt-acc (str acc nxt-char)
                    rst (rest e)]
                (case nxt-char
                  \( (get-par rst (inc n-par) nxt-acc)
                  \) (get-par rst (dec n-par) nxt-acc)
                  (get-par rst n-par nxt-acc)))))]
    (if (string/includes? expr "(")
      (let [[bef par-start] (split-with #(not= \( %) expr)
            [par aft] (get-par (rest par-start) 1 "")
            str-bef  (apply str bef)
            eval-par (advanced-evaluate par)
            new-expr (str str-bef eval-par aft)]
        (advanced-evaluate new-expr))
      (if (nil? (re-find #"[\+\*]" expr))
        (Integer. expr)
        (if (string/includes? expr "+")
          (if (string/includes? expr "*")
            (reduce * (map advanced-evaluate (string/split expr #" \* ")))
            (reduce + (map advanced-evaluate (string/split expr #" \+ "))))
          (reduce * (map advanced-evaluate (string/split expr #" \* "))))))))

(defn next-val [expr]
  (if (= \( (first expr))
    (letfn [
            (get-par [e n-par acc]
              (if (= 0 n-par)
                [acc (apply str (rest e))]
                (let [nxt-char (first e)
                    nxt-acc (str acc nxt-char)
                    rst (rest e)]
                  (case nxt-char
                    \( (get-par rst (inc n-par) nxt-acc)
                    \) (get-par rst (dec n-par) nxt-acc)
                    (get-par rst n-par nxt-acc)))))]
      (get-par (rest expr) 1 (first expr)))
    (let [[_ v rst] (re-find #"(\d+)\ ?(.*)?" expr)]
      [v rst])))

(defn evaluate 
  ([expr]
   (if (nil? (re-find #"[\+\*]" expr))
     (Integer. expr)
     (let [[fst rst] (next-val expr)
           no-par-fst (if (= "" rst)
                         (second (re-matches #"\((.*)\)" fst))
                         fst)]
    (evaluate (evaluate no-par-fst) rst))))
  ([acc expr]
   (if (= "" expr)
     acc
     (let [[op rst] (string/split expr #"\ " 2)
           [v 
            next-expr] (next-val rst)
           eval-v (evaluate v)
           next-acc (if (= "+" op)
                      (+ acc eval-v)
                      (* acc eval-v))]
       (evaluate next-acc next-expr)))))

(defn read-input [path]
  (->> path
       (slurp)
       (string/split-lines)))

(def exprs (read-input "input.txt"))
(println (reduce + (map evaluate exprs)))
(println (reduce + (map advanced-evaluate exprs)))
