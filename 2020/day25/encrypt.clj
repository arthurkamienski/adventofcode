(ns encrypt)

(def subj-num 7)
;(def card-pk 5764801)
;(def door-pk 17807724)
(def card-pk 1004920)
(def door-pk 10441485)

(defn find-loop-size
  ([subj-num pk] (find-loop-size subj-num pk 1 1))
  ([subj-num pk n it]
   (let [next-n (mod (* n subj-num) 20201227)]
     (if (= next-n pk)
       it
       (recur subj-num pk next-n (inc it))))))

(def door-loop-size (find-loop-size subj-num door-pk))
(def card-loop-size (find-loop-size subj-num card-pk))

(defn transform [subj-num loop-size]
   (nth (iterate #(mod (* % subj-num) 20201227) 1) loop-size))

(def encrypt-key (transform door-pk card-loop-size))

(println encrypt-key)
