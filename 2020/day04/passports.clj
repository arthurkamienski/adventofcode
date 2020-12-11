(ns passports)
(require '[clojure.string :as str])

(defn read-input [p]
  (letfn [(process-pass [p]
            (let [
              fields (str/split p #"\n|\ ")
              records (map #(str/split % #":") fields)]
            (apply merge (map #(hash-map (keyword (first %)) (second %)) records))))]
  (->>
       (->
         (slurp p)
         (str/split #"\n\n"))
       (map process-pass))))

(defn all-fields? [p]
  (let [fields '(:byr :iyr :eyr :hgt :hcl :ecl :pid)]
  (every? p fields)))

(defn valid? [p]
  (letfn [(check-year [y s f]
            (let [
              year (get p y)
              match (re-matches #"\d{4}" year)]
            (if match
              (let [intYear (Integer. year)]
                (and (>= intYear s) (<= intYear f)))
              false)))
          (check-re [k re]
            (let [field (get p k)]
            (not (nil? (re-matches re field)))))
          (check-hgt []
            (let [
              hgt (get p :hgt)
              match (re-matches #"(\d{2,3})(cm|in)" hgt)]
            (if match
              (let [hgtInt (Integer. (nth match 1))]
               (if (= (nth match 2) "cm")
                (and (>= hgtInt 150) (<= hgtInt 193))
                (and (>= hgtInt 59) (<= hgtInt 76))))
              false)))]
        (every? true? [(check-year :byr 1920 2002)
          (check-year :iyr 2010 2020)
          (check-year :eyr 2020 2030)
          (check-hgt)
          (check-re   :hcl #"#\w{6}")
          (check-re   :ecl #"amb|blu|brn|gry|grn|hzl|oth")
          (check-re   :pid #"\d{9}")
          ])))

(def input (read-input "input.txt"))
(println (count (filter all-fields? input)))
(println (count (filter valid? (filter all-fields? input))))
