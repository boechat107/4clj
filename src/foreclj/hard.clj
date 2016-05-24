(ns foreclj.hard)

;;; ===================================================
;;; Problem #82
;;; ===================================================

(defn- diff<=1? [a b]
  "Returns true if the given words has at most one change of difference."
  ;; Defining an auxiliary recursive function.
  (let [f (fn [[a0 & ar] [b0 & br] diff]
            (cond 
              ;; Stop condition. We are not interested on differences
              ;; bigger than 1.
              (or (> diff 1) 
                  (and (nil? a0) (nil? b0))) diff
              ;; Two first characters are equal.
              (= a0 b0) (recur ar br diff)
              ;; If they are different...
              :else (let [na (count ar), nb (count br)]
                      ;; If the words have different lengths, we need to
                      ;; skip the insertion or deletion (counting them as a
                      ;; difference, of course).
                      (cond 
                        (> na nb) (recur ar (conj br b0) (inc diff))
                        (< na nb) (recur (conj ar a0) br (inc diff))
                        :else (recur ar br (inc diff))))))]
    ;; The initial call of the recursive function and the boolean value we
    ;; want to return.
    (< (f a b 0) 2)))

#_(println 
      (diff<=1? "abc" "abxce")
      (diff<=1? "abc" "abxc")
      (diff<=1? "abc" "ab")
      (diff<=1? "abc" "xbc")
      )

(defn word-chain?
  "Returns true if the given sequence of words can be arranged into a continuous word
  chain.
  PROBLEM #82"
  [words]
  (->> words
       (map (fn [w] 
              (map #(diff<=1? w %) words)))
       (map (fn [bs] 
              (reduce #(if %2 (inc %1) %1) 0 bs)))
       (map #(if (<= % 2) 1 0))
       (reduce +)
       (>= 2)))
