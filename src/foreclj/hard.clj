(ns foreclj.hard)

;;; ===================================================
;;; Problem #82
;;; ===================================================

(defn word-chain?
  "Returns true if the given sequence of words can be arranged into a continuous word
  chain.
  PROBLEM #82"
  [words]
  (letfn [(diff<=1? [a b]
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
              (< (f a b 0) 2)))]
    (->> words
      ;; For each word, generates a seq of comparisons (only one difference?).
      (map (fn [w] 
             (map #(diff<=1? w %) words)))
      ;; For each word, counts the number of words with at most one letter
      ;; difference (including the word itself). A sequence of counters is returned.
      (map (fn [bs] 
             (reduce #(if %2 (inc %1) %1) 0 bs)))
      ;; Invalid sequences have more than two words with counters less or equal 2
      ;; (one is the word itself, the other is the second or one before the last in
      ;; the chain). 
      (map #(if (<= % 2) 1 0))
      ;; Words with counter = 2 could be, at most, be placed at the extremes of a
      ;; chain. So, a valid sequence can have at most two of these words. 
      (reduce +)
      (>= 2))))

;; ===================================================
;; Problem #130
;; ===================================================

(defn prewalk [[node & children]]
  (if-not node
    nil
    (do (println node)
        (doall (map prewalk children))
        nil)))

(defn move-down [cur-tree new-parent-node uptree]
  (letfn [(f [[n & chs :as t]]
            (cond
              (nil? n) (throw (Exception. "Some problem"))
              (= n new-parent-node) (concat t [cur-tree])
              :else (conj (map f chs) n)))]
    (f uptree)))

(defn tree-reparenting
  [new-root tree]
  (letfn [(prewalk [[node & children :as tree]]
            (cond 
              (nil? node) nil
              (= node new-root) [node tree]
              :else 
              (let [walks (seq (map prewalk children))
                    reg-children (remove vector? walks)
                    [n t] (first (filter vector? walks))]
                (if-not n
                  tree
                  [node (move-down (list* node reg-children) n t)]))))]
    (second (prewalk tree))))
