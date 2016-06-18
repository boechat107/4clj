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
;;
;; It took me a whole afternoon...
;; ===================================================

(defn tree-reparenting
  [new-root tree]
  (letfn [(prewalk [[node & children :as tree]]
            ;; A recursive function to transverse the tree and return an original
            ;; subtree or a modified one (if the new root is in it).
            ;; NOTE: a breadth first search, instead of this depth first search,
            ;; could be a better solution.
            (if (= node new-root)
              ;; The current node (and possible root of a subtree) is the target
              ;; node. Here we return the original root of this possible subtree (in
              ;; this case, this subtree is the same as the original.
              [node tree]
              ;; We can have normal leaves, original subtrees or modified subtrees.
              (let [walks (seq (map prewalk children)) ; recursive call
                    ;; The results of these walks can be three possibilities:
                    ;; nil ('node' is a leaf), a tree (a normal list of nodes)
                    ;; or a vector [original-root-node-of modified-subtree].
                    reg-children (remove vector? walks) ; regular children (original subtrees).
                    ;; Modified subtree. 'n' the original root of the modified
                    ;; subtree 't'.
                    [n t] (first (filter vector? walks))]
                (if-not n
                  ;; If we don't have any modified child, we can just return
                  ;; this original subtree.
                  tree
                  ;; If we have modified child, we need to "move down" 'node'
                  ;; and its unmodified children as a child of 'n' and return
                  ;; the original root of this subtree and the new (modified)
                  ;; subtree.
                  ;; We need to search for it in the modified
                  ;; subtree.
                  [node (move-down (list* node reg-children) n t)]))))
          (move-down [cur-tree new-parent-node uptree]
            ;; Moves the tree 'cur-tree' to a leaf of 'new-parent-node', which is
            ;; somewhere inside the 'uptree'.
            (letfn [(f [[n & chs :as t]]
                      ;; Recursive function that works in a very similar way as
                      ;; 'prewalk'.
                      (if (= n new-parent-node)
                        ;; We found the new root of 'cur-tree'. We need to put at the
                        ;; rightest place.
                        (concat t [cur-tree])
                        ;; We just return the tree as we found it.
                        (conj (map f chs) n)))]
              (f uptree)))]
    (second (prewalk tree))))
