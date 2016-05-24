(ns foreclj.hard-test
  (:require [clojure.test :refer :all]
            [foreclj.hard :refer :all]))

(deftest problem-82
  (are [ret words] (= (word-chain? words) ret)
       true #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}
       false #{"cot" "hot" "bat" "fat"}
       false #{"to" "top" "stop" "tops" "toss"}
       true #{"spout" "do" "pot" "pout" "spot" "dot"}
       true #{"share" "hares" "shares" "hare" "are"}
       false #{"share" "hares" "hare" "are"}))
