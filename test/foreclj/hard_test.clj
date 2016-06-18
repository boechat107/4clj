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

(deftest problem-130
  (are [ret node tree] (= (tree-reparenting node tree) ret)
       ;;
       '(n)
       'n 
       '(n)
       ;;
       '(a (t (e)))
       'a 
       '(t (e) (a))
       ;;
       '(e (t (a)))
       'e
       '(a (t (e)))
       ;;
       '(a (b (c)))
       'a
       '(c (b (a)))
       ;;
       '(d
         (b
          (c) 
          (e) 
          (a (f
              (g)
              (h)))))
       'd
       '(a
         (b
          (c)
          (d)
          (e))
         (f
          (g)
          (h)))
       ;;
       '(c 
          (d)
          (e)
          (b
           (f
            (g)
            (h))
           (a 
             (i
              (j
               (k)
               (l))
              (m
               (n)
               (o))))))
       'c
       '(a
         (b 
           (c
            (d)
            (e))
           (f
            (g)
            (h)))
         (i
          (j
           (k)
           (l))
          (m
           (n)
           (o))))))
