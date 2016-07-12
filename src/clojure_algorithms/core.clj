(ns clojure-algorithms.core)

(defn numbers [n]
  "generate seq of random numbers"
  (take n (repeatedly #(rand-int 100))))

;; Sorting algorithms

(defn bubble-sort [coll]
  "sort a collection using the bubble sort algo."
  (let [bubble (fn [acc x]
                 (if (> 0 (compare x (peek acc)))
                   (conj (pop acc) x (peek acc))
                   (conj acc x)))]
    (loop [to-sort coll]
      (let [bubbled (reduce bubble [] to-sort)]
        (if (= to-sort bubbled)
          bubbled
          (recur bubbled))))))

(defn merge-sort
  "sorting the given collection with merge-sort"
  [coll]
  (if (or (empty? coll) (= 1 (count coll)))
    coll
    (let [[l1 l2] (split-at (/ (count coll) 2) coll)]
      (loop [r [] l1 (merge-sort l1) l2 (merge-sort l2)]
        (cond (empty? l1) (into r l2)
              (empty? l2) (into r l1)
              :else
              (if (> 0 (compare (first l1) (first l2)))
                      (recur (conj r (first l1)) (rest l1) l2)
                      (recur (conj r (first l2)) l1 (rest l2))))))))

;; Search algorithms

(defn linear-search
  "linear search"
  [coll val]
  (reduce
    #(if (= val %2)
      (reduced (.indexOf coll %2))
      %1)
    false
    coll))

(defn binary-search [coll target]
  "recursive binary search"
  (if (seq coll)
    false
    (loop [low 0
           high (dec (count coll))]
      (if (> low high)
        false
        (let [mid (quot (+ low high) 2)
              mid-val (coll mid)]
          (cond (< mid-val target) (recur (inc mid) high)
                (< target mid-val) (recur low (dec mid))
                :else mid))))))

;; data structures

;; sudo linked-list

(defrecord listNode [elem pointer])

(defn linked-insert [list value]
  (cond
    (nil? list) (listNode value nil)
    :else (listNode value list)))

(defn linked-remove [list]
  (:pointer list))

(defn list-count [list]
  (if list
    (+ 1 (list-count (:pointer list)))
    0))

(def my-linked-list (atom '(5 4 3 2 1)))
(first @my-linked-list)
(rest @my-linked-list)
(swap! my-linked-list conj 6)

(->> '(1 3 4)
     rest
     (cons 5))

;; sudo stack

(def my-stack (atom '(1 2 3)))
(peek @my-stack)
(swap! my-stack pop)
(swap! my-stack conj 5)

(->> '(1 2 3)
     pop
     (cons 4))

;; binary search tree

(defrecord Node [elem left right])

(defn tree-insert [{:keys [elem left right] :as tree} value]
  (cond
    (nil? tree) (Node. value nil nil)
    (< value elem) (Node. elem (tree-insert left value) right)
    (> value elem) (Node. elem left (tree-insert right value))
    :else tree))

(def to-tree #(reduce tree-insert nil %))

(defn to-list [{:keys [elem left right] :as tree}]
  (when tree
    `(~@(to-list left) ~elem ~@(to-list right))))

(defn tree-remove [{:keys [elem left right] :as tree} value]
  (cond
    (nil? tree) nil
    (< value elem) (Node. elem (tree-remove left value) right)
    (> value elem) (Node. elem left (tree-remove right value))
    (nil? left) right
    (nil? right) left
    :else (let [min-value (min right)]
            (Node. min-value left (tree-remove right min-value)))))

(defn tree-min [{:keys [elem left]}]
  (if left
    (recur left)
    elem))

(defn tree-max [{:keys [elem right]}]
  (if right
    (recur right)
    elem))

(defn tree-contains? [{:keys [elem left right] :as tree} value]
  (cond
    (nil? tree) false
    (< value elem) (recur left value)
    (> value elem) (recur right value)
    :else true))

(defn tree-count [{:keys [left right] :as tree}]
  (if tree
    (+ 1 (tree-count left) (tree-count right))
    0))

(defn tree-height
  ([tree] (tree-height tree 0))
  ([tree count]
   (if tree
     (max (tree-height (:left tree) (inc count))
          (tree-height (:right tree) (inc count)))
     count)))

(defn bst?
  ([tree] (bst? tree Integer/MIN_VALUE Integer/MAX_VALUE))
  ([{:keys [elem left right] :as tree} min max]
   (cond
     (nil? tree) true
     (or (< elem min) (> elem max)) false
     :else (and (bst? left min (dec elem))
                (bst? right (inc elem) max)))))
