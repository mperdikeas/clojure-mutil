(ns mutil
  (:use [clojure.java.jdbc :as sql :only ()])
  (:use [clojure.repl])
  (:use [clojure.set :only (superset?)])
  (:use [clojure.walk :only (postwalk)])
  (:use [clojure.set :only (map-invert)]))

(import '(java.io File))
(comment (use 'clojure.set))

(defn not-empty? [coll]
  (not (empty? coll)))


(defn in? [seq elm]
  (some #(= elm %) seq))

(defn all-in? [colla collb]
  (every? #(= true %)
          (map #(in? colla %) collb)))

(defn not-in? [seq elm]
  (not (in? seq elm)))

(def not-nil? (complement nil?))

(defn only [x] {:pre [(nil? (next x))]} (first x))

(defn only-distinct
  ([coll] (only (distinct coll)))
  ([mapkey collmap]
     (assert (every? map? collmap))
     (only-distinct (map #(% mapkey) collmap))))



(defn third  [coll] (nth coll 2))
(defn fourth [coll] (nth coll 3))
(defn fifth [coll] (nth coll 4))

(defn project2 [xrel ks]
  (map #(select-keys % ks) xrel))

(defn lmul [a list-of-lists-of-bs]
  (map #(cons a %) list-of-lists-of-bs))

(defn permutate
  ([items]
     (permutate items 1 (inc (count items))))
  ([items n1 n2]
     (reduce concat
             (map #(permutate % items)
                  (range n1 (inc n2)))))
  ([n items]
     (if (= n 0)
       '(()) ;; without this escape we are facing a stack overflow close to 23 items
       (if (= n (count items))
         (list items)
         (if (empty? items)
           items
           (concat
            (permutate n (rest items))
            (lmul (first items) (permutate (dec n) (rest items)))))))))

(defn  make-counter
  ([]
    (make-counter 0))
  ([n]
     (let [counter (ref n)]
       (fn [] (dosync (alter counter inc))))))

(defn depthFirst-postOrder
  ([aform]
     (let [
           passAll (fn [x] true)
           ]
       (depthFirst-postOrder aform passAll)))
  ([aform pred]
     (let [
           counter   (atom -1)
           ordToNode (atom {})
           ]
       (postwalk (fn [x]
                   (do
                     (when (pred x)
                       (swap! ordToNode assoc (swap! counter inc) x))
                     x))
                 aform)
       @ordToNode)))

(comment ;; version contributed from SO
(defn idx [col]
  (let [m (map vector
               (range)
               (let [v (atom [])]
                 (postwalk (fn [f] (swap! v conj f) f) col)
                 @v))
        rm (map-invert m)]
    (into {} (map (fn [[i e]]
                    [i [e (if (sequential? e)
                            (mapv rm e)
                            [])]])
                  m))))
)

(defn single-valued?
  [x]
  (not (or (nil? x)
           (.. x getClass isArray)
           (some #(instance? % x) [clojure.lang.Counted
                                   clojure.lang.IPersistentCollection
                                   java.util.Collection
                                   java.util.Map]))))
(def not-single-valued?
  (comp not single-valued?))



(defn enum-idx
  ([coll]
     (enum-idx 0 coll))
  ([start coll]
     (let [applicableRange (range start (+ start (count coll)))]
       (map vector applicableRange coll))))


(defn indexer [listOfKv] ; or a map thereof ..
  (fn [value idx]
    (let
        [ onlyCorrectValues (filter (fn [[k v]] (= v value)) listOfKv)
         ]
      (first (nth onlyCorrectValues idx nil)))))


(defn memoryIndexer [listOfKv]
  (let [
        simpleIndexer (indexer listOfKv)
        memory (atom {})
        ]
    (fn [value]
      (when (nil? (@memory value))
        (swap! memory assoc value 0))
      (swap! memory assoc value (inc (@memory value)))
      (simpleIndexer value (dec (@memory value))))))
      

(defn idx
  ([col]
     (let [
           alwaysTrue (fn [x] true)
           ]
       (idx col alwaysTrue)
     ))
  ([col pred]
  (let [m (map vector
               (range)
               (let [v (atom [])]
                 (postwalk (fn [f]
                             (when (pred f)
                               (swap! v conj f))
                             f)
                           col)
                 @v))
        memIndx (memoryIndexer m)
        ]
    (into {} (map (fn [[i e]]
                    [i [e (if (not-single-valued? e)
                            (filter (comp not nil?) (mapv (fn [el] (memIndx el)
                                                            )
                                                          e))
                            '()
                            )
                        ]
                     ])
                  m)))))

(defn idx-wt-father [m pred]
  (let [
        mm (idx m pred)
        plist (into {} (for [[k [_ kids]]
                             mm vv kids
                             ] [vv k]))
        ]
    (into {} (map (fn [[k [f kids]]]
                    [k (vector f kids (plist k))]
                    )
                  mm))))

(def a {:a [] :b [[[[]]]] :c [] :ac '(:a [] :b [])})
(defn alwaysTrue [x] true)
(= (idx-wt-father a alwaysTrue) { 0 [:a '() 2],
                                  1 [[] '() 2],
                                  2 [[:a []] '(0 1) 19],
                                  3 [:b '() 8],
                                  4 [[] '() 5],
                                  5 [[[]] '(4) 6],
                                  6 [[[[]]] '(5) 7],
                                  7 [[[[[]]]] '(6) 8],
                                  8 [[:b [[[[]]]]] '(3 7) 19],
                                  9 [:c [] 11],
                                 10 [[] '() 11],
                                 11 [[:c []] '(9 10) 19],
                                 12 [:ac '() 18],
                                 13 [:a '() 17],
                                 14 [[] '() 17],
                                 15 [:b '() 17],
                                 16 [[] '() 17],
                                 17 ['(:a [] :b []) '(13 14 15 16) 18],
                                 18 [[:ac '(:a [] :b [])] '(12 17) 19],
                                 19 [{:a [], :b [[[[]]]], :c [], :ac '(:a [] :b [])} '(2 8 11 18) nil]})

(defmacro unless [pred a]
  `(when (not ~pred) ~a))

(defn my-project [xrel ks]
    (map #(select-keys % ks) xrel))

(let [exceptSupersets (fn [colla collb]
                       (let
                           [superSetOfASetInCollb (fn [acoll]
                                                    (some #(superset? (set acoll) (set %)) collb))
                            ]
                         (filter (comp not superSetOfASetInCollb) colla)))
      ]
     (defn take-exc-supersets
       ([items sets]
          (take-exc-supersets 0  (inc (count items)) sets))
       ([items n1 n2 sets]
          (reduce concat
                  (map #(take-exc-supersets % items sets)
                       (range n1 (inc n2)))))
       ([n items sets]
          (exceptSupersets (permutate n items) sets))))

;; test case
(assert ;; this test proves that the order in the excluded lists doesn't matter
 (=
  (take-exc-supersets '(1 2 3 4 5 6)  3 4 '((1) (2 4)))
  (take-exc-supersets '(1 2 3 4 5 6)  3 4 '((1) (4 2)))))

(defn reorder
  "reorders colla by the example of collb (which must be a superset)
   both colla and collb must be distinct"
  [colla collb]
  (let [
        _ (assert (all-in? collb colla))
        _ (assert (and (coll? colla) (coll? collb)))
        _ (doall (map #(when
                           (not-empty? %)
                         (assert (and
                                  (apply distinct? %)
                                  (not-in? % nil))))
                      [colla collb]))
        colla-idx (map-indexed vector colla)
        collb-idx (map-indexed vector collb)
        collb-map (apply zipmap (list (for [[i _ ] collb-idx]  i)
                                      (for [[_ el] collb-idx] el)))
                                      
        ]
    (vector colla-idx collb-map)
    (filter not-nil?
            (for [i (range (count collb-map))] (when (in? colla (collb-map i)) (collb-map i))))))

(assert (= (reorder '(a b c d)' (e  d f g c h i b a))
           '(d c b a)))

(defn reordermap
  "reorders collmapa which is a collection of maps based on the example
   of collb (corresponding to an ordering of the values for the key 'field'"
  [collmapa field collb]
  (let [
        _assert (and (coll? collmapa) (coll? collb) (every? map? collmapa))
        val_collmapa (map #(% field) collmapa)
        val_collmap_reordered (reorder val_collmapa collb)
        map_collmapa (zipmap val_collmapa collmapa)
        ]
    (map map_collmapa val_collmap_reordered)))

(assert (= (reordermap '({1 :a 2 :b} {1 :c 2 :d}) 1 '(:e :d :c :b :a))
           '({1 :c, 2 :d} {1 :a, 2 :b})))

(assert (= (reordermap '({:n "beta" :v 2} {:n "gamma" :v 3} {:n "omega" :v 24} {:n "alpha" :v 1} {:n "kappa" :v 11}) :n '("alpha" "beta" "gamma" "delta" "epislon" "kappa" "omega" "xristos"))
           '({:n "alpha", :v 1} {:n "beta", :v 2} {:n "gamma", :v 3} {:n "kappa", :v 11} {:n "omega", :v 24})))



(defn unflatten2 [sizes coll]
  (if sizes
    (unflatten2 (next sizes) (partition (first sizes) coll))
    coll))

(assert (= (unflatten2 [1] '(0)) '((0))))
(assert (= (unflatten2 [2 2] '(1 2 3 4)) '( ( (1 2) (3 4) ) )))
(assert (= (unflatten2 [2 3 1 1] '(1 2 3 4 5 6)) '(((((1 2) (3 4) (5 6)))))))

(defn unflatten [sizes coll]
  (reduce #(partition %2 %1) coll sizes))

(assert (only-distinct (map #(= (unflatten %1 %2)
                                (unflatten2 % %2))
                            (list [1] [2 2] [2 3 1] [2 3 4 2 1 1 1 1])
                            (map range '(2 5 9 48)))))
                                
(defn clashAdorn
  "adorns, if necessary, the collection of unique items so that the collection items
   do not clash according to the f function"
  [mapcoll f]
  (let
      [_ (when (not (distinct? mapcoll))
           (throw (RuntimeException.)))
       mapcoll-grpd (group-by f mapcoll)
       lookup (fn [el]
                (let
                    [clashed (mapcoll-grpd (f el))]
                  (if (= (count clashed) 1)
                    [nil el]
                    (let
                        [clashed-idx (enum-idx 1 clashed)
                         clashed-idx-unique (only (filter #(= (second %) el) clashed-idx))
                         ]
                      [(first clashed-idx-unique) (second clashed-idx-unique)]))))
       ]
    (map lookup mapcoll)))

(assert (= (clashAdorn '({:a 1 :b 2} {:a 1 :b 3} {:a 2 :b 4}) :a)
           '([1 {:a 1, :b 2}] [2 {:a 1, :b 3}] [nil {:a 2, :b 4}])))

(assert (= (clashAdorn '(1 2 3) even?)
           '([1 1][nil 2][2 3])))

(let
    [f (fn [ [_ [name _]] ]
         name)]
  (assert (= (clashAdorn {1 ["foo" :foo] 2 ["zoo" :foo]} f)
             '([nil [1 ["foo" :foo]]] [nil [2 ["zoo" :foo]]])))
  (assert (= (clashAdorn {1 ["foo" :foo] 2 ["foo" :foo]} f)
             '([1 [1 ["foo" :foo]]] [2 [2 ["foo" :foo]]]))))

(defn maponly
  "takes a collection of collections, filters out the
   collections of more than one element and returns a collection
   of the single element of each collection"
  ([collcoll]
     (maponly collcoll false))
  ([collcoll lenient]
     (let [_ (unless (or lenient
                         (not-any? #(> (count %) 1) collcoll))
                     (throw (RuntimeException. )))
           collcoll-fltr (filter #(= (count %) 1)
                                 collcoll)]
       (map only collcoll-fltr))))

(defn some-true? [coll]
  (= true (some #(= true %) (map #(= true %) coll))))

(defn alltrue [coll]
  (every? #(= true %) coll))

(defn not-alltrue [coll]
  (not (alltrue coll)))

(defn allfalse [coll]
  (every? #(= false %) coll))

(defn flatten2
  "Like `clojure.core/flatten` but better, stronger, faster.
   Takes any nested combination of sequential things (lists, vectors,
   etc.) and returns their contents as a single, flat, lazy sequence.
   If the argument is non-sequential (numbers, maps, strings, nil,
   etc.), returns the original argument."
    {:static true}
    [x]
    (letfn [(flat [coll]
              (lazy-seq
               (when-let [c (seq coll)]
                 (let [x (first c)]
                   (if (sequential? x)
                     (concat (flat x) (flat (rest c)))
                     (cons x (flat (rest c))))))))]
      (if (sequential? x) (flat x) x)))

(defn strct1lvlFlatten
  "1-level flattening demand that coll is a collection of collections and,
   if strict, after 1-level-only flattening, a collection
   of not sequential elements"
  ([coll]
     (strct1lvlFlatten coll true))
  ([coll strict]
     (let [_ (unless (alltrue (map #(sequential? %) coll))
                     (throw (RuntimeException.)))
        rv (reduce concat coll)]
    (when strict
      (if (not-alltrue (map #(not (sequential? %)) rv))
        (throw (RuntimeException.))))
      rv)))

(assert (= (strct1lvlFlatten '( (1 2) (3) (4 5) (6)))
           (range 1 7)))

;; not very handy - delete after some time
;;(defn flatmap [f coll]
;;  (strct1lvlFlatten (map f coll) false))

(defn coll-sub
  "removes from colla all items found in collb"
  [colla collb]
     (filter #(not-in? collb %) colla))

(defn group-by-only
  [f coll]
  (let
      [temp (group-by f coll)]
    (into {} (for [[k v] temp] [k (only v)]))))


(defn mul [as bs]
  (if (and (coll? as)
           (coll? bs))
    (for [a as b bs] (list a b))
    (if (coll? as)
      (for [a as] (list a bs))
      (if (coll? bs)
        (for [b bs] (list as b))
        (throw (RuntimeException.))))))

(defn flat-mul [as bs]
  (strct1lvlFlatten (mul as bs)))
