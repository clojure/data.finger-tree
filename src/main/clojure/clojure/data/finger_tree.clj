;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Persistent collections based on 2-3 finger trees."
      :author "Chris Houser"}
  clojure.data.finger-tree
  (:import (clojure.lang Seqable Sequential ISeq IPersistentSet ILookup
                         IPersistentStack IPersistentCollection Associative
                         Sorted Reversible Indexed Counted)))

(comment ; TODO:

- implement java.util.Collection
- implement IMeta
- implement IChunkedSeq?
- replace copy/pasted code with macros
- test dequeue complexity
- confirm recursion is bounded, though perhaps O(log n) growth is slow enough
- add simple lookup to Splittable?
- add sorted map with index?
)

;(set! *warn-on-reflection* true)

(defprotocol ConjL
  (conjl [s a] "Append a to the left-hand side of s"))

(defprotocol ObjMeter
  "Object for annotating tree elements.  idElem and op together form a Monoid."
  (measure [_ o] "Return the measured value of o (same type as idElem)")
  (idElem [_] "Return the identity element for this meter")
  (opfn [_] "Return an associative function of two args for combining measures"))

(defprotocol Measured
  (measured [o] "Return the measured value of o")
  (getMeter [o] "Return the meter object for o"))

(defprotocol Splittable
  (split [o pred acc] "Return [pre m post] where pre and post are trees"))

(defprotocol SplitAt
  (ft-split-at [o k notfound] [o k]
               "Return [pre m post] where pre and post are trees"))

(defprotocol Tree
  (app3 [t1 ts t2] "Append ts and (possibly deep) t2 to tree t1")
  (app3deep [t2 ts t1] "Append ts and t2 to deep tree t1")
  (measureMore [o] "Return the measure of o not including the leftmost item")
  (measurePop [o] "Return the measure of o not including the rightmost item"))

(extend-type nil
  ObjMeter
  (measure [_ _] nil)
  (idElem [_] nil)
  (opfn [_] nil)
  Measured
  (measured [_] nil)
  (getMeter [_] nil))

(declare newEmptyTree newSingleTree newDeepTree digit deep)

(defmacro ^:private defdigit [& items]
  (let [i (gensym "i_")
        p (gensym "p_")
        o (gensym "o_")
        typename (symbol (str "Digit" (count items)))
        this-items (map #(list (keyword %) o) items)]
   `(deftype ~typename [~@items ~'meter-obj ~'measure-ref]
      Seqable
        (seq [_] ~(reduce #(list `cons %2 %1) nil (reverse items)))
      Indexed
        (count [_] ~(count items)) ; not needed?
        (nth [_ ~i notfound#]
          (cond ~@(mapcat (fn [sym n] [`(== ~i (int ~n)) sym])
                          items
                          (range (count items)))
            :else notfound#))
      Sequential
      ISeq
        (first     [_] ~(first items))
        (more      [_] ~(if (> (count items) 1)
                          `(digit ~'meter-obj ~@(next items))
                          `(newEmptyTree ~'meter-obj)))
        (next      [_] ~(when (> (count items) 1)
                          `(digit ~'meter-obj ~@(next items))))
      IPersistentStack
        (peek      [_] ~(last items))
        (pop       [_] ~(if (> (count items) 1)
                          `(digit ~'meter-obj ~@(drop-last items))
                          `(newEmptyTree ~'meter-obj)))
      IPersistentCollection
        (empty [_]) ; TBD ; not needed?
        (equiv [_ x] false) ; TBD
        (cons  [_ x#] (digit ~'meter-obj ~@items x#))
      ConjL
        (conjl [_ x#] (digit ~'meter-obj x# ~@items))
      Measured
        (measured [_] @~'measure-ref)
        (getMeter [_] ~'meter-obj) ; not needed?
      Splittable ; allow to fail if op is nil:
        (split [_ ~p ~i]
          ~(letfn [(step [ips [ix & ixs]]
                      (if (empty? ixs)
                        [(when ips `(digit ~'meter-obj ~@ips))
                         ix
                         nil]
                        `(let [~i ((opfn ~'meter-obj)
                                     ~i
                                     (measure ~'meter-obj ~ix))]
                           (if (~p ~i)
                             [~(when ips
                                 `(digit ~'meter-obj ~@ips))
                              ~ix
                              (digit ~'meter-obj ~@ixs)]
                             ~(step (concat ips [ix]) ixs)))))]
             (step nil items))))))

(defmacro ^:private make-digit [meter-obj & items]
  (let [typename (symbol (str "Digit" (count items)))]
    `(let [~'mobj ~meter-obj
           ~'op (opfn ~'mobj)]
       (new ~typename ~@items ~'mobj
            (when ~'op
              (delay ~(reduce #(list 'op %1 %2)
                              (map #(list `measure 'mobj %) items))))))))

(defmacro meter [measure idElem op]
  `(reify ObjMeter
      (measure [_ a#] (~measure a#))
      (idElem [_] ~idElem)
      (opfn [_] ~op)))

(defdigit a)
(defdigit a b)
(defdigit a b c)
(defdigit a b c d)

;; cannot be static because it calls protocol methods
(defn digit
  ([meter-obj a]       (make-digit meter-obj a))
  ([meter-obj a b]     (make-digit meter-obj a b))
  ([meter-obj a b c]   (make-digit meter-obj a b c))
  ([meter-obj a b c d] (make-digit meter-obj a b c d)))

(defn ^:static nodes [mfns xs]
  (let [v (vec xs), c (count v)]
    (seq
      (loop [i (int 0), nds []]
        (condp == (- c i)
          (int 2) (-> nds (conj (digit mfns (v i) (v (+ (int 1) i)))))
          (int 3) (-> nds (conj (digit mfns (v i) (v (+ (int 1) i))
                                       (v (+ (int 2) i)))))
          (int 4) (-> nds (conj (digit mfns (v i) (v (+ (int 1) i))))
                    (conj (digit mfns (v (+ (int 2) i))
                                 (v (+ (int 3) i)))))
          (recur (+ (int 3) i)
                 (-> nds
                   (conj (digit mfns (v i) (v (+ (int 1) i))
                                (v (+ (int 2) i)))))))))))

(deftype EmptyTree [meter-obj]
  Seqable
    (seq [_] nil)
  Sequential
  ISeq
    (first [_] nil)
    (more [this] this)
    (next [_] nil)
  IPersistentStack
    (peek [_] nil)
    (pop [this] this)
  Reversible
    (rseq [_] nil)
  IPersistentCollection
    (count [_] 0) ; not needed?
    (empty [this] this)
    (equiv [_ x] false) ; TBD
    (cons  [_ b] (newSingleTree meter-obj b))
  ConjL
    (conjl [_ a] (newSingleTree meter-obj a))
  Measured
    (measured [_] (idElem meter-obj))
    (getMeter [_] meter-obj) ; not needed?
;  Splittable
;    (split [pred acc]) ; TBD -- not needed??
  Tree
    (app3 [_ ts t2] (reduce conjl t2 (reverse ts)))
    (app3deep [_ ts t1] (reduce conj t1 ts))
    (measureMore [_] (idElem meter-obj))
    (measurePop [_] (idElem meter-obj)))

(defn ^:static newEmptyTree [meter-obj]
  (EmptyTree. meter-obj))

(defn ^:static finger-meter [meter-obj]
  (when meter-obj
    (meter
      #(measured %)
      (idElem meter-obj)
      (opfn meter-obj))))

(deftype SingleTree [meter-obj x]
  Seqable
    (seq [this] this)
  Sequential
  ISeq
    (first [_] x)
    (more [_] (EmptyTree. meter-obj))
    (next [_] nil)
  IPersistentStack
    (peek [_] x)
    (pop [_] (EmptyTree. meter-obj))
  Reversible
    (rseq [_] (list x)) ; not 'this' because tree ops can't be reversed
  IPersistentCollection
    (count [_]) ; not needed?
    (empty [_] (EmptyTree. meter-obj)) ; not needed?
    (equiv [_ x] false) ; TBD
    (cons  [_ b] (deep (digit meter-obj x)
                       (EmptyTree. (finger-meter meter-obj))
                       (digit meter-obj b)))
  ConjL
    (conjl [_ a] (deep (digit meter-obj a)
                       (EmptyTree. (finger-meter meter-obj))
                       (digit meter-obj x)))
  Measured
    (measured [_] (measure meter-obj x))
    (getMeter [_] meter-obj) ; not needed?
  Splittable
    (split [this pred acc] (let [e (empty this)] [e x e]))
  Tree
    (app3 [this ts t2] (conjl (app3 (empty this) ts t2) x))
    (app3deep [_ ts t1] (conj (reduce conj t1 ts) x))
    (measureMore [_] (idElem meter-obj))
    (measurePop [_] (idElem meter-obj)))

(defn ^:static newSingleTree [meter-obj x]
  (SingleTree. meter-obj x))

(deftype DelayedTree [tree-ref mval]
  Seqable
    (seq [this] this)
  Sequential
  ISeq
    (first [_] (first @tree-ref))
    (more [_] (rest @tree-ref))
    (next [_] (next @tree-ref))
  IPersistentStack
    (peek [_] (peek @tree-ref))
    (pop [_] (pop @tree-ref))
  Reversible
    (rseq [_] (rseq @tree-ref)) ; not this because tree ops can't be reversed
  IPersistentCollection
    (count [_]) ; not needed?
    (empty [_] (empty @tree-ref))
    (equiv [_ x] false) ; TBD
    (cons  [_ b] (conj @tree-ref b))
  ConjL
    (conjl [_ a] (conjl @tree-ref a))
  Measured
    (measured [_] mval)
    (getMeter [_] (getMeter @tree-ref)) ; not needed?
  Splittable
    (split [_ pred acc] (split @tree-ref pred acc))
  Tree
    (app3 [_ ts t2] (app3 @tree-ref ts t2))
    (app3deep [_ ts t1] (app3deep @tree-ref ts t1))
    (measureMore [_] (measureMore @tree-ref))
    (measurePop [_] (measurePop @tree-ref)))

(defmacro ^:private delay-ft [tree-expr mval]
  `(DelayedTree. (delay ~tree-expr) ~mval))
  ;`(let [v# ~mval] (assert v#) ~tree-expr))
  ;`(delayed-ft (delay (do (print "\nforce ") ~tree-expr)) ~mval))

(defn ^:static to-tree [meter-obj coll]
  (reduce conj (EmptyTree. meter-obj) coll))

(defn deep-left [pre m suf]
  (cond
    (seq pre) (deep pre m suf)
    (empty? (first m)) (to-tree (getMeter suf) suf)
    :else (deep (first m)
                (delay-ft (rest m) (measureMore m))
                suf)))

(defn deep-right [pre m suf]
  (cond
    (seq suf) (deep pre m suf)
    (empty? (peek m)) (to-tree (getMeter pre) pre)
    :else (deep pre
                (delay-ft (pop m) (measurePop m))
                (peek m))))

(defn ^:private measured3 [meter-obj pre m suf]
  (when-let [op (opfn meter-obj)]
    (op
      (op (measured pre)
          (measured m))
        (measured suf))))

(defn deep [pre m suf]
  (let [meter-obj (getMeter pre)
        op (opfn meter-obj)]
    (newDeepTree meter-obj pre m suf
                 (when op
                   (delay (if (seq m)
                            (measured3 meter-obj pre m suf)
                            (op (measured pre) (measured suf))))))))

(deftype DeepTree [meter-obj pre mid suf mval]
  Seqable
    (seq [this] this)
  Sequential
  ISeq
    (first [_] (first pre))
    (more [_] (deep-left (rest pre) mid suf))
    (next [this] (seq (rest this)))
  IPersistentStack
    (peek [_] (peek suf))
    (pop [_] (deep-right pre mid (pop suf)))
  Reversible
    (rseq [this] (lazy-seq (cons (peek this) (rseq (pop this)))))
  IPersistentCollection
    (count [_]) ; not needed?
    (empty [_] (newEmptyTree meter-obj))
    (equiv [_ x] false) ; TBD
    (cons  [_ a] (if (< (count suf) 4)
                   (deep pre mid (conj suf a))
                   (let [[e d c b] suf
                         n (digit meter-obj e d c)]
                     (deep pre (conj mid n) (digit meter-obj b a)))))
  ConjL
    (conjl [_ a] (if (< (count pre) 4)
                   (deep (conjl pre a) mid suf)
                   (let [[b c d e] pre
                         n (digit meter-obj c d e)]
                     (deep (digit meter-obj a b) (conjl mid n) suf))))
  Measured
    (measured [_] @mval)
    (getMeter [_] (getMeter pre)) ; not needed?
  Splittable ; allow to fail if op is nil:
    (split [_ pred acc]
      (let [op (opfn meter-obj)
            vpr (op acc (measured pre))]
        (if (pred vpr)
          (let [[sl sx sr] (split pre pred acc)]
            [(to-tree meter-obj sl) sx (deep-left sr mid suf)])
          (let [vm (op vpr (measured mid))]
            (if (pred vm)
              (let [[ml xs mr] (split mid pred vpr)
                    [sl sx sr] (split xs pred (op vpr (measured ml)))]
                [(deep-right pre ml sl) sx (deep-left sr mr suf)])
              (let [[sl sx sr] (split suf pred vm)]
                [(deep-right pre mid sl)
                  sx
                  (to-tree meter-obj sr)]))))))
  Tree
    (app3 [this ts t2] (app3deep t2 ts this))
    (app3deep [_ ts t1]
      (deep (.pre ^DeepTree t1)
            (app3 (.mid ^DeepTree t1)
                  (nodes meter-obj (concat (.suf ^DeepTree t1) ts pre))
                  mid)
            suf))
    (measureMore [this] (measured3 meter-obj (rest pre) mid suf))
    (measurePop  [this] (measured3 meter-obj pre mid (pop suf))))

(defn ^:static newDeepTree [meter-obj pre mid suf mval]
  (DeepTree. meter-obj pre mid suf mval))

(defn ^:static finger-tree [meter-obj & xs]
  (to-tree meter-obj xs))

(defn split-tree [t p]
  (split t p (idElem (getMeter t))))

(defn ft-concat [t1 t2]
  (assert (= (getMeter t1) (getMeter t2))) ;meters must be the same
  (app3 t1 nil t2))

(defn- seq-equals [a b]
  (boolean
    (when (or (sequential? b) (instance? java.util.List b))
      (loop [a (seq a), b (seq b)]
        (when (= (nil? a) (nil? b))
          (or
            (nil? a)
            (when (= (first a) (first b))
              (recur (next a) (next b)))))))))

;;=== applications ===

(deftype DoubleList [tree mdata]
  Object
    (equals [_ x] (seq-equals tree x))
    (hashCode [this] (hash (map identity this)))
  clojure.lang.IObj
    (meta [_] mdata)
    (withMeta [_ mdata] (DoubleList. tree mdata))
  Sequential
  Seqable
    (seq [this] (when (seq tree) this))
  ISeq
    (first [_] (first tree))
    (more [_] (DoubleList. (rest tree) mdata))
    (next [_] (if-let [t (next tree)] (DoubleList. t mdata)))
  IPersistentStack ; actually, queue
    (peek [_] (peek tree))
    (pop [_] (DoubleList. (pop tree) mdata))
  Reversible
    (rseq [_] (rseq tree)) ; not 'this' because tree ops can't be reversed
  IPersistentCollection
    (count [_] (count (seq tree))) ; Slow!
    (empty [_] (DoubleList. (empty tree) mdata))
    (equiv [_ x] (seq-equals tree x))
    (cons  [_ b] (DoubleList. (conj tree b) mdata))
  ConjL
    (conjl [_ a] (DoubleList. (conjl tree a) mdata))
  Measured
    (measured [_] (measured tree))
    (getMeter [_] (getMeter tree))
  Tree
    (app3 [_ ts t2] (DoubleList. (app3 tree ts t2) mdata))
    (app3deep [_ ts t1] (DoubleList. (app3deep tree ts t1) mdata)))

(defn double-list [& args]
  (into (DoubleList. (EmptyTree. nil) nil) args))

(deftype CountedDoubleList [tree mdata]
  Object
    (equals [_ x] (seq-equals tree x))
    (hashCode [this] (hash (map identity this)))
  clojure.lang.IObj
    (meta [_] mdata)
    (withMeta [_ mdata] (CountedDoubleList. tree mdata))
  Sequential
  Seqable
    (seq [this] (when (seq tree) this))
  ISeq
    (first [_] (first tree))
    (more [_] (CountedDoubleList. (rest tree) mdata))
    (next [_] (if-let [t (next tree)] (CountedDoubleList. t mdata)))
  IPersistentStack
    (peek [_] (peek tree))
    (pop [_] (CountedDoubleList. (pop tree) mdata))
  Reversible
    (rseq [_] (rseq tree)) ; not 'this' because tree ops can't be reversed
  IPersistentCollection
    (empty [_] (CountedDoubleList. (empty tree) mdata))
    (equiv [_ x] (seq-equals tree x))
    (cons  [_ b] (CountedDoubleList. (conj tree b) mdata))
  ConjL
    (conjl [_ a] (CountedDoubleList. (conjl tree a) mdata))
  Measured
    (measured [_] (measured tree))
    (getMeter [_] (getMeter tree)) ; not needed?
  SplitAt
    (ft-split-at [this n notfound]
      (cond
        (< n 0) [(empty this) notfound this]
        (< n (count this))
          (let [[pre m post] (split-tree tree #(> % n))]
            [(CountedDoubleList. pre mdata) m (CountedDoubleList. post mdata)])
        :else [this notfound (empty this)]))
    (ft-split-at [this n]
      (ft-split-at this n nil))
  Tree
    (app3 [_ ts t2]
      (CountedDoubleList. (app3 tree ts (.tree ^CountedDoubleList t2)) mdata))
    ;(app3deep [_ ts t1] (CountedDoubleList. (app3deep tree ts t1) mdata))
    (measureMore [_] (measureMore tree))
    (measurePop [_] (measurePop tree))
  Counted
    (count [_] (measured tree))
  Associative
    (assoc [this k v]
      (cond
        (== k -1) (conjl this v)
        (== k (measured tree)) (conj this v)
        (< -1 k (measured tree))
          (let [[pre mid post] (split-tree tree #(> % k))]
            (CountedDoubleList. (ft-concat (conj pre v) post) mdata))
        :else (throw (IndexOutOfBoundsException.))))
    (containsKey [_ k] (< -1 k (measured tree)))
    (entryAt [_ n] (clojure.lang.MapEntry.
                     n (second (split-tree tree #(> % n)))))
    (valAt [this n notfound] (if (.containsKey this n)
                               (second (split-tree tree #(> % n)))
                               notfound))
    (valAt [this n] (.valAt this n nil))
  Indexed
    (nth [this n notfound] (if (.containsKey this n)
                             (second (split-tree tree #(> % n)))
                             notfound))
    (nth [this n] (if (.containsKey this n)
                    (second (split-tree tree #(> % n)))
                    (throw (IndexOutOfBoundsException.)))))

(let [measure-len (constantly 1)
      len-meter (meter measure-len 0 +)]
  (def empty-counted-double-list
    (CountedDoubleList. (EmptyTree. len-meter) nil)))

(defn counted-double-list [& args]
  (into empty-counted-double-list args))


(defrecord Len-Right-Meter [^int len right])

(def ^:private notfound (Object.))

(deftype CountedSortedSet [cmpr tree mdata]
  Object
    (equals [_ x]
      (boolean
        (if (instance? java.util.Set x)
          (and (= (count x) (count tree)) 
               (every? #(contains? x %) tree))
          (seq-equals tree x))))
    (hashCode [_] (reduce + (map hash tree)))
  clojure.lang.IObj
    (meta [_] mdata)
    (withMeta [_ mdata] (CountedSortedSet. cmpr tree mdata))
  Seqable
    ; return 'tree' instead of 'this' so that result will be Sequential
    (seq [this] (when (seq tree) tree))
  IPersistentCollection
    (cons [this value]
      (if (empty? tree)
        (CountedSortedSet. cmpr (conj tree value) mdata)
        (let [[l x r] (split-tree tree #(>= 0 (cmpr value (:right %))))
              compared (cmpr value x)]
          (if (zero? compared)
            this ; already in set
            (let [[a b] (if (>= 0 compared) [value x] [x value])]
              (CountedSortedSet. cmpr (ft-concat (conj l a) (conjl r b)) mdata))))))
    (empty [_] (CountedSortedSet. cmpr (empty tree) mdata))
    (equiv [this x] (.equals this x)) ; TBD
  ISeq
    (first [_] (first tree))
    (more [_] (CountedSortedSet. cmpr (rest tree) mdata))
    (next [_] (if-let [t (next tree)] (CountedSortedSet. cmpr t mdata)))
  IPersistentStack
    (peek [_] (peek tree))
    (pop [_] (CountedSortedSet. cmpr (pop tree) mdata))
  Reversible
    (rseq [_] (rseq tree)) ; not 'this' because tree ops can't be reversed
  Measured
    (measured [_] (measured tree))
    (getMeter [_] (getMeter tree)) ; not needed?
  SplitAt
    (ft-split-at [this n notfound]
      (cond
        (< n 0) [(empty this) notfound this]
        (< n (count this)) (let [[l x r] (split-tree tree #(> (:len %) n))]
                             [(CountedSortedSet. cmpr l mdata) x
                              (CountedSortedSet. cmpr r mdata)])
        :else [this notfound (empty this)]))
    (ft-split-at [this n]
      (ft-split-at this n nil))
  Counted
    (count [_] (:len (measured tree)))
  ILookup
    (valAt [_ k notfound]
      (if (empty? tree)
        notfound
        (let [x (second (split-tree tree #(>= 0 (cmpr k (:right %)))))]
          (if (= x k)
            k
            notfound))))
    (valAt [this k]
      (.valAt this k nil))
  IPersistentSet
    (disjoin [this k]
      (if (empty? tree)
        this
        (let [[l x r] (split-tree tree #(>= 0 (cmpr k (:right %))))]
          (if (= x k)
            (CountedSortedSet. cmpr (ft-concat l r) mdata)
            this))))
    (get [this k] (.valAt this k nil))
  Indexed
    (nth [this n notfound] (if (< -1 n (:len (measured tree)))
                             (second (split-tree tree #(> (:len %) n)))
                             notfound))
    (nth [this n] (if (< -1 n (:len (measured tree)))
                    (second (split-tree tree #(> (:len %) n)))
                    (throw (IndexOutOfBoundsException.))))
  Sorted
    (comparator [_] cmpr)
    (entryKey [_ x] x)
    (seq [this ascending?] (if ascending?  (.seq this) (rseq tree)))
    (seqFrom [_ k ascending?]
      (let [[l x r] (split-tree tree #(>= 0 (cmpr k (:right %))))]
        (if ascending?
          (CountedSortedSet. cmpr (conjl r x) mdata)
          (rseq (conj l x)))))
  java.util.Set
    (contains [this x] (not= notfound (get this x notfound)))
    (containsAll [this xs] (every? #(contains? this %) xs))
    (isEmpty [_] (empty? tree))
    (iterator [_]
      (let [t (atom tree)]
        (reify java.util.Iterator
          (next [_] (first (swap! t next))) 
          (hasNext [_] (boolean (next @t))))))
    (size [this] (count this))
    (toArray [_] nil) ; TBD
    (toArray [_ a] nil)) ; TBD

(let [measure-lr (fn [x] (Len-Right-Meter. 1 x))
      zero-lr (Len-Right-Meter. 0 nil)
      len-lr (meter measure-lr
                    zero-lr
                    #(Len-Right-Meter. (+ (.len ^Len-Right-Meter %1)
                                          (.len ^Len-Right-Meter %2))
                                       (or (:right %2) (:right %1))))
      empty-tree (EmptyTree. len-lr)
      default-empty-set (CountedSortedSet. compare empty-tree nil)]
  (defn counted-sorted-set-by [cmpr & args]
    (into (CountedSortedSet. cmpr empty-tree nil) args))
  (defn counted-sorted-set [& args]
    (into default-empty-set args)))

;(prefer-method clojure.pprint/simple-dispatch IPersistentSet ISeq)

(defprotocol PrintableTree
  (print-tree [tree]))

(defn- p [t & xs]
  (print "<")
  (print t)
  (doseq [x xs]
    (print " ")
    (print-tree x))
  (print ">"))

(extend-protocol PrintableTree
  Digit1      (print-tree [x] (p "Digit1" (.a x)))
  Digit2      (print-tree [x] (p "Digit2" (.a x) (.b x)))
  Digit3      (print-tree [x] (p "Digit3" (.a x) (.b x) (.c x)))
  Digit4      (print-tree [x] (p "Digit4" (.a x) (.b x) (.c x) (.d x)))
  EmptyTree   (print-tree [x] (p "EmptyTree"))
  DelayedTree (print-tree [x] (p "DelayedTree" @(.tree-ref x)))
  DeepTree    (print-tree [x] (p "DeepTree" (.pre x) (.mid x) (.suf x)))
  SingleTree  (print-tree [x] (p "SingleTree" (.x x)))
  Object      (print-tree [x] (pr x)))
