(ns clojure.lang.fingertree)
(comment ; TODO:

- test performance
- add pre-packaged ctor for vector-like obj, perhaps also priority queue
- implement java.util.Collection
- implement equals
- implement IMeta
- implement IChunkedSeq?
- fix clojure core.clj to call consLeft/consRight
- replace copy/pasted code with macros
- test deque complexity
- confirm recursion is bounded, though perhaps O(log n) growth is slow enough
)

;(set! *warn-on-reflection* true)

(defprotocol DoubleSeq
  (consl [s a] "Append a to the left-hand side of s")
  (conjr [s b] "Append b to the right-hand side of s"))

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

(defprotocol Tree
  (app3 [t1 ts t2] "Append ts and (possibly deep) t2 to tree t1")
  (app3deep [t2 ts t1] "Append ts and t2 to deep tree t1")
  (measureMore [o] "Return the measure of o not including the leftmost item")
  (measurePop [o] "Return the measure of o not including the rightmost item"))

(use 'clojure.test)
(import '(clojure.lang Seqable ISeq IPersistentStack IPersistentCollection
                       Reversible Indexed))

(extend-type nil
  ObjMeter
  (measure [_ _] nil)
  (idElem [_] nil)
  (opfn [_] (constantly nil))
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
        (seq [_] (list ~@items))
      Indexed
        (count [_] ~(count items)) ; not needed?
        (nth [_ ~i notfound#]
          (cond ~@(mapcat (fn [sym n] [`(== ~i (int ~n)) sym])
                          items
                          (range (count items)))
            :else notfound#))
      IPersistentCollection
        (cons [_ x]) ; TBD
        (empty [_]) ; TBD ; not needed?
        (equiv [_ x]) ; TBD
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
      DoubleSeq
        (consl [_ x#] (digit ~'meter-obj x# ~@items))
        (conjr [_ x#] (digit ~'meter-obj ~@items x#))
      Measured
        (measured [_] @~'measure-ref)
        (getMeter [_] ~'meter-obj) ; not needed?
      Splittable
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
            (delay ~(reduce #(list 'op %1 %2)
                            (map #(list `measure 'mobj %) items)))))))

(defmacro meter [measure idElem op]
  `(reify ObjMeter
      (measure [_ a#] (~measure a#))
      (idElem [_] ~idElem)
      (opfn [_] ~op)))

(defdigit a)
(defdigit a b)
(defdigit a b c)
(defdigit a b c d)

(defn digit
  ([meter-obj a]       (make-digit meter-obj a))
  ([meter-obj a b]     (make-digit meter-obj a b))
  ([meter-obj a b c]   (make-digit meter-obj a b c))
  ([meter-obj a b c d] (make-digit meter-obj a b c d)))

(defn nodes [mfns xs]
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
  IPersistentCollection
    (cons [_ x]) ; TBD
    (count [_] 0) ; not needed?
    (empty [this] this)
    (equiv [_ x] false) ; TBD
  ISeq
    (first [_] nil)
    (more [this] this)
    (next [_] nil)
  IPersistentStack
    (peek [_] nil)
    (pop [this] this)
  Reversible
    (rseq [_] nil)
  DoubleSeq
    (consl [_ a] (newSingleTree meter-obj a))
    (conjr [_ b] (newSingleTree meter-obj b))
  Measured
    (measured [_] (idElem meter-obj))
    (getMeter [_] meter-obj) ; not needed?
;  Splittable
;    (split [pred acc]) ; TBD -- not needed??
  Tree
    (app3 [_ ts t2] (reduce consl t2 (reverse ts)))
    (app3deep [_ ts t1] (reduce conjr t1 ts))
    (measureMore [_] (idElem meter-obj))
    (measurePop [_] (idElem meter-obj)))

(defn newEmptyTree [meter-obj]
  (EmptyTree. meter-obj))

(defn finger-meter [meter-obj]
  (when meter-obj
    (meter
      #(measured %)
      (idElem meter-obj)
      (opfn meter-obj))))

(deftype SingleTree [meter-obj x]
  Seqable
    (seq [this] this)
  IPersistentCollection
    (cons [_ x]) ; TBD
    (count [_]) ; not needed?
    (empty [_] (EmptyTree. meter-obj)) ; not needed?
    (equiv [_ x]) ; TBD
  ISeq
    (first [_] x)
    (more [_] (EmptyTree. meter-obj))
    (next [_] nil)
  IPersistentStack
    (peek [_] x)
    (pop [_] (EmptyTree. meter-obj))
  Reversible
    (rseq [_] (list x)) ; not this because tree ops can't be reversed
  DoubleSeq
    (consl [_ a] (deep (digit meter-obj a)
                       (EmptyTree. (finger-meter meter-obj))
                       (digit meter-obj x)))
    (conjr [_ b] (deep (digit meter-obj x)
                       (EmptyTree. (finger-meter meter-obj))
                       (digit meter-obj b)))
  Measured
    (measured [_] (measure meter-obj x))
    (getMeter [_] meter-obj) ; not needed?
  Splittable
    (split [this pred acc] (let [e (empty this)] [e x e]))
  Tree
    (app3 [this ts t2] (consl (app3 (empty this) ts t2) x))
    (app3deep [_ ts t1] (conjr (reduce conjr t1 ts) x))
    (measureMore [_] (idElem meter-obj))
    (measurePop [_] (idElem meter-obj)))

(defn newSingleTree [meter-obj x]
  (SingleTree. meter-obj x))

(deftype DelayedTree [tree-ref mval]
  Seqable
    (seq [this] this)
  IPersistentCollection
    (cons [_ x]) ; TBD
    (count [_]) ; not needed?
    (empty [_] (empty @tree-ref))
    (equiv [_ x]) ; TBD
  ISeq
    (first [_] (first @tree-ref))
    (more [_] (rest @tree-ref))
    (next [_] (next @tree-ref))
  IPersistentStack
    (peek [_] (peek @tree-ref))
    (pop [_] (pop @tree-ref))
  Reversible
    (rseq [_] (rseq @tree-ref)) ; not this because tree ops can't be reversed
  DoubleSeq
    (consl [_ a] (consl @tree-ref a))
    (conjr [_ b] (conjr @tree-ref b))
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

(defn to-tree [meter-obj coll]
  (reduce conjr (EmptyTree. meter-obj) coll))

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

(defn- measured3 [meter-obj pre m suf]
  (let [op (opfn meter-obj)]
    (op
      (op (measured pre)
          (measured m))
        (measured suf))))

(defn deep [pre m suf]
  (let [meter-obj (getMeter pre)]
    (newDeepTree meter-obj pre m suf
                 (delay (if (seq m)
                          (measured3 meter-obj pre m suf)
                          ((opfn meter-obj) (measured pre) (measured suf)))))))

(deftype DeepTree [meter-obj pre mid suf mval]
  Seqable
    (seq [this] this)
  IPersistentCollection
    (cons [_ x]) ; TBD
    (count [_]) ; not needed?
    (empty [_] (empty pre))
    (equiv [_ x]) ; TBD
  ISeq
    (first [_] (first pre))
    (more [_] (deep-left (rest pre) mid suf))
    (next [this] (seq (rest this)))
  IPersistentStack
    (peek [_] (peek suf))
    (pop [_] (deep-right pre mid (pop suf)))
  Reversible
    (rseq [this] (lazy-seq (cons (peek this) (rseq (pop this)))))
  DoubleSeq
    (consl [_ a] (if (< (count pre) 4)
                   (deep (consl pre a) mid suf)
                   (let [[b c d e] pre
                         n (digit meter-obj c d e)]
                     (deep (digit meter-obj a b) (consl mid n) suf))))
    (conjr [_ a] (if (< (count suf) 4)
                   (deep pre mid (conjr suf a))
                   (let [[e d c b] suf
                         n (digit meter-obj e d c)]
                     (deep pre (conjr mid n) (digit meter-obj b a)))))
  Measured
    (measured [_] @mval)
    (getMeter [_] (getMeter pre)) ; not needed?
  Splittable
    (split [_ pred acc]
      (let [op (opfn meter-obj)
            vpr (op acc (measured pre))]
        (if (pred vpr)
          (let [[sl sx sr] (split pre pred acc)]
            [(to-tree meter-obj sl) sx (deep-left sr mid suf)])
          (let [vm (op vpr (measured mid))]
            (if (pred vm)
              (let [[ml xs mr] (split mid pred vpr)
                    [sl sx sr] (split (apply digit meter-obj xs)
                                      pred
                                      (op vpr (measured ml)))]
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

(defn newDeepTree [meter-obj pre mid suf mval]
  (DeepTree. meter-obj pre mid suf mval))

(defn finger-tree [meter-obj & xs]
  (to-tree meter-obj xs))

(defn split-tree [t p]
  (split t p (idElem (getMeter t))))

(defn ft-concat [t1 t2]
  (assert (= (getMeter t1) (getMeter t2))) ;meters must be the same
  (app3 t1 nil t2))

;;=== tests ===

(deftest Conj-Seq-Queue
  (let [len 100]
    (are [x] (= (map identity x) (range len))
      (rseq (reduce consl (finger-tree nil) (range len)))
      (seq  (reduce conjr (finger-tree nil) (range len))))))

(deftest Conj-Seq-Stack
  (let [len 100]
    (are [x] (= (map identity x) (range (dec len) -1 -1))
      (rseq (reduce conjr (finger-tree nil) (range len)))
      (seq  (reduce consl (finger-tree nil) (range len))))))
    
(deftest Conj-Seq-Mixed
  (doseq [m (range 2 7)]
    (loop [ft (finger-tree nil), vc [], i (int 0)]
      (when (< i 40)
        (is (= (seq (map identity ft)) (seq vc)))
        (if (zero? (rem i m))
          (recur (consl ft i) (vec (cons i vc)) (inc i))
          (recur (conjr ft i) (conj vc i)       (inc i)))))))

(deftest Concat
  (doseq [a-len (range 25), b-len (range 25)]
    (let [a-s (map #(symbol (str % 'a)) (range a-len))
          b-s (map #(symbol (str % 'b)) (range b-len))
          a (apply finger-tree nil a-s)
          b (apply finger-tree nil b-s)]
      (is (= (seq (concat a-s b-s)) (seq (map identity (ft-concat a b))))))))


(def len-meter (meter (constantly 1) 0 +))
(def string-meter (meter str "" str))

(defrecord Len-String-Meter [len string])

(def len-string-meter
  (meter
    (fn [o]
      (Len-String-Meter. (measure len-meter o) (measure string-meter o)))
    (Len-String-Meter. (idElem len-meter) (idElem string-meter))
    (fn [a b] (Len-String-Meter.
                ((opfn len-meter) (:len a) (:len b))
                ((opfn string-meter) (:string a) (:string b))))))


(deftest Annotate-One-Direction
  (let [measure-fns len-string-meter]
    (let [len 100]
      (are [x] (= x (Len-String-Meter. len (apply str (range len))))
        (measured (reduce conjr (finger-tree measure-fns) (range len))))
      (are [x] (= x (Len-String-Meter. len (apply str (reverse (range len)))))
        (measured (reduce consl (finger-tree measure-fns) (range len)))))))
      
(deftest Annotate-Mixed-Conj
  (let [measure-fns len-string-meter]
    (doseq [m (range 2 7)]
      (loop [ft (finger-tree measure-fns), vc [], i (int 0)]
        (when (< i 40)
          (is (= (measured ft) (Len-String-Meter. (count vc) (apply str vc))))
          (if (zero? (rem i m))
            (recur (consl ft i) (vec (cons i vc)) (inc i))
            (recur (conjr ft i) (conj vc i)       (inc i))))))))

(deftest Ann-Conj-Seq-Queue
  (let [len 100]
    (are [x] (= (map identity x) (range len))
      (rseq (reduce consl (finger-tree len-meter) (range len)))
      (seq  (reduce conjr (finger-tree len-meter) (range len))))))

(deftest Annotate-Concat
  (let [measure-fns len-string-meter]
    (doseq [a-len (range 25), b-len (range 25)]
      (let [a-s (map #(symbol (str % 'a)) (range a-len))
            b-s (map #(symbol (str % 'b)) (range b-len))
            a (apply finger-tree measure-fns a-s)
            b (apply finger-tree measure-fns b-s)]
        (is (= (Len-String-Meter.
                 (+ (count a-s) (count b-s))
                 (apply str (concat a-s b-s)))
               (measured (ft-concat a b))))))))

(deftest Split
  (let [mfns len-string-meter
        make-item (fn [i] (symbol (str i 'a)))]
    (doseq [len (range 10)
            :let [tree (to-tree mfns (map make-item (range len)))]
            split-i (range len)]
      (is (= [len split-i (make-item split-i)]
             [len split-i (second (split-tree tree #(< split-i (:len %))))])))))
