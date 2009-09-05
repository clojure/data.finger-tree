;(ns clojure.contrib.finger-tree)
(comment ; TODO:

- clean up interfaces
- clean up own namespace name
- fix clojure core.clj to call consLeft/consRight
- fix any undelayed middle trees
- confirm recursion is bounded -- fix if not
- test performance
- add pre-packaged ctor for vector-like obj, perhaps also priority queue
- implement java.util.Collection
- implement equals
- implement IChunkedSeq?
- replace copy/pasted code with macros
)

(use 'clojure.test)

(gen-interface
  :name clojure.lang.IFingerTreeNode
  :methods [[pre [] clojure.lang.IFingerTreeNode]
            [mid [] clojure.lang.IFingerTreeNode]
            [suf [] clojure.lang.IFingerTreeNode]
            [consLeft  [Object] clojure.lang.IFingerTreeNode]
            [consRight [Object] clojure.lang.IFingerTreeNode]
            [app3 [clojure.lang.ISeq clojure.lang.IFingerTreeNode]
                  clojure.lang.IFingerTreeNode]
            [app3deep [clojure.lang.ISeq clojure.lang.IFingerTreeNode]
                      clojure.lang.IFingerTreeNode]
            [split [clojure.lang.IFn Object] clojure.lang.IPersistentVector]
            [measure [] Object]
            [measureFns [] clojure.lang.IPersistentCollection]])

(gen-interface
  :name clojure.lang.IMeasureFn
  :methods [[iden [] Object]
            [measure [Object] Object]
            [reduce [Object Object] Object]])

(gen-interface
  :name clojure.lang.IPrintable
  :methods [[print [java.io.Writer] Object]])

(import '(clojure.lang Indexed Seqable ISeq IFingerTreeNode IPrintable
                       IPersistentStack Reversible))

(defn mes* [cache-fns & xs]
  (into cache-fns
        (if (instance? IFingerTreeNode (first xs))
          (let [mes-maps (map #(.measure #^IFingerTreeNode %) xs)]
            (for [[k [mes red]] cache-fns]
              [k (reduce red (map k mes-maps))]))
          (for [[k [mes red]] cache-fns]
            [k (reduce red (map mes xs))]))))

(defn red* [cache-fns v1 v2]
  (zipmap (keys cache-fns)
          (for [[k [mes red]] cache-fns]
            (red (k v1) (k v2)))))

(defn iden* [cache-fns]
  (into cache-fns (for [[k [_ _ iden]] cache-fns] [k iden])))


(defmethod print-method IPrintable [x w] (.print #^IPrintable x w))
(prefer-method print-method IPrintable ISeq)

(def asplit vector)

(defn #^IFingerTreeNode digit [measure-fns & xs]
  (assert (<= 1 (count xs) 4))
  (let [xs-vec (vec xs)]
    (new [IFingerTreeNode ISeq IPersistentStack Indexed IPrintable] this
      (consLeft  [x] (apply digit measure-fns x xs-vec))
      (consRight [x] (apply digit measure-fns (conj xs-vec x)))
      (measure   []  (apply mes* measure-fns xs-vec))
      (measureFns[]  measure-fns)
      (split     [p i] (loop [i i, l [], [x & xs] xs-vec]
                         (let [i* (red* measure-fns i (mes* measure-fns x))]
                           (if (p i*)
                             (asplit (when (seq l) (apply digit measure-fns l))
                                     x
                                     (when xs (apply digit measure-fns xs)))
                             (recur i* (conj l x) xs)))))
      (nth       [i] (nth xs-vec i))
      (count     []  (count xs-vec))
      (seq       []  (seq xs-vec))
      (first     []  (nth xs-vec 0))
      (next      []  (when (> (count xs-vec) 1)
                       (apply digit measure-fns (next xs-vec))))
      (peek      []  (peek xs-vec))
      (pop       []  (when (> (count xs-vec) 1)
                       (apply digit measure-fns (pop xs-vec))))
      (toString  []  (str xs-vec))
      (print     [w] (.write w (str "#<digit " this ">"))))))

(defn #^IFingerTreeNode node2 [measure-fns a b]
  (let [mval (mes* measure-fns a b)]
    (new [IFingerTreeNode Indexed Seqable IPrintable] this
      (measure   []  mval)
      (measureFns[]  measure-fns)
      (nth       [i] (if (zero? i) a b))
      (count     []  (int 2))
      (seq       []  (list a b))
      (toString  []  (str [a b] mval))
      (print     [w] (.write w (str "#<node2 " this ">"))))))

(defn #^IFingerTreeNode node3 [measure-fns a b c]
  (let [mval (mes* measure-fns a b c)]
    (new [IFingerTreeNode Indexed Seqable IPrintable] this
      (measure   []  mval)
      (measureFns[]  measure-fns)
      (nth       [i] (condp == i (int 0) a (int 1) b c))
      (count     []  (int 3))
      (seq       []  (list a b c))
      (toString  []  (str [a b c] mval))
      (print     [w] (.write w (str "#<node3 " this ">"))))))

(defn- nodes
  ([mfns a b]          (list (digit mfns a b)))
  ([mfns a b c]        (list (digit mfns a b c)))
  ([mfns a b c d]      (list (digit mfns a b) (digit mfns c d)))
  ([mfns a b c d & xs] (lazy-seq  ; lazy to avoid stack overflow
                         (cons (digit mfns a b c) (apply nodes mfns d xs)))))

(declare single deep)

(defn #^IFingerTreeNode empty-ft [measure-fns]
  (new [IFingerTreeNode ISeq IPersistentStack Reversible IPrintable] this
    (consLeft  [a] (single measure-fns a))
    (consRight [b] (single measure-fns b))
    (app3      [ts t2] (reduce #(.consLeft #^IFingerTreeNode %1 %2)
                               (or t2 (empty-ft measure-fns))
                               (reverse ts)))
    (app3deep  [ts t1] (let [t2 this]
                         (reduce #(.consRight #^IFingerTreeNode %1 %2) t1 ts)))
    (measure   []  (iden* measure-fns))
    (measureFns[]  measure-fns)
    (count     []  0)
    (seq       []  nil)
    (rseq      []  nil)
    (first     []  nil)
    (more      []  this)
    (next      []  nil)
    (peek      []  nil)
    (pop       []  this)
    (toString  []  (str (keys measure-fns)))
    (print     [w] (.write w (str "#<empty " this ">")))))

(defn single [measure-fns x]
  (new [IFingerTreeNode ISeq IPersistentStack Reversible IPrintable] this
    (consLeft  [a] (deep (digit measure-fns a)
                         (empty-ft measure-fns)
                         (digit measure-fns x)))
    (consRight [b] (deep (digit measure-fns x)
                         (empty-ft measure-fns)
                         (digit measure-fns b)))
    (app3      [ts t2] (.consLeft (.app3 (empty-ft measure-fns) ts t2) x))
    (app3deep  [ts t1] (let [t2 this]
                         (.consRight
                           #^IFingerTreeNode (reduce #(.consRight #^IFingerTreeNode %1 %2) t1 ts)
                           x)))
    (measure   []  (mes* measure-fns x))
    (measureFns[]  measure-fns)
    (split     [p i] (let [e (empty-ft measure-fns)] (asplit e x e)))
    (seq       []  this)
    (rseq      []  (list x))
    (first     []  x)
    (more      []  (empty-ft measure-fns))
    (next      []  nil)
    (peek      []  x)
    (pop       []  (empty-ft measure-fns))
    (toString  []  (str x " " (.measure #^IFingerTreeNode this)))
    (print     [w]
      (binding [*out* w]
        (print "#<single ")
        (pr x)
        (pr (.measure #^IFingerTreeNode this))
        (print ">")))))

(defn delayed-ft [tree-ref mval]
  (new [IFingerTreeNode ISeq IPersistentStack IPrintable] this
    (consLeft  [a] (.consLeft  #^IFingerTreeNode @tree-ref a))
    (consRight [b] (.consRight #^IFingerTreeNode @tree-ref b))
    (app3      [ts t2] (.app3  #^IFingerTreeNode @tree-ref ts t2))
    (app3deep  [ts t1] (.app3deep #^IFingerTreeNode @tree-ref ts t1))
    (measure   []  mval)
    (measureFns[]  (.measureFns #^IFingerTreeNode @tree-ref))
    (split     [p i] (.split #^IFingerTreeNode @tree-ref p i))
    (seq       []  this) ; empty trees are never delayed
    (rseq      []  this)
    (first     []  (.first #^ISeq @tree-ref))
    (more      []  (.more  #^ISeq @tree-ref))
    (next      []  (.next  #^ISeq @tree-ref))
    (peek      []  (.peek  #^IPersistentStack @tree-ref))
    (pop       []  (.pop   #^IPersistentStack @tree-ref))
    (toString  []  (.toString #^Object @tree-ref))
    (print     [w]
      (binding [*out* w]
        (print "#<delay ")
        (pr @tree-ref)
        (print ">")))))

(defmacro delay-ft [tree-expr mval]
  `(delayed-ft (delay ~tree-expr) ~mval))

(defn to-tree [measure-fns s]
  (reduce #(.consRight #^IFingerTreeNode %1 %2) (empty-ft measure-fns) s))

(defn deep-left [pre, m, #^IFingerTreeNode suf]
  (cond
    pre     (deep pre m suf)
    (seq m) (deep
              (apply digit (.measureFns #^IFingerTreeNode m) (.first #^ISeq m))
              (.more #^ISeq m) ; TBD delay
              suf)
    :else   (to-tree (.measureFns suf) suf)))

(defn #^IFingerTreeNode deep-right
  [#^IFingerTreeNode pre, #^IPersistentStack m, suf]
  (cond
    suf     (deep pre m suf)
    (seq m) (deep
              pre
              (.pop m) ; TBD delay
              (apply digit (.measureFns #^IFingerTreeNode m) (.peek m)))
    :else   (to-tree (.measureFns pre) pre)))

(defn deep [#^IFingerTreeNode pre, #^IFingerTreeNode m, #^IFingerTreeNode suf]
  (assert (= (.measureFns pre) (.measureFns suf)))
  (let [measure-fns (.measureFns pre)
        mval (if (.seq #^ISeq m)
               (mes* measure-fns pre m suf)
               (mes* measure-fns pre suf))]
    (new [IFingerTreeNode ISeq IPersistentStack Reversible IPrintable] this
      (pre [] pre)
      (mid [] m)
      (suf [] suf)
      (consLeft  [a] (if (< (count pre) 4)
                       (deep (.consLeft pre a) m suf)
                       (let [[b c d e] pre
                             n (node3 measure-fns c d e)]
                         (deep (digit measure-fns a b)
                               (if (.seq #^ISeq m)
                                 (delay-ft (.consLeft m n)
                                           (mes* measure-fns n m))
                                 (single measure-fns n))
                               suf))))
      (consRight [a] (if (< (count suf) 4)
                       (deep pre m (.consRight suf a))
                       (let [[e d c b] suf
                             n (node3 measure-fns e d c)]
                         (deep pre
                               (if (seq m)
                                 (delay-ft (.consRight m n)
                                           (mes* measure-fns m n))
                                 (single measure-fns n))
                               (digit measure-fns b a)))))
      (app3      [ts t2] (.app3deep t2 ts this))
      (app3deep  [ts t1] (let [t2 #^IFingerTreeNode this]
                           (deep (.pre t1)
                                 (.app3 (.mid t1)
                                        (apply nodes measure-fns
                                               (concat (.suf t1) ts (.pre t2)))
                                        (.mid t2))
                                 (.suf t2))))
      (measure   []  mval)
      (measureFns[]  measure-fns)
      (split     [p i]
        (let [vpr (red* measure-fns i (.measure pre))
              vm  (red* measure-fns vpr (.measure m))]
          (cond
            (p vpr) (let [[sl sx sr] (.split pre p i)]
                      (asplit (to-tree measure-fns sl) sx (deep-left sr m suf)))
            (p vm) (let [[ml xs mr] (.split m p vpr)
                         [sl sx sr]
                           (.split
                             #^IFingerTreeNode (apply digit measure-fns xs)
                             p
                             (red* measure-fns vpr (mes* measure-fns ml)))]
                     (asplit (deep-right pre ml sl) sx (deep-left sr mr suf)))
            :else (let [[sl sx sr] (.split suf p vm)]
                    (asplit (deep-right pre m sl)
                            sx
                            (to-tree measure-fns sr))))))
      (seq       []  this)
      (rseq      []  (lazy-seq (cons (.peek #^IPersistentStack this)
                                     (rseq (.pop #^IPersistentStack this)))))
      (first     []  (.first #^ISeq pre))
      (more      []  (deep-left (.next #^ISeq pre) m suf))
      (next      []  (.seq (.more #^ISeq this)))
      (peek      []  (.peek #^IPersistentStack suf))
      (pop       []  (deep-right pre m (.pop #^IPersistentStack suf)))
      (toString  []  "deep-finger-tree")
      (print     [w]
        (binding [*out* w]
          (print "#<deep ")
          (pr pre m suf)
          (print " ")
          (pr (.measure #^IFingerTreeNode this))
          (print ">"))))))

(defn finger-tree [measure-fns & xs]
  (to-tree measure-fns xs))

(defn split-tree [t p]
  (.split t p (iden* (.measureFns t))))

(defn ft-concat [#^IFingerTreeNode t1 #^IFingerTreeNode t2]
  (assert (= (.measureFns t1) (.measureFns t2))) ; cache-fns must be the same
  (.app3 t1 nil t2))

;;=== tests ===

(defn consl [t a] (.consLeft #^IFingerTreeNode t a))
(defn conjr [t a] (.consRight #^IFingerTreeNode t a))

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

(deftest Annotate-One-Direction
  (let [cache-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (let [len 100]
      (are [x] (= x {:size len :str (apply str (range len))})
        (.measure (reduce conjr (finger-tree cache-fns) (range len))))
      (are [x] (= x {:size len :str (apply str (reverse (range len)))})
        (.measure (reduce consl (finger-tree cache-fns) (range len)))))))
      
(deftest Annotate-Mixed-Conj
  (let [cache-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (doseq [m (range 2 7)]
      (loop [ft (finger-tree cache-fns), vc [], i (int 0)]
        (when (< i 40)
          (is (= (.measure ft) {:size (count vc) :str (apply str vc)}))
          (if (zero? (rem i m))
            (recur (consl ft i) (vec (cons i vc)) (inc i))
            (recur (conjr ft i) (conj vc i)       (inc i))))))))

(deftest Annotate-Concat
  (let [cache-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (doseq [a-len (range 25), b-len (range 25)]
      (let [a-s (map #(symbol (str % 'a)) (range a-len))
            b-s (map #(symbol (str % 'b)) (range b-len))
            a (apply finger-tree cache-fns a-s)
            b (apply finger-tree cache-fns b-s)]
        (is (= {:size (+ (count a-s) (count b-s))
                :str (apply str (concat a-s b-s))}
               (.measure (ft-concat a b))))))))

(deftest Split
  (let [mfns {:size [(constantly 1) + 0] :str [str str ""]}
        make-item (fn [i] (symbol (str i 'a)))]
    (doseq [len (range 100)
            :let [tree (to-tree mfns (map make-item (range len)))]
            split-i (range len)]
      (is (= (make-item split-i)
             (nth (split-tree tree #(< split-i (:size %))) 1))))))
