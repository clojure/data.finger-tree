{:namespaces
 ({:source-url
   "https://github.com/clojure/data.finger-tree/blob/f798322d5ee1ccb5801322e99caf9a5541a8e3b4/src/main/clojure/clojure/data/finger_tree.clj",
   :wiki-url
   "http://clojure.github.com/data.finger-tree/clojure.data.finger-tree-api.html",
   :name "clojure.data.finger-tree",
   :author "Chris Houser",
   :doc "Persistent collections based on 2-3 finger trees."}),
 :vars
 ({:file "src/main/clojure/clojure/data/finger_tree.clj",
   :raw-source-url
   "https://github.com/clojure/data.finger-tree/raw/f798322d5ee1ccb5801322e99caf9a5541a8e3b4/src/main/clojure/clojure/data/finger_tree.clj",
   :source-url
   "https://github.com/clojure/data.finger-tree/blob/f798322d5ee1ccb5801322e99caf9a5541a8e3b4/src/main/clojure/clojure/data/finger_tree.clj#L33",
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/ObjMeter",
   :namespace "clojure.data.finger-tree",
   :line 33,
   :var-type "var",
   :doc
   "Object for annotating tree elements.  idElem and op together form a Monoid.",
   :name "ObjMeter"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/app3",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([t1 ts t2]),
   :doc "Append ts and (possibly deep) t2 to tree t1",
   :name "app3"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/app3deep",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([t2 ts t1]),
   :doc "Append ts and t2 to deep tree t1",
   :name "app3deep"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/conjl",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([s a]),
   :doc "Append a to the left-hand side of s",
   :name "conjl"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/ft-split-at",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([o k notfound] [o k]),
   :doc "Return [pre m post] where pre and post are trees",
   :name "ft-split-at"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/getMeter",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([o]),
   :doc "Return the meter object for o",
   :name "getMeter"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/idElem",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([_]),
   :doc "Return the identity element for this meter",
   :name "idElem"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/measure",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([_ o]),
   :doc "Return the measured value of o (same type as idElem)",
   :name "measure"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/measureMore",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([o]),
   :doc "Return the measure of o not including the leftmost item",
   :name "measureMore"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/measurePop",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([o]),
   :doc "Return the measure of o not including the rightmost item",
   :name "measurePop"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/measured",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([o]),
   :doc "Return the measured value of o",
   :name "measured"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/opfn",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([_]),
   :doc
   "Return an associative function of two args for combining measures",
   :name "opfn"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/data.finger-tree//clojure.data.finger-tree-api.html#clojure.data.finger-tree/split",
   :namespace "clojure.data.finger-tree",
   :var-type "function",
   :arglists ([o pred acc]),
   :doc "Return [pre m post] where pre and post are trees",
   :name "split"})}
