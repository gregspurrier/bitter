(ns bitter.core)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; On the JVM use longs, which hold 64 bits
(def bits-per-word-log2 6)
(def bits-per-word (bit-shift-left 1 ^long bits-per-word-log2))
(def bit-index-mask (dec ^long bits-per-word))

(defmacro n->word-index [n]
  `(unsigned-bit-shift-right ~n ~bits-per-word-log2))
(defmacro n->bit-index [n]
  `(bit-and ^long ~n ~bit-index-mask))

;; From http://chessprogramming.wikispaces.com/De+Bruijn+Sequence+Generator
(def de-bruijn-table
  [ 0  1  2 53  3  7 54 27
    4 38 41  8 34 55 48 28
   62  5 39 46 44 42 22  9
   24 35 59 56 49 18 29 11
   63 52  6 26 37 40 33 47
   61 45 43 21 23 58 17 10
   51 25 36 32 60 20 57 16
   50 31 19 15 30 14 13 12])

(defn- bit-index ^long [^long x]
  (de-bruijn-table (unsigned-bit-shift-right (* x 0x022fdd63cc95386d) 58)))

(defn- add-word-elements! [x ^long offset accum]
  (loop [^long x x]
    (when (not= x 0)
      (let [x-with-isolated-right-bit (bit-and x (- x))
            element (+ (bit-index x-with-isolated-right-bit) offset)
            next-x (bit-xor x x-with-isolated-right-bit)]
        (do (conj! accum element)
            (recur next-x))))))

(defn- get-elements [words]
  (let [num-words (count words)
        accum (transient [])]
    (loop [index 0]
      (when (< index num-words)
        (add-word-elements! (get words index) (* index ^long bits-per-word) accum)
        (recur (inc index))))
    (persistent! accum)))

(defprotocol PBitmap
  (capacity [x])
  (bitmap-test [x n]))

(declare bitmap create-persistent-bitmap)

;; size: the number of bit positions represented by the bitmap
;; words: a vector of longs containing the bitmap's bits
(deftype TransientBitmap [size words]
  clojure.lang.ITransientCollection
  (conj [this n]
    {:pre [(< -1 n size)]}
    (let [^long n n
          word-index (n->word-index n)]
      (assoc! words word-index (bit-set ^long (get words word-index)
                                      (n->bit-index n))))
    this)

  (persistent [_]
    (create-persistent-bitmap nil size (persistent! words))))

;; _meta: metadata associated with the bitmap
;; size: the number of bit positions represented by the bitmap
;; words: a vector of longs containing the bitmap's bits
(deftype PersistentBitmap [_meta size words]
  java.util.Collection
  (toArray [_]
    (to-array (get-elements words)))

  clojure.lang.IPersistentCollection
  (seq [_]
    (clojure.core/seq (get-elements words)))

  ;; For IPersistentCollections that do not implement Counted, core.clojure/count
  ;; is implemented by iterating over the seq. The object's .count method is not
  ;; used.
  ;; (count [this])

  (cons [this n]
    {:pre [(< -1 n size)]}
    (let [^long n n
          word-index (n->word-index n)
          bit-index (n->bit-index n)]
      (PersistentBitmap. _meta size (assoc words word-index
                                           (bit-set (words word-index) bit-index)))))

  (empty [_]
    (with-meta (bitmap size) _meta))

  (equiv [this o]
    (and (instance? PersistentBitmap o)
         (= size (capacity ^PersistentBitmap o))
         ;; seq always returns the elements in order
         (= (seq this) (seq o))))

  clojure.lang.IEditableCollection
  (asTransient [_]
    (TransientBitmap. size (transient words)))

  clojure.lang.IObj
  (meta [_] _meta)

  (withMeta [_ m]
    (PersistentBitmap. m size words))

  PBitmap
  (capacity [_] size)

  (bitmap-test [_ n]
    {:pre [(< -1 n size)]}
    (let [^long n n
          word-index (n->word-index n)
          bit-index (n->bit-index n)]
      (bit-test (words word-index) bit-index))))

(defn- create-persistent-bitmap [metadata capacity words]
  (PersistentBitmap. metadata capacity words))

(defn bitmap
  "Creates a new persistent bitmap with capacity for n bits"
  ([^long n]
     {:pre [(> n 0)]}
     (let [num-words (inc (n->word-index (dec n)))
           words (vec (repeat num-words 0))]
       (create-persistent-bitmap nil n words)))
  ([n bits]
     (into (bitmap n) bits)))
