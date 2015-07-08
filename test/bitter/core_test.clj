(ns bitter.core-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [bitter.core :refer :all]))

(def max-tested-bitmap-size (* 64 3))

(def gen-bitmap-size (gen/choose 1 max-tested-bitmap-size))
(def gen-bit-position (gen/sized #(gen/choose 0 (dec %))))
;; Allow duplicate positions
(def gen-bit-positions (gen/vector gen-bit-position))

(def gen-bitwise-bitmap-op (gen/tuple (gen/elements [:set :clear])
                                      gen-bit-position))
(def gen-binary-bitmap-op (gen/tuple (gen/elements [:and :or :xor])
                                     gen-bit-positions))
(def gen-unary-bitmap-op (gen/return [:not]))

(def gen-op (gen/sized (fn [n]
                         (gen/resize n (gen/frequency [[2 gen-bitwise-bitmap-op]
                                                       [3 gen-binary-bitmap-op]
                                                       #_[1 gen-unary-bitmap-op]])))))

(def gen-bit-sequence-test-scenario
  (gen/bind gen-bitmap-size
            (fn [n]
              (gen/tuple (gen/return n)
                         (gen/resize n gen-bit-positions)))))

(def gen-small-bit-sequence-test-scenario
  (gen/bind (gen/choose 1 5)
            (fn [n]
              (gen/tuple (gen/return n)
                         (gen/resize n gen-bit-positions)))))

(def gen-dual-bit-sequence-test-scenario
  (gen/bind gen-bitmap-size
            (fn [n]
              (gen/tuple (gen/return n)
                         (gen/resize n gen-bit-positions)
                         (gen/resize n gen-bit-positions)))))

(def gen-operator-test-scenario
  (gen/bind gen-bitmap-size
            (fn [n]
              (gen/tuple (gen/return n)
                         (gen/vector (gen/resize n gen-op))))))

(defspec acts-like-a-set-wrt-count
  (prop/for-all [[n bits] gen-bit-sequence-test-scenario]
                (= (count (set bits))
                   (count (bitmap n bits)))))

(defspec acts-like-a-set-wrt-seq
  (prop/for-all [[n bits] gen-bit-sequence-test-scenario]
                (= (set (seq (set bits)))
                   (set (seq (bitmap n bits))))))

(defspec acts-like-a-set-wrt-conj
  (prop/for-all [[n first-bits more-bits] gen-dual-bit-sequence-test-scenario]
                (= (sort (apply conj (set first-bits) more-bits))
                   (sort (apply conj (bitmap n first-bits) more-bits)))))

(defspec acts-like-a-set-wrt-into
  (prop/for-all [[n first-bits more-bits] gen-dual-bit-sequence-test-scenario]
                (= (sort (into (set first-bits) more-bits))
                   (sort (into (bitmap n first-bits) more-bits)))))

(defspec bitmaps-are-equiv-iff-sizes-and-bits-match
  (prop/for-all [[n bits] gen-small-bit-sequence-test-scenario
                 [m other-bits] gen-small-bit-sequence-test-scenario]
                (let [bm1 (bitmap n bits)
                      bm2 (bitmap m other-bits)]
                  (if (and (= n m)
                           (= (set bits) (set other-bits)))
                    (= bm1 bm2)
                    (not= bm1 bm2)))))
