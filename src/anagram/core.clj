(ns anagram.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [clojure.java.io :as java-io]))


;; anagrams.clj
;;
;; The problem is - given a word from a wordlist, find one anagram with the
;; most number of words (from the given wordlist) and one anagram with just
;; two words in them (if one exists). If no anagrams exist with two or more
;; words, the program should print an empty string for both cases.




;; Notes:
;; The lookup problem is more or less solved, generating anagrams to
;; look up is not.
;; The current method is to step through the lexical order of
;; partitions (generated lazily by clojure.math.combinatorics/partitions)
;; and check them one by one.  Not included here is the algorithm I'm
;; working on to avoid this.
;;
;; Thanks to a data-structure of nested hash-maps representing
;; the word list performing lookups is extremely fast.  Forcing the
;; partition function to run without checking against the wordlist is
;; almost as fast as with checking (here ran with a numeral which
;; guarantees that no anagram will be found.  'parts' is called within
;; get-one-anagram and every member is checked against the word list):
;;
;; anagram.core=> (time (dorun (parts "halzotqwre1")))
;; "Elapsed time: 37211.583681 msecs"
;; nil
;;
;; anagram.core=> (time (get-one-anagram "halzotqwre1"))
;; "Elapsed time: 40578.567359 msecs"
;; false
;;
;; Generating partitions takes over 90% of the time at this point.
;; Interestingly duplicates (and triplicates etc.) in the input string
;; lower completion time noticably.  



;; Notes if you're not familiar with Clojure but are familiar with
;; Lisp:
;;
;; Clojure has more brackets than Scheme:
;; (): the usual linked list
;; []: this is a perisitent vector, it's backed by a binary tree so
;;     pulling from the head and the tail is the same
;; {}: hash-map
;; ->, -->, ->>: these are the threading macros.  They operate like
;;               the pipe in the unix shell (output of the first
;;               becomes input of the second and so-on)
;;               (-> x H G F) is equivalent to (F (G (H x)))
;;
;; (fn [x] (+ 5 x)), #(+ 5 %): lambda function syntax
;; (defn name [args] body): function syntax
;;
;; "first" is car
;; "rest" is cdr
;; cons is rarely used, conj is more common but operates differently
;; on different data types (appends to the head of a list, tail of
;; a vector)
;;
;; Tail-call optimization requires loop/recur in order to work.
;; Laziness is common.


;; wordlist (relative) locations.  relative to resources folder.
(def loc1
  "google-10000-english-usa.txt")
(def loc2
  "words.txt")


(def word-list
  "Load the word list, split it, make everything lowercase."
  (-> "words.txt"
      java-io/resource
      java-io/file
      slurp
      string/split-lines
      (#(map string/lower-case %))))


(defn as-vec [word]
  ;; "abcd" --> ["a" "b" "c" "d"]
  (vec (map str (seq word))))

(defn assoc-deep   [m ks e]
  "Updates nested maps non-destructively, creates keys if there
   are none.  Last key is set to the default value {e nil}"
  ;; m: map, ks: list of keys, d: default end tag
  (let [k (first ks)]
      (cond (empty? ks) ;; if no more keys
            {e nil} ;; end tag
            (contains? m (first ks)) ;; if the key exists
            (assoc m k (merge (m k)  ;; combine new and old
                              (assoc-deep (m k) (rest ks) e))) 
            :else
            (assoc m k (assoc-deep {} (rest ks) e))))) ;; new map

(defn add-word [dict _word]
  ;; add word to the Setrie (nested hash-map) structure
  (assoc-deep dict
              (as-vec _word)
              :end))
    
(defn make-dictionary [words]
  "Returns a nested hash-map structure representing all of the given words."
  (loop [words words
         dict {}]
    (cond (empty? words)
          dict
          :else
          (recur (rest words)
                 (add-word dict (first words))))))

;; => (make-dictionary ["hello" "there" "theodore" "that" "thing" "there" "thinks" "the" "thoughts"])
;; {"t" {"h" {"o" {"u" {"g" {"h" {"t" {"s" {:end nil}}}}}},
;;            "i" {"n" {"k" {"s" {:end nil}},
;;                      "g" {:end nil}}},
;;            "a" {"t" {:end nil}},
;;            "e" {:end nil,
;;                 "o" {"d" {"o" {"r" {"e" {:end nil}}}}},
;;                 "r" {"e" {:end nil}}}}},
;;  "h" {"e" {"l" {"l" {"o" {:end nil}}}}}}
;;
;;
;; After I thought of this I looked it up I found it's called a
;; GADDAD by its first creator who came up with it while working
;; on a Scrabble algorithm.
;; Since it's a Trie with single elements I'm calling it a SETrie
;; or Setrie instead of GADDAD.
;; I was surprised to find it's a somewhat rare data structure,
;; it seems more intuitive than a Trie.  It makes lookups fast,
;; checking for string membership it does about 100,000 per
;; second.
;;
;; using Clojure's get-in operator is extremely fast for
;; lookups, but a custom checking algorithm that only calls
;; 'contains?' might be faster:  'contains?' runs in O(1)
;; worst-case, so calling 'contains?' n times should be O(n)
;;
;; (At this point the algorithm takes roughly 200x more time to
;;  generate the partitions than it does to check them.)



(def word-dict (make-dictionary word-list))

(defn legal-string? [s]
  "Check if a string is either a string or substring in words.txt"
  (not (false? (get-in word-dict
                       (as-vec s)
                       false))))

(defn legal-word? [s]
  "Check if a string is a complete word in words.txt"
  (and (> (count s) 1)
       (not (false? (get-in word-dict
                            (conj (as-vec s) :end) ;; look for end tag
                            false)))))


;;
;; next few funtions are for double-checking that things work
;; (these are still a bit buggy at times)
;;

(defn keys2 [m]
  "Exactly like keys but returns an empty set instead of nil when called on nil or an empty map"
  (let [s (keys m)]
    ;;(println "s: " s)
    (if (or (nil? s) (= :nil s) (= :end s))
      '()
      s)))

(defn vals2 [m]
  "Exactly like vals but returns an empty set instead of nil when called on nil or an empty map"
  (let [s (vals m)]
    ;;(println "vals2, s: " s)
    (if (or (nil? s) (= :nil s) (= '(:end) s))
      '()
      s)))

(defn count-keys [m]
  "Count the total number of keys for a given (nested) map"
  (cond (empty? m)
        0
        :else
        (+ (count (keys2 m))
           (reduce + (map count-keys (vals2 m))))))

(defn count-leaves [m]
  (cond (contains? m :end)
        1
        :else
        (reduce + (map count-leaves (vals2 m)))))

;; end double-checking functions



(defn split-map [m]
  "(split-map {:a :b :c :d :e :f}) --> ({:a :b} {:c :d} {:e :f})"
  (map (fn [[k v]] {k v}) m))


(defn paths [m]
  "A function to take a GADDAD/setrie structure and return all of the words it contains. *This is broken and the victim of some 1 AM debugging.*  "
  ;; To show breakage:
  ;; (paths (make-dictionary ["hello" "there" "tree"]))
  (cond (= m {:end :end})
        ""
        :else
        (let [branches (split-map m)]
          (defn on-branch [b]
            (if (= b {:end :end})
              ""
              (let [sub-branches (split-map (first (vals b)))]
                (defn on-sub-branch [sb]
                  (concat (first (keys b))
                          (first (paths sb))))
                (doall (map on-sub-branch sub-branches)))))
          (doall (map on-branch branches)))))





;; the word-dict Setrie holds 993,046 keys, including the 235,886
;; end tags (one for each word) leaving 757,160 letters
;;
;; => (reduce + (map count word-list))
;; gives 2,257,223 for the number of letters in the words as a list
;; (not including newline characters)
;;
;; to check the legality of every word in the word list:
;; (time (count (filter true? (map legal-string? word-list))))
;; "Elapsed time: 2651.056884 msecs"
;; 235886
;;
;; for reference, to look up list membership using 'some'
;; takes much longer (the next function is a syntactic mess,
;;                    #() is a lambda function, % is its argument
;;                    #{} is a set):
;; => (time (count (map #(some #{%} word-list) (take 10000 word-list))))
;; "Elapsed time: 14520.783195 msecs"
;; ** 14+ seconds to look up 10,000 words in the word-list,
;;    whereas the word-dict/Setrie method does 235,000 in 2.6
;;    seconds, and the word-dict automatically handles substrings**


(defn assoc-deep  [m ks e]
  "Updates nested maps non-destructively, creates keys if there
   are none.  Last key is set to the default value {e nil}"
  ;; m: map, ks: list of keys, d: default end tag
  (let [k (first ks)]
      (cond (empty? ks) ;; if no more keys
            {e nil} ;; end tag
            (contains? m (first ks)) ;; if the key exists
            (assoc m k (merge (m k)  ;; combine new and old
                              (assoc-deep (m k) (rest ks) e))) 
            :else
            (assoc m k (assoc-deep {} (rest ks) e)))))



(defn deep-merge [maps]
  "Non-destructively combine nested maps.

(deep-merge [{:a {:b {:c {:end :end}
                      :q {:r {:end :end}}}}}
             {:a {:b {:c {:d {:e {:f {:g {:end :end}}}}}}}}
             {:a {:x {:y {:z {:end :end}}}}}])
result:
{:a {:x {:y {:z {:end :end}}},
     :b {:c {:d {:e {:f {:g {:end :end}}}},
             :end :end},
         :q {:r {:end :end}}}}}"
  (cond (map? maps)
        maps  ;; if only one map (not a list of maps), return it
        (= (count maps) 1)
        (first maps)  ;; does this ever happen?
        (= maps [{:end :end} {:end :end}])
        ;; a special condition arises in words with multiples of the
        ;; same letter, like "hello": they generate two equal nested
        ;; map structures which breaks this function.
        ;; this solves the problem but may not be the best way
        {:end :end}
        :else
        (apply merge-with (fn [m1 m2] (deep-merge [m1 m2])) maps)))


(defn parts [s]
  "Generate all partitions of a string.  Uses clojure.math.combinatorics/partitions"
  ;;
  ;; It's generating about 4x too many partitions which must be
  ;; filtered down with 'every?'.
  ;; To go faster the algorithm will need to be reworked:
  ;; https://github.com/clojure/math.combinatorics/
  (filter (fn [y] (every? (fn [x] (> (count x) 1)) y))
          (combo/partitions s
                            :min 2 ;; minumum of two partitions
                            ;; if an 8 character string is in 5 pieces
                            ;; 1 piece (at least) has only 1 character
                            ;; so we limit it to (/ 8 2) = 4 pieces
                            :max (/ (count s) 2))))


(defn legal-anagram? [s]
  (every? true? (map legal-word? s)))


(defn get-one-anagram [s]
  "Given a string, get the first legal anagram.  If there are no legal anagrams it will search the entire space and return false."
  (loop [p (parts s)]
    (cond (empty? p) ;; the lazy-seq is empty, no legal anagram
          false
          (legal-anagram? (first p))
          (map #(apply str %) (first p))
          :else
          (recur (rest p)))))

(defn get-all-anagrams [s]
  (->> s
      parts
      (#(filter legal-anagram? %))
      (map (fn [x] (map (fn [y] (apply str y))
                        x)))))
