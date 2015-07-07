(ns anagram.core
  (:require [clojure.string :as string]
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
;;
;; Thanks to a data-structure of nested hash-maps representing
;; the word list (a trie) performing lookups is extremely fast.
;; The general method for iterating through the permutations is
;; to generate a trie of successful anagrams while checking nodes
;; during the build (and not building their branches if the node
;; isn't legal.)  I'm also keeping a list of visited nodes to check
;; new nodes against: "the dog spot" and "spot the dog" are equivalent
;; to each other.


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
;; -> is the "thread first" macro
;; output of the previous function is inserted into the first argument location
;; of the current function
;;
;; ->> is the "thread last" macro
;; output of the previous function is inserted into the last argument location
;; of the current function
;;
;; (fn [x] (+ 5 x)), #(+ 5 %): two equivalent lambda function syntaxes
;; (defn name [args] body): function syntax
;;
;; "first" is car
;; "rest" is cdr
;; cons is rarely used, conj is more common but operates differently
;; on different data types (appends to the head of a list, tail of
;; a vector)
;;
;; Tail-call optimization requires loop/recur in order to work.
;; Laziness is common and can cause strange problems in recursive algorithms.


;; wordlist (relative) locations.  relative to resources folder.
(def loc1
  "google-10000-english-usa.txt")
(def loc2
  "words.txt")


(def word-list
  "Load the word list, split it, make everything lowercase."
  (-> "google-10000-english-usa.txt"
      ;;"words.txt"
      java-io/resource
      java-io/file
      slurp
      string/split-lines
      (#(map string/lower-case %))))


(defn as-vec [word]
  ;; "abcd" ---> ["a" "b" "c" "d"]
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
  ;; add word to the Trie (nested hash-map) structure
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


;; I duct-taped these mutable variable at the end 
(def ^:dynamic *excluded-words*
  "When building anagrams of long words sometimes it becomes clear that some uninteresting words keep appearing, this prevents those words and their branches from being built.  This variable can be re-bound to a set of words to exclude."
  #{})

(def ^:dynamic *min-length*
  "Rebinding this will change the minimum length of words in the anagram.  Raising this number helps longer words compute in a reasonable time."
  2)

(defn legal-word? [s]
  "Check if a string is a complete word in the dictionary."
  (and (> (count s) *min-length*)
       (nil? (*excluded-words* s))
       (not (false? (get-in word-dict
                            (conj (as-vec s) :end) ;; look for end tag
                            false)))))


;;
;; next few funtions are for double-checking that things work
;; (these are still a bit buggy)
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
  "(split-map {:a :b :c :d :e :f}) ---> ({:a :b} {:c :d} {:e :f})"
  (map (fn [[k v]] {k v}) m))


(defn paths [m]
  "A function to take a Trie structure and return all of the words it contains."
  (->> (cond (= m {:end :end})
            ""
            :else
            (let [branches (split-map m)]
              ;(println "branches: " branches)
              (defn on-branch [b]
                (if (= b {:end :end})
                  ""
                  (let [sub-branches (split-map (first (vals b)))]
                    (defn on-sub-branch [sb]
                      (concat (first (keys b))
                              (first (paths sb))))
                    (map on-sub-branch sub-branches))))
              (mapcat on-branch branches)))
      (map (fn [x] (apply str x)))))


(defn paths [m]
  (println "hello, m: " m)
  (cond (= {:end :end} m)
        ""
        :else
        (let [branches (split-map m)]
          (println "branches: " branches)
          (map (fn [x] (concat (first (keys x))
                               (paths (branches (first (keys x))))))
               branches))))




;; the word-dict Trie holds 993,046 keys, including the 235,886
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
;;    whereas the word-dict/Trie method does 235,000 in 2.6
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


(defn not-nth [s n]
  "Return the string except for its nth item"
  (concat (take n s) (nthrest s (inc n))))


(defn legal-s? [s]
  "Takes a string, splits on the spaces, checks that the last
   section is a legal substring, and that the previous 
   sections are legal full words.  
   
   Returns false if any
   of the above fail, or if the last two characters are 
   spaces.
   Returns true if the string is (so far) legal"
  ;; (legal-s? "the green aard ") returns false
  ;; (legal-s? "the green aard") returns true
  (cond (= '(" " " ")
           (map str (take-last 2 s)))  ;; false if last two are spaces
        false
        (= " " (str (last s)))  ;; if it ends with a space
        (let [complete-words (string/split s #"\s+")]
          (every? true? (map legal-word? complete-words)))
        :else  ;; else, last word still in progress
        (let [spl (string/split s #"\s+")
              complete-words (butlast spl)
              partial-word (last spl)]
          (and (every? true? (map legal-word? complete-words))
               (true? (legal-string? partial-word))))))

(defn legal-anagram_s? [s]
  ;;(println "in legal anagram: realized? s: " (realized? s) "s: " s)
  (every? true? (map legal-word? (string/split s #"\s+"))))


(defn legal-last? [s]
  "Check the last word of the anagram only."
  (legal-word? (last (string/split s #"\s+"))))

(defn legal-current? [s]
  "Check the in-progress word being built."
  (legal-string? (last (string/split s #"\s+"))))



(defn dbg [x]
  "Helps debug threading macros, prints current value, returns current value."
  (println "\ndbg: " x "\n")
  x)


;; mutable structures -------------------------------------------

;; Because there's so much duplication, especially with larger
;; anagrams, keep track of successes and failures.
;;
;; No node needs to be explored more than once and there are multiple
;; paths to equivalent nodes: "car the" and "the car" are the same.
;;
;; Each node has a "signature" which is its words along with the number of
;; times each word appears.  These signatures are stored in a list.  This hasn't
;; been optimized, hashing may work well here.


(def visited-nodes
  "A set in an atom (a mutable set).  Contains the signatures of every visited node.  Each node in this case is the end of a word."
  (atom #{}))

(def visited-subnodes
  "A set in an atom (a mutable set).  Contains the signatures of every visited subnode.  The subnodes are the nodes that occur within individual words."
  (atom #{}))

(defn reset-visited! []
  "Reset the visited nodes/subnodes.  Must be called every time anagrams are searched."
  (reset! visited-nodes #{})
  (reset! visited-subnodes #{}))


(defn node-visited! [n]
  "Create node signature and save it to visited-nodes."
  ;;(println "node-visited! " n)
  (->> n
       (#(string/split % #"\s"))
       frequencies
       (#(into (sorted-map) %))
       ;str ;; a string is all that's needed, should make set ordering easier
       (swap! visited-nodes conj)))


(defn node-visited? [n]
  "Check if node signature has been saved to visited-nodes.  Returns boolean"
  (-> n
      (#(string/split % #"\s"))
      frequencies
      (#(into (sorted-map) %))
      ;str
      (#(@visited-nodes %))
      nil?
      not))

(defn subnode-visited! [n]
  "Add new signature to the visited-subnodes atom."
  (swap! visited-subnodes conj n))

(defn subnode-visited? [n]
  "Lookup a signature in the visited-subnodes atom.  Returns boolean."
  (if (> (count n) 1)
    (not (nil? (@visited-subnodes n)))
    false))

;; end mutable structures ---------------------------------------






(defn p [s l ls e]
  ;; (println "p.  ls: " ls)
  (cond (empty? s) ;; empty string? then l is the last letter
        (if (legal-anagram_s? (string/join [ls l])) ;; final check
          {l {e :end}}  ;; legal anagram, return end-tag
          false)        ;; not legal anagram
        (= l " ")  ;; this is an end-of-word node
        (if (node-visited? ls)
          false  ;; if the node's been visited just bug-out
          (do (node-visited! ls)  ;; otherwise, add to visited-nodes and continue
              (if (legal-last? ls)
                (let [with-letter (fn [x] (p (not-nth s x)   ;; string except for nth letter
                                             (str (nth s x)) ;; nth letter becomes new l
                                             (string/join [ls l])  ;; current l is appended to known legals
                                             e)) ;; end tag
                      results (map with-letter (range (count s)))] ;; list, each member is false or a map
                  (if (every? false? results)
                    false ;; there were no successful leaves found for this node
                    {l (-> results  ;; threading macro, returning a map
                           ((fn [x] (filter identity x)))
                           ((fn [x] (deep-merge x))))}))
                false))) ;; last word not legal
        (subnode-visited? (string/join [ls l]))
        false
        ;; (do ;;(println "subnode tossed out: " (string/join [ls l]))
        ;;     false)
        :else  ;; must be a word in progress
        (let [with-space (if (or (= (last ls) " ") (< (count s) 2))
                           false ;; needs at least two letters for a space to make sense
                           (p s    ;; same string
                              " "  ;; new letter is a space
                              (string/join [ls l]) ;; add current letter to old letters
                              e))  ;; end tag
              with-letter (fn [x] (if (legal-current? (string/join [ls l]))  ;; check if the new string is legal before recursing
                                    (p (not-nth s x)   ;; string except nth letter
                                       (str (nth s x)) ;; nth letter becomes new l
                                       (string/join [ls l])  ;; current l is appended to known legals
                                       e)
                                    false)) ;; end tag
              letter-results (map with-letter (range (count s)))
              results (conj letter-results with-space)] ;; list, each member is false or a map
          (subnode-visited! (string/join [ls l]))
          (if (every? false? results)
            false ;; there were no successful leaves found for this node
            {l (-> results  ;; threading macro, returning a map
                   ((fn [x] (filter identity x)))
                   ((fn [x] (deep-merge x))))}))))  ;; combine all successful branches into one map and return it.     


;; the following are mutually recursive with p2 so p2 needs a forward
;; declaration (it's not in the namespace yet)
(declare p2)

;; for the following functions
;; s: string remaining, l: current letter (or space), ls: past letters
;; if "carport" is the given word, the first calle to p2 will be with s="carport", l="", ls=""
;; then s="carort", l="p", ls=""
;; then s="caror", l="t", ls="p"
;; then s="caro", l="r", ls="pt"
;; etc.

(defn _anagram-complete [s l ls]
  "Called by p2 when there are no letters left.  If the node has already been visited return false.  Otherwise add to the visited nodes list and return true or false based on the anagram legality."
  (cond (node-visited? (string/join [ls l]))
        false
        (legal-anagram_s? (string/join [ls l])) ;; final check
        (do (node-visited! (string/join [ls l]))
            {l {:end :end}})  ;; legal anagram, return end-tag
        :else
        (do (node-visited! (string/join [ls l]))
            false)))

(defn _end_of_word [s l ls]
  "Called by p2 when s is a space.  If node has already been visited return false.  Otherwise add node to visited nodes list, and if node is legal call p2 with all possible next letter combinations."
  (if (node-visited? ls)
    false  ;; if the node's been visited just bug-out
    (do (node-visited! ls)  ;; otherwise, add to visited-nodes and continue
        (if (legal-last? ls)
          (let [with-letter (fn [x] (p2 (not-nth s x)   ;; string except for nth letter
                                       (str (nth s x)) ;; nth letter becomes new l
                                       (string/join [ls l])))  ;; current l is appended to known legals
                                       
                results (map with-letter (range (count s)))] ;; list, each member is false or a map
            (if (every? false? results)
              false ;; there were no successful leaves found for this node
              {l (-> results  ;; threading macro, returning a map
                     ((fn [x] (filter identity x)))
                     ((fn [x] (deep-merge x))))}))
          false)))) ;; last word not legal)

(defn _redundant-subnode [s l ls]
  "Stub in case there's something to be done with redundant subnodes."
  false)

(defn _subnode [s l ls]
  "Called by p2 when s is a new letter.  subnode-visited was called by p2 so this is a new subnode.  Call p2 with every combination of new letter as well as with a new space.  If all calls to p2 return false, also return false.  If legal branches come back, merge their maps into one and return that."
  (let [with-space (if (or (= (last ls) " ") (< (count s) 2))
                     false ;; needs at least two letters for a space to make sense
                     (p2 s    ;; same string
                         " "  ;; new letter is a space
                         (string/join [ls l]))) ;; add current letter to old letters
                        
        with-letter (fn [x] (if (legal-current? (string/join [ls l]))  ;; check if the new string is legal before recursing
                              (p2 (not-nth s x)   ;; string except nth letter
                                 (str (nth s x)) ;; nth letter becomes new l
                                 (string/join [ls l]))  ;; current l is appended to known legals
                              false))
        letter-results (map with-letter (range (count s)))
        results (conj letter-results with-space)] ;; list, each member is false or a map
    (subnode-visited! (string/join [ls l]))
    (if (every? false? results)
      false ;; there were no successful leaves found for this node
      {l (-> results  ;; threading macro, returning a map
             ((fn [x] (filter identity x)))
             ((fn [x] (deep-merge x))))})))


(defn p2 [s l ls]
  "This is the main anagram algorithm.  

s: string remaining, l: current letter (or space), ls: past letters

This is mutually-recursive with the three functions above.  If the remaining string is empty then l is the last letter.  If l is a space then this is a node at the end of a word.  If the subnode has been visited simply return false.  Otherwise the current letter represents a subnode that needs to be checked for legality and if it's legal _subnode will call this function again with new parameters."
  (cond (empty? s) ;; empty string? then l is the last letter
        (_anagram-complete s l ls)
        (= l " ")
        (_end_of_word s l ls)
        (subnode-visited? (string/join [ls l])) ;; caused by repeated letters
        ;;(_redundant-subnode s l ls)
        false
        :else
        (_subnode s l ls)))
        


(defn keys-in [m]
  ;; from http://stackoverflow.com/questions/21768802/how-can-i-get-the-nested-keys-of-a-map-in-clojure
  (if (map? m)
    (vec 
     (mapcat (fn [[k v]]
               (let [sub (keys-in v)
                     nested (map #(into [k] %) (filter (comp not empty?) sub))]
                 (if (seq nested)
                   nested
                   [[k]])))
             m))
    []))


(defn paths [m]
  "Print all paths through a Trie structure"
  (->> m
      keys-in
      (map butlast)
      (map #(apply str %))))


(defn get-anagrams [word & excluded-words]
  "Given a word and optional excluded words, finds all anagrams of the word that don't contain the excluded words."
  (reset-visited!)
  (binding [*excluded-words* (set (vec excluded-words))
            *min-length* 3]
    (println "excluded: " *excluded-words*)
    (paths (p2 word "" ""))))

