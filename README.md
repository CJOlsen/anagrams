# anagram

A small program to explore anagrams.

## Usage

* Download Leiningen
* "lein deps" from the root of the project directory will grab the project dependencies (including Clojure)
* "lein repl" will open a repl in the shell.
* in the repl, run "(use '[anagram.core])" to load the main namespace
* (get-one-anagram "carcar") should return ("carr" "ca")
* (get-all-anagrams "horsecart") should return (("horst" "re" "ca") ("he" "orc" "ra" "st") ("ha" "orc" "re" "st"))
* instead of Leiningen's repl, LightTable may work and emacs will work but takes some time.


## Copyright

Copyright © 2015 Christopher Olsen
