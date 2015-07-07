# anagram

A small program to explore anagrams.

## Usage

* Install Leiningen (http://leiningen.org/#install)
* "lein deps" from the root of this project directory will grab the project dependencies, including Clojure.
* "lein repl" will open a repl in the shell.
* in the repl, run "(use '[anagram.core])" to load the main namespace

Getting anagrams:
* (get-anagrams "anyword")
* (get-anagrams "anyword" "optional" "excluded" "words")

Other notes:
* instead of Leiningen's repl, LightTable may work and emacs will work but takes time to set up.
* to change the length of acceptable words change the *min-length* binding in the get-anagrams function (a better interface is on the TODO list.)
* Tweaking the min-length and excluded words has significant effects on running time starting around 8-11 letters.


A string of 960 illegal characters ("1234567890" repeated) returns in under seven seconds.  A string of 10 common characters "aeiourstln" takes close to the same amount of time.

Run time depends heavily on the letters used: "hrdlcmwfgypbvkjxqz" returns in under 2 seconds. Replacing the "z" with an "e" to make "hrdlcmwfgypbvkjxqe" takes over 15 seconds. Replacing the "z" with an "e" and the "q" with an "a" while removing 4 letters to make "cmwfgypbvkjxae" takes over 4 seconds.

Using excluded words allows the algorithm to ignore entire branches before they're completed.  Repeated and rare letters help the algorithm to complete quickly.

## Copyright

Copyright © 2015 Christopher Olsen
