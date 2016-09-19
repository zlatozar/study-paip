;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TUTOR; Base: 10 -*-

;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991, 1996 Peter Norvig

;; NOTE: this file will be spread through chapters

(in-package #:tutor)

(defexamples 15 "Symbolic Mathematics with Canonical Forms"
  "This chapter uses a canonical representation for polynomials"
  "to achieve a more efficient program than the rules-based one in Chapter 8."
  (:section "15.1 A Canonical Form for Polynomials")
  ((requires "cmacsyma"))
  "We represent polynomials as vectors, with the variable in element 0,"
  "and the coefficients starting in element 1 and going up from there."
  "Here is the representation of 5x^3 + 10x^2 + 20x + 30"
  ('#(x 30 20 10 5) @ 511)
  "Here are some examples (without the interactive loop):"
  ((canon '(3 + x + 4 - x)) => 7 @ 521)
  ((canon '(x + y + y + x)) => ((2 * x) + (2 * y)))
  ((canon '(3 * x + 4 * x)) => (7 * x))
  ((canon '(3 * x + y + x + 4 * x)) => ((8 * x) + y))
  ((canon '((x + 1) ^ 10)) =>
   ((x ^ 10) + (10 * (x ^ 9)) + (45 * (x ^ 8)) + (120 * (x ^ 7))
    + (210 * (x ^ 6)) + (252 * (x ^ 5)) + (210 * (x ^ 4))
    + (120 * (x ^ 3)) + (45 * (x ^ 2)) + (10 * x) + 1))
  ((canon '((x + 1) ^ 10 - (x - 1) ^ 10)) =>
   ((20 * (x ^ 8)) + (240 * (x ^ 7)) + (504 * (x ^ 5))
    + (240 * (x ^ 3)) + (20 * x)))
  ((canon '(d (3 * x ^ 2 + 2 * x + 1) / d x)) @ 522 =>
   ((6 * x) + 2))
  ((canon '(d (z + 3 * x + 3 * z * x ^ 2 + z ^ 2 * x ^ 3) / d z)) =>
   (((2 * z) * (x ^ 3)) + (3 * (x ^ 2)) + 1)))


(defexamples 16 "Expert Systems"
  "In this chapter we develop an expert system shell, and give it a few rules"
  "about infectious disease, thus duplicating some of the Mycin system."
  ((requires "mycin-r"))
  "Because this is an interactive system, we can't show the interaction here."
  "You can try it yourself by evaluating (mycin)"
  )

(defexamples 17 "Line Diagram Labelling by Constraint Satisfaction"
  "In this chapter we look at the line-diagram labeling problem: Given a list"
  "of lines and the vertexes at which they intersect, how can we determine"
  "what the lines represent?"
  ((requires "waltz"))
  (:section "17.2 Combining Constraints and Searching")
  "First let's test that we can find the possible labelings for a vertex class:"
  ((possible-labelings 'Y) @ 574 =>
   ((+ + +) (- - -) (L R -) (- L R) (R - L)))
  "Notice how matrix-transpose works:"
  ((matrix-transpose (possible-labelings 'Y)) =>
   ((+ - L - R)
    (+ - R L -)
    (+ - - R L)))
  ((defdiagram cube
       (a Y b c d)
     (b W g e a)
     (c W e f a)
     (d W f g a)
     (e L c b)
     (f L d c)
     (g L b d)) @ 575)
  (:section "17.3 Labelling Diagrams")
  "We are now ready to try labelling diagrams.  First the cube:"
  ((print-labelings (diagram 'cube)) @ 577)
  "The cube should have given four solutions."
  "We can get down to one solution by grounding line GD:"
  ((print-labelings (ground (diagram 'cube) 'g 'd)) @ 580)
  "For the more complex cube on a plate, we get similar results;"
  "Four interpretations, which turn to one after grounding line KM:"
  ((defdiagram cube-on-plate
       (a Y b c d)
     (b W g e a)
     (c W e f a)
     (d W f g a)
     (e L c b)
     (f Y d c i)
     (g Y b d h)
     (h W l g j)
     (i W f m j)
     (j Y h i k)
     (k W m l j)
     (l L h k)
     (m L k i)) @ 581)
  ((print-labelings (ground (diagram 'cube-on-plate) 'k 'm)) @ 582)
  "It is interesting to try the algorithm on an 'impossible' diagram."
  "It turns out the algorithm correctly finds no interpretation for this"
  "well-known illusion:"
  ((defdiagram poiuyt
       (a L b g)
     (b L j a)
     (c L d l)
     (d L h c)
     (e L f i)
     (f L k e)
     (g L a l)
     (h L l d)
     (i L e k)
     (j L k b)
     (k W j i f)
     (l W h g c)) @ 583)
  ((print-labelings (diagram 'poiuyt)) @ 583)
  "Now we try a more complex diagram:"
  ((defdiagram tower
       (a Y b c d)    (n L q o)
       (b W g e a)    (o W y j n)
       (c W e f a)    (p L r i)
       (d W f g a)    (q W n s w)
       (e L c b)      (r W s p x)
       (f Y d c i)    (s L r q)
       (g Y b d h)    (t W w x z)
       (h W l g j)    (u W x y z)
       (i W f m p)    (v W y w z)
       (j Y h o k)    (w Y t v q)
       (k W m l j)    (x Y r u t)
       (l L h k)      (y Y v u o)
       (m L k i)      (z Y t u v)) @ 584)
  ((print-labelings (ground (diagram 'tower) 'l 'k)) @ 584))

(defexamples 18 "Search and the Game of Othello"
  "In this chapter we will develop a simplified Othello-playing program."
  "It will not be a champion, but is much better than beginning players."
  (:section "18.2 Representation Choices")
  ((requires "othello"))
  "First, we see that our choices for representing the board seem to work:"
  ((print-board (initial-board)) @ 604)
  "Now we can compare the weighted squares and count difference strategies"
  "by playing two games, alternating who goes first.  The NIL as third argument"
  "means don't print the board after each move."
  ((othello (maximizer #'weighted-squares)
            (maximizer #'count-difference) nil) @ 610)
  ((othello (maximizer #'count-difference)
            (maximizer #'weighted-squares) nil))

  (:section "18.4 Searching Ahead: Minimax")
  "We can test the minimax strategy, and see that searching ahead 3 ply is"
  "indeed better than looking at only 1 ply.  We can follow the whole game"
  ((othello (minimax-searcher 3 #'count-difference)
            (maximizer #'count-difference)) @ 614 => 53)

  (:section "18.5 Smarter Searching: Alpha-Beta Search")
  "The following should produce the same result, only faster:"
  ((othello (alpha-beta-searcher 3 #'count-difference)
            (maximizer #'count-difference) nil) => 53)

  (:section "18.8 Playing a Series of Games")
  "A single game is not enough to establish that one strategy is better than"
  "another.  The function RANDOM-OTHELLO-SERIES allows two strategies to"
  "compete in a series of games."
  ((requires "othello2"))
  ((random-othello-series
    (alpha-beta-searcher 2 #'weighted-squares)
    (alpha-beta-searcher 2 #'modified-weighted-squares)
    5) @ 628)
  "Here is a comparison of five strategies that search only 1 ply."
  "To save time, we run 2 pairs of games each, not 5 pairs."
  ((round-robin
    (list (maximizer #'count-difference)
          (maximizer #'mobility)
          (maximizer #'weighted-squares)
          (maximizer #'modified-weighted-squares)
          #'random-strategy)
    2 10
    '(count-difference mobility weighted modified-weighted random)) @ 629)
  "Now we compare alpha-beta searchers at 3 ply for 1 pair of games each."
  "In the book it was 4 ply for 5 pairs each, but that takes too long."
  ((round-robin
    (list (alpha-beta-searcher 3 #'count-difference)
          (alpha-beta-searcher 3 #'weighted-squares)
          (alpha-beta-searcher 3 #'modified-weighted-squares)
          #'random-strategy)
    1 10
    '(count-difference weighted modified-weighted random)))
  )

(defexamples 19 "Introduction to Natural Language"
  "This chapter is a brief introduction to natural language processing."
  (:section "19.1 Parsing with a Phrase-Structure Grammar")
  "We start with the grammar defined on page 39 for the GENERATE program."
  "I include 'noun' and 'verb' as nouns in the grammar *grammar3*"
  ((requires "syntax1"))
  (*grammar3* @ 657)
  ((use *grammar3*))
  ((parser '(the table)) => ((NP (ART THE) (NOUN TABLE))))
  ((parser '(the ball hit the table)) =>
   ((SENTENCE (NP (ART THE) (NOUN BALL))
              (VP (VERB HIT)
                  (NP (ART THE) (NOUN TABLE))))))
  ((parser '(the noun took the verb)) =>
   ((SENTENCE (NP (ART THE) (NOUN NOUN))
              (VP (VERB TOOK)
                  (NP (ART THE) (NOUN VERB))))))
  "The range of sentences we can parse is quite limited."
  "The following grammar includes a wider variety."
  (*grammar4* @ 661)
  ((use *grammar4*))
  ((parser '(The man hit the table with the ball)) =>
   ((S (NP (D THE) (N MAN))
       (VP (VP (V HIT) (NP (D THE) (N TABLE)))
           (PP (P WITH) (NP (D THE) (N BALL)))))
    (S (NP (D THE) (N MAN))
       (VP (V HIT)
           (NP (NP (D THE) (N TABLE))
               (PP (P WITH) (NP (D THE) (N BALL))))))))
  "Here we see a phrase that is ambiguous between a sentence and a noun phrase:"
  ((parser '(the orange saw)) @ 662 =>
   ((S (NP (D THE) (N ORANGE)) (VP (V SAW)))
    (NP (D THE) (A+ (A ORANGE)) (N SAW))))

  (:section "19.4 The Unknown-Word Problem")
  "As it stands, the parser cannot deal with unknown words."
  "One way of treating unknown words is to allow them to be any of the"
  "'open-class' categories--nouns, verbs, adjectives, and names."
  ((parser '(John liked Mary)) @ 664 =>
   ((S (NP (NAME JOHN))
       (VP (V LIKED) (NP (NAME MARY))))))
  ((parser '(Dana liked Dale)) @ 665 =>
   ((S (NP (NAME DANA))
       (VP (V LIKED) (NP (NAME DALE))))))
  "We see the parser works as well with words it knows (John and Mary)"
  "as with new words (Dana and Dale), which it can recognize as names"
  "because of their position in the sentence."
  ((parser '(the rab zaggled the woogly quax)) =>
   ((S (NP (D THE) (N RAB))
       (VP (V ZAGGLED) (NP (D THE) (A+ (A WOOGLY)) (N QUAX))))))
  ((parser '(the slithy toves gymbled)) =>
   ((S (NP (D THE) (N SLITHY)) (VP (V TOVES) (NP (NAME GYMBLED))))
    (S (NP (D THE) (A+ (A SLITHY)) (N TOVES)) (VP (V GYMBLED)))
    (NP (D THE) (A+ (A SLITHY) (A+ (A TOVES))) (N GYMBLED))))
  ((parser '(the slithy toves gymbled on the wabe)) =>
   ((S (NP (D THE) (N SLITHY))
       (VP (VP (V TOVES) (NP (NAME GYMBLED)))
           (PP (P ON) (NP (D THE) (N WABE)))))
    (S (NP (D THE) (N SLITHY))
       (VP (V TOVES) (NP (NP (NAME GYMBLED))
                         (PP (P ON) (NP (D THE) (N WABE))))))
    (S (NP (D THE) (A+ (A SLITHY)) (N TOVES))
       (VP (VP (V GYMBLED)) (PP (P ON) (NP (D THE) (N WABE)))))
    (NP (NP (D THE) (A+ (A SLITHY) (A+ (A TOVES))) (N GYMBLED))
        (PP (P ON) (NP (D THE) (N WABE))))))
  (:section "19.5 Parsing into a Semantic Representation")
  ((requires "syntax2"))
  "Syntactic parse trees of a sentence may be interesting, but by themselves"
  "they're not very useful.  We use sentences to communicate ideas, not to"
  "display grammatical structures."
  ""
  "Imagine a compact disc player for which you can punch buttons like"
  "'play 1 to 5 without 3'.  We will define such a language."
  "The meaning of a sentence in the language is the list of tracks played."
  (*grammar5* @ 667)
  ((use *grammar5*))
  ((meanings '(1 to 5 without 3)) @ 669 => ((1 2 4 5)))
  ((meanings '(1 to 4 and 7 to 9)) => ((1 2 3 4 7 8 9)))
  ((meanings '(1 to 6 without 3 and 4)) => ((1 2 4 5 6) (1 2 5 6)))
  "The example '1 to 6 without 3 and 4' is ambiguous."
  "The syntactic ambiguity leads to a semantic ambiguity."
  "We can define a new grammar that eliminates some ambiguities:"
  (*grammar6* @ 669)
  ((use *grammar6*))
  "With this new grammar, we can get single interpretations out of most inputs"
  ((meanings '(1 to 6 without 3 and 4)) => ((1 2 5 6)))
  ((meanings '(1 and 3 to 7 and 9 without 5 and 6)) => ((1 3 4 7 9)))
  ((meanings '(1 and 3 to 7 and 9 without 5 and 2)) => ((1 3 4 6 7 9 2)))
  ((meanings '(1 9 8 to 2 0 1)) => ((198 199 200 201)))
  ((meanings '(1 2 3)) => (123 (123)))

  (:section "19.6 Parsing with Preferences")
  ((requires "syntax3"))
  "We need some compromise between the permissive grammar, which generated"
  "all possible parses, and the restrictive grammar, which eliminates too"
  "many parses.  To get the 'best' interpretation we will need not only a"
  "new grammar, we will also need to modify the program to compare the"
  "relative worth of candidate interpretations."
  (*grammar7* @ 673)
  ((use *grammar7*))
  "We will need a way to show off the prefernce rankings:"
  ((all-parses '(1 to 6 without 3 and 4)) @ 675)
  ((all-parses '(1 and 3 to 7 and 9 without 5 and 6)))
  ((all-parses '(1 and 3 to 7 and 9 without 5 and 2)) @ 676)
  "In each case, the preference rules are able to assign higher scores to"
  "more reasonable interpretations.  What we really want is to pick the best."
  "Here we see some examples:"
  ((meaning '(1 to 5 without 3 and 4)) => (1 2 5))
  ((meaning '(1 to 5 without 3 and 6)) => (1 2 4 5 6))
  ((meaning '(1 to 5 without 3 and 6 shuffled)))
  ((meaning '([ 1 to 5 without [ 3 and 6 ] ] reversed)) => (5 4 2 1))
  ((meaning '(1 to 5 to 9)) => NIL)
  )


(defexamples 20 "Unification Grammars"
  "Prolog was invented as a formalism to describe the grammar of French."
  "It is still useful to view a grammar as a set of logic programming clauses."
  "This chapter describes how that can be done."
  ((requires "unifgram"))
  (:section "20.3 A Simple Grammar in DCG Format")
  "Here is the trivial grammar from page 688 in DCG format:"
  ((clear-db))
  ((rule (S (?pred ?subj)) -->
         (NP ?agr ?subj)
         (VP ?agr ?pred)) @ 692)
  ((rule (NP ?agr (?det ?n)) -->
         (Det ?agr ?det)
         (N ?agr ?n)))
  ((rule (NP 3sg (the male))          --> (:word he)) @ 693)
  ((rule (NP ~3sg (some objects))     --> (:word they)))
  ((rule (VP 3sg sleep)               --> (:word sleeps)))
  ((rule (VP ~3sg sleep)              --> (:word sleep)))
  ((rule (Det ?any the)               --> (:word the)))
  ((rule (N 3sg (young male human))   --> (:word boy)))
  ((rule (N 3sg (young female human)) --> (:word girl)))
  "We can parse some of the sentences from page 689 (but in DCG format)."
  "Parsing:"
  ((?- (S ?sem (He sleeps) ())) :input ".")
  "Generating:"
  ((?- (S (sleep (the male)) ?words  ())) :input ".")
  "Enumerating:"
  ((?- (S ?sem ?words ())) :input ";;;;")
  "If we want the interpretation of 'Terry kisses Jean' to be"
  "(kiss Terry Jean) not ((lambda (x) (kiss x Jean)) Terry), then we need"
  "a way to unify semantic components together.  Here's one way:"
  ((clear-db))
  ((rule (S ?pred) -->
         (NP ?agr ?subj)
         (VP ?agr ?subj ?pred)) @ 694)
  ((rule (VP ?agr ?subj ?pred) -->
         (Verb/tr ?agr ?subj ?pred ?obj)
         (NP ?any-agr ?obj)))
  ((rule (VP ?agr ?subj ?pred) -->
         (Verb/intr ?agr ?subj ?pred)))

  ((rule (Verb/tr ~3sg ?x (kiss ?x ?y) ?y) --> (:word kiss)))
  ((rule (Verb/tr 3sg ?x (kiss ?x ?y) ?y) --> (:word kisses)))
  ((rule (Verb/tr ?any  ?x (kiss ?x ?y) ?y) --> (:word kissed)))

  ((rule (Verb/intr ~3sg ?x (sleep ?x)) --> (:word sleep)))
  ((rule (Verb/intr 3sg ?x (sleep ?x)) --> (:word sleeps)))
  ((rule (Verb/intr ?any  ?x (sleep ?x)) --> (:word slept)))

  "Here are the rules for noun phrases and nouns"
  ((rule (NP ?agr ?sem) -->
         (Name ?agr ?sem)))
  ((rule (NP ?agr (?det-sem ?noun-sem)) -->
         (Det ?agr ?det-sem)
         (Noun ?agr ?noun-sem)))

  ((rule (Name 3sg Terry) --> (:word Terry)))
  ((rule (Name 3sg Jean)  --> (:word Jean)))

  ((rule (Noun 3sg (young male human))           --> (:word boy)) @ 695)
  ((rule (Noun 3sg (young female human))         --> (:word girl)))
  ((rule (Noun ~3sg (group (young male human)))   --> (:word boys)))
  ((rule (Noun ~3sg (group (young female human))) --> (:word girls)))

  ((rule (Det ?any the)  --> (:word the)))
  ((rule (Det 3sg a) --> (:word a)))

  "This grammar and lexicon generates more sentences, although it is still"
  "rather limited.  Here are some examples:"

  ((?- (S ?sem (The boys kiss a girl) ())) @ 695 :input ";.")
  ((?- (S ?sem (The girls kissed the girls) ())) :input ";.")
  ((?- (S ?sem (Terry kissed the girl) ())) :input ";.")
  ((?- (S ?sem (The girls kisses the boys) ())) :input ";.")
  ((?- (S ?sem (Terry kissed a girls) ())) :input ";.")
  ((?- (S ?sem (Terry sleeps Jean) ())) :input ";.")

  (:section "20.4 A DCG Grammar with Quantifiers")
  ((clear-db))
  ((rule (Det ?any ?x ?p ?q (the ?x (and ?p ?q)))    --> (:word the)) @ 697)
  ((rule (Det 3sg  ?x ?p ?q (exists ?x (and ?p ?q))) --> (:word a)))
  ((rule (Det 3sg  ?x ?p ?q (all    ?x (-> ?p ?q)))  --> (:word every)))

  ((rule (Noun 3sg ?x (picture ?x)) --> (:word picture)) @ 698)
  ((rule (Noun 3sg ?x (story ?x)) --> (:word story)))
  ((rule (Noun 3sg ?x (and (young ?x) (male ?x) (human ?x))) -->
         (:word boy)))

  ((rule (NP ?agr ?x ?pred ?pred) -->
         (Name ?agr ?name)))

  ((rule (NP ?agr ?x ?pred ?np) -->
         (Det ?agr ?x ?noun&rel ?pred ?np)
         (Noun ?agr ?x ?noun)
         (rel-clause ?agr ?x ?noun ?noun&rel)))

  ((rule (rel-clause ?agr ?x ?np ?np) --> ))
  ((rule (rel-clause ?agr ?x ?np (and ?np ?rel)) -->
         (:word that)
         (VP ?agr ?x ?rel)))

  ((rule (Verb/tr ~3sg ?x ?y (paint ?x ?y)) --> (:word paint)) @ 699)
  ((rule (Verb/tr 3sg  ?x ?y (paint ?x ?y)) --> (:word paints)))
  ((rule (Verb/tr ?any ?x ?y (paint ?x ?y)) --> (:word painted)))

  ((rule (Verb/intr ~3sg ?x (sleep ?x)) --> (:word sleep)))
  ((rule (Verb/intr 3sg  ?x (sleep ?x)) --> (:word sleeps)))
  ((rule (Verb/intr ?any ?x (sleep ?x)) --> (:word slept)))

  ((rule (Verb/intr 3sg  ?x (sells ?x)) --> (:word sells)))
  ((rule (Verb/intr 3sg  ?x (stinks ?x)) --> (:word stinks)))

  ((rule (VP ?agr ?x ?vp) -->
         (Verb/tr ?agr ?x ?obj ?verb)
         (NP ?any-agr ?obj ?verb ?vp)))

  ((rule (VP ?agr ?x ?vp) -->
         (Verb/intr ?agr ?x ?vp)))

  ((rule (S ?np) -->
         (NP ?agr ?x ?vp ?np)
         (VP ?agr ?x ?vp)))

  "Now we define a function to show the output from a query."
  "In the book, you just saw the output of such a function."
  ((defun do-s (words)
     (top-level-prove `((S ?sem ,words ())))))

  ((do-s '(Every picture paints a story)) :input "." @ 699)
  ((do-s '(Every boy that paints a picture sleeps)) :input ".")
  ((do-s '(Every boy that sleeps paints a picture)) :input ".")
  ((do-s '(Every boy that paints a picture that sells paints a picture
           that stinks)) :input "." @ 700)

  (:section "20.5 Preserving Quantifier Scope Ambiguity")
  ((clear-db))
  ((rule (S (and ?np ?vp)) -->
         (NP ?agr ?x ?np)
         (VP ?agr ?x ?vp)) @ 701)

  ((rule (VP ?agr ?x (and ?verb ?obj)) -->
         (Verb/tr ?agr ?x ?o ?verb)
         (NP ?any-agr ?o ?obj)))

  ((rule (VP ?agr ?x ?verb) -->
         (Verb/intr ?agr ?x ?verb)))

  ((rule (NP ?agr ?name t) -->
         (Name ?agr ?name)))

  ((rule (NP ?agr ?x ?det) -->
         (Det ?agr ?x (and ?noun ?rel) ?det)
         (Noun ?agr ?x ?noun)
         (rel-clause ?agr ?x ?rel)))

  ((rule (rel-clause ?agr ?x t) --> ))
  ((rule (rel-clause ?agr ?x ?rel) -->
         (:word that)
         (VP ?agr ?x ?rel)))

  ((rule (Name 3sg Terry)                     --> (:word Terry)))
  ((rule (Name 3sg Jean)                      --> (:word Jean)))
  ((rule (Det 3sg  ?x ?restr (all ?x ?restr)) --> (:word every)))
  ((rule (Noun 3sg ?x (man ?x))               --> (:word man)))
  ((rule (Verb/tr 3sg ?x ?y (love ?x ?y))     --> (:word loves)))
  ((rule (Verb/intr 3sg ?x (lives ?x))        --> (:word lives)))
  ((rule (Det 3sg  ?x ?res (exists ?x ?res))  --> (:word a)))
  ((rule (Noun 3sg ?x (woman ?x))             --> (:word woman)))

  "Here is an example of the new representation:"
  ((do-s '(every man loves a woman)) :input "." @ 701)
  )

(defexamples 21 "A Grammar of English"
  ((if (boundp 'clear-db) (clear-db)) @ 715)
  ((requires "grammar" "lexicon"))
  ((prolog-compile-symbols))
  (:section "21.10 Word Categories")
  ((?- (word sees verb ?infl ?senses)) :input ".")
  ((try S John promised Kim to persuade Lee to sleep) :input ";;;.")
  (:section "21.14 Examples")
  ((try S When did John promise Kim to persuade Lee to sleep)
   @ 746 :input ";;;.")
  ((try S Kim would not have been looking for Lee) @ 747 :input ";;;.")
  ((try s It should not surprise you that Kim does not like Lee) :input ";;;.")
  )

(defexamples 22 "Scheme: An Uncommon Lisp"
  "This chapter presents the Scheme dialect of Lisp and an interpreter for it."
  "Understanding the interpreter can give you a better appreciation of Lisp."
  (:section "22.1 A Scheme Interpreter")
  ((requires "interp1"))
  "We're ready to try out the interpreter.  Note we provide an argument"
  "to avoid going into a read-eval-print loop with SCHEME.  This is a new"
  "functionality, no in the book, added to make these examples easier."
  ((scheme '(+ 2 2)) @ 760 => 4 )
  ((scheme '((if (= 1 2) * +) 3 4)) => 7)
  ((scheme '((if (= 1 1) * +) 3 4)) => 12 @ 761)
  ((scheme '(set! fact (lambda (n) (if (= n 0) 1
                                       (* n (fact (- n 1))))))))
  ((scheme '(fact 5)) => 120)
  ((scheme '(set! table (lambda (f start end)
                          (if (<= start end)
                              (begin
                               (write (list start (f start)))
                               (newline)
                               (table f (+ start 1) end)))))))

  ((scheme '(table fact 1 10)) => NIL )
  ((scheme '(table (lambda (x) (* x x x)) 5 10)) => NIL)

  (:section "22.2 Syntactic Extension with Macros")
  "Scheme has a number of special forms that were not listed above."
  "These can be implemented by macros (although macros are not officially"
  "part of Scheme).  We can test out the macro facility:"
  ((scheme-macro-expand '(and p q)) => (IF P (AND Q)) @ 765)
  ((scheme-macro-expand '(and q)) => Q)
  ((scheme-macro-expand '(let ((x 1) (y 2)) (+ x y))) =>
   ((LAMBDA (X Y) (+ X Y)) 1 2))
  ((scheme-macro-expand
    '(letrec
      ((even? (lambda (x) (or (= x 0) (odd? (- x 1)))))
       (odd?  (lambda (x) (even? (- x 1)))))
      (even? z))))
  "Now let's look at uses of the macros DEFINE and LET*"
  ((scheme '(define (reverse l)
             (if (null? l) nil
                 (append (reverse (cdr l)) (list (car l)))))) => REVERSE)
  ((scheme '(reverse '(a b c d))) => (D C B A))
  ((scheme '(let* ((x 5) (y (+ x x)))
             (if (or (= x 0) (and (< 0 y) (< y 20)))
                 (list x y)
                 (+ y x)))) => (5 10))


  (:section "22.4 Throw, Catch, and Call/cc")
  ((requires "interp3"))
  "Non-local flow of control is provided in Scheme with a very general and"
  "powerful procedure, CALL-WITH-CURRENT-CONTINUATION, which is often"
  "abbreviated CALL/CC.  Here are some examples:"
  ((scheme '(+ 1 (call/cc (lambda (cc) (+ 20 300))))) @ 770 => 321)
  "The above example ignores CC and computes (+ 1 (+ 20 300))"
  "The next example does make use of CC:"
  ((scheme '(+ 1 (call/cc (lambda (cc) (+ 20 (cc 300)))))) => 301)
  "The above passes 300 to CC, thus bypassing the addition of 20."
  "It effectively throws 300 out to the catch point established by call/cc."
  )

(defexamples 23 "Compiling Lisp"
  "Compilers are simple to write and useful to know about."
  "In this chapter we develop a simple compiler for Scheme."
  ""
  ((requires "compile1"))
  "Now we are ready to show the simple compiler at work:"
  ((comp-show '(if (= x y) (f (g x)) (h x y (h 1 2)))) @ 791)
  "Here are some places where a compiler could do better than an interpreter"
  "(although our compiler currently does not):"
  ((comp-show '(begin "doc" (write x) y)) @ 792)
  "We should not have to push 'doc' on the stack just to pop it off."
  "Here's another example:"
  ((comp-show '(begin (+ (* a x) (f x)) x)))
  "Here's an example using local variables:"
  ((comp-show '((lambda (x) ((lambda (y z) (f x y z)) 3 x)) 4)) @ 794)
  (:section "23.1 A Properly Tail-Recursive Compiler")
  "Notice the two new instructions, CALLJ and SAVE"
  ((requires "compile2"))
  "First we see how nested function calls work:"
  ((comp-show '(f (g x))) @ 796)
  "In the next example we see that unneeded constants and variables in BEGIN"
  "expressions are ignored:"
  ((comp-show '(begin "doc" x (f x) y)) @ 797)
  ((comp-show '(begin (+ (* a x) (f x)) x)))
  "Here are some examples of IF expressions:"
  ((comp-show '(if p (+ x y) (* x y))) @ 801)
  "If we put the same code inside a BEGIN we get something quite different:"
  ((comp-show '(begin (if p (+ x y) (* x y)) z)) @ 802)
  "Here are some more examples of the compiler at work:"
  ((comp-show '(if (null? (car l)) (f (+ (* a x) b))
                (g (/ x 2)))) @ 806)
  ((comp-show '(define (last1 l)
                (if (null? (cdr l)) (car l)
                    (last1 (cdr l))))) @ 807)
  ((comp-show '(define (length l)
                (if (null? l) 0 (+ 1 (length (cdr l)))))) @ 808)
  "Of course, it is possible to write LENGTH in tail-recursive fashion:"
  ((comp-show '(define (length l)
                (letrec ((len (lambda (l n)
                                (if (null? l) n
                                    (len (rest l) (+ n 1))))))
                  (len l 0)))))
  (:section "23.4 A Peephole Optimizer")
  "In this section we investigate a simple technique that will generate"
  "slightly better code in cases where the compiler is less than perfect."
  ((requires "compile3"  "compopt"))
  ((comp-show '(begin (if (if t 1 (f x)) (set! x 2)) x)) @ 818)
  )
