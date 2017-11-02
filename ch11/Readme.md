## Chapter 11

The idea behind _logic programming_ is that the programmer should state the _relationships_
that describe a problem and its solution. These relationships act as constraints on the
algorithms that can solve the problem, but the system itself, rather than the programmer,
is responsible for the details of the algorithm.

Logic(relational) programming language specifies relations among values. A logic program
can be viewed as a collection of relations with each clause specifying some condition
under which the relation holds. Logic programming is declarative: the programmer specifies
"what" is to be done, leaving the "how" largely up to the computer.

Prolog programming uses relations within a logical structure to represent knowledge.
Deductions, based on the represented knowledge, are used to form logical consequences.
Prolog programming allows knowledge to be viewed both procedurally and declaratively.
The procedural aspect of knowledge representation is found in the _order_ of execution
of the program statements. The declarative aspect is embodied in the logic statements
that describe the problem domain and how to solve problems in that domain. The logic
programmer is less concerned with how the machine will solve a particular problem and
more concerned with the accuracy of the defined relationships of the problem and how they
interact and hold under various circumstances. Ideally, the Prolog interpreter takes care
of the "how" aspect of problem solving, freeing the programr-r to focus on the relations
characterizing a problem.

### Prolog language

A Prolog program consists of a collection of _procedures_. Each procedure defines a
particular _predicate_, being a certain relationship between its arguments. A procedure
consists of one or more assertions, or _clauses_. It is convenient to think of two kinds of
clauses: _facts_ and _rules_.

_In Prolog, there's not often a finely detailed step-by-step recipe._ If you find the
relation you have to write few lines of code.

**Tip**: All Prolog code lives in a data base so Prolog program can be regarded as a
relational database that contains rules as well as facts. It is easy to add and remove
information from the database, and to pose sophisticated queries.

What is Prolog?

1. Declaring some ```facts``` about objects and their relationships;
2. Defining ```rules``` about objects and their relationships, and
3. Asking ```questions``` (queries) about objects and their relationships.


#### Facts

A ```fact``` is the most basic statement in Prolog. Facts are conclusions or
consequents that have no conditions or antecedents; facts thus form simple statements
about objects and their relationships. A fact announces that some relation is _true_. A
finite set of facts constitutes a simple logic program. Here is some examples:

```prolog

likes(joe,apples).  % "joe likes apples."
friend(bonny,joe).  %  "bonny is a friend of joe."
mother(sally,sue).  % "sally is the mother of sue."
```


#### Rules

A single program-clause can either be a ```fact``` or a ```rule```. Rules define complicated
relationships among objects by announcing that the _head_ of the rule is true if all of
the goals in the body of the rule are true. A rule has the form

```
|-- head --|    |-- body --|
 B1......Bn  :-  A1......An

```

```:-``` can be read as the word _"if"_ or _"is implied by"_

Here is an example:

```prolog

happy(X) -:    % Any X is happy if
   has(X,Y),   % X has Y and
   dog(Y).     % Y is a dog.
```

Prolog programs are composed of facts and rules. Facts are stored with rules. A collection
of facts and rules is commonly referenced as _database_. There is no need Prolog programming 
to differentiate between programming and data.

### Queries.

Queries form provide a means for retrieving information from a logic program.
A query has the special form:

```
? - G1,G2,...,Gn
```

A query initiates the execution of a program and is tyically typed at the terminal with
the interpreter running and the desired program loaded. Prolog solves a query by forming
a set of _variable bindings_ that makes each of the goals in the query consistent with the
facts ard rules in the program. This process can be illustrated with the following
example:

```
fact 1. likes(joe,apples).
fact 2. friend(bonny,joe).

rule 1. likes(X,Y) :-
           friend(X,Z),
           likes(Z,Y).
```


The interpreter now knows two facts that state that ```joe``` likes ```apples``` and ```bonny``` is a friend
of ```joe```. The single rule expresses a more complex relationship. _Any X likes any Y if X is_
_a friend of Z and Z likes Y_. The database, once loaded, can be interrogated with queries.

Consider the following query:

```
?- likes(joe,X).
```

The Prolog interpreter will start at the top of the database and search for a _fact_ or _rule_
whose head matches our query. The interpreter knows it is looking for facts or rules named
```likes``` with **two** arguments, the first of which must be ```joe``` or able to be instantiated to
```joe```. The second variable X has not yet been instantiated. The first fact encountered
that meets all the requircnents is _fact 1_ if X is instantiated to "apples". The interpreter
will respond to our query with:

```
X = apples
more (y/n)? y
no
```

The interpreter can not find any more facts c. rules that it can match with our query.
As a more complex example, consider the compound query:

```
?-likes(joe,X),likes(bonny,X).
```

This query asks the interpreter to find some inbtantiation for the variable X that both "'joe"
and "bonny" like. The following sequente of events will occur:

1. The first goal of the query succeeds because of fact number **1** with X instantiated to
   "apples".

2. "bonny" can not be matched to any likes fact so the second goal can only succeed
   through rule **1**. The rule is invoked with X instantiated to "bonny" and the variable
   Y instantiated to "apples".
   
3. The first goal of rule **1** succeeds with X instantiated to "bonny" and Z instantiated
   to "joe" via fact **2**.
   
4. The second goal of rule **1** then succeeds with Z instantiated to "joe" and Y to
   "apples".
   
5. Sine both goals of the query goal succeeded the query itself succeeds with X
   instantiated to "apples"
   

### Prolog syntax

Prolog uses only one data-type, the ```term```. Any term has one of three forms:

```
1. a constant
2. a variable, or
3  a structure.
```

#### constant

A ```constant``` is either an ```atom``` or a ```number```.  An atom can be represented
either by a sequence of alphanumeric characters beginning with a lower-cas, letter or by a
sequence of characters enclosed in single quotes.  A variable in Prolog stands for some
_definite_ but **unidentified** object. A variable is expressed by a sequence of
alphanumeric character or underscores, beginning Jith either an upper-case letter or an
underscore. Variables in Prolog do not designate storage-locations in memory as do
variables used in conventional languages. Instead, variables are used for pattern-matching
within a term. Example:

```
Constants       Variables
--------------------------
  a               A
  ab              Ab
  a_b             A_B
  'AB'            _AB5
  1              _1
```

#### compound term

A ```compound term``` is composed of a ```functor``` and a sequence of one or more arguments
and is commonly referred to as a structure. A structure has the form

```
foo(t1,....tn)
```

where foo, the functor (or predicate), is an atom and ```t1,...,tn```, the arguments, are terms.
The number of arguments is referred to as the arity of the term. The Prolog interpreter
distinguishes among the different forms of structures by the name of the functor and its
arity. A constant is considered to have an arity of zero.

```
Prolog Structure                                     Funtor        Arity
-------------------------------------------------------------------------
student(name(first(tom)))                           student         1

family(dad(joe),mom(bonny),children(jill,joshua))   family          3

```

#### list

An important data structure used in logic programming is the ```list```. A list
is a sequence of any number of terms such as ```6, bob, (fred(man),sally(woman)), four```.
Written in Prolog, this list would appear as ```[6,bob,[fred(man),sally(woman)],four]```.

A list is expressed with the following format: ```[Head|Tail]```. For example [1,2,3]

The head of a list is the first term (or element) in the list. The head of the list in
the above example is the element "1". The tail of a list is itself _a list_ that consists of
everything in the original list except the head. The tail of the above example is the list
[2,3]. Square brackets "[]" are used to denote an empty list.

Sometimes it is convenient to denote more than one element at the front of a list. To show
this, the representation is [X,Y,Z|Y] where X, Y, and Z are the first three elements of
the list and Y is the tail. The list [1,2,3], written in this form would be [1,2,3|[]].

**NOTE:** It is not mentioned in book but list in Lisp implementation is **cons** cells: ```(first . rest)```

- ```;``` is logical **OR**

- ```,``` is logical **AND**, separates goals

- ```=``` represents unification in Prolog

### Unification (or matching)

Unification is a process of matching terms.

A principle called _unification_ governs the process of instantiating variables with
terms. Unification is basically an attempt to _match_ two terms, by instantiating the
variables contained in the terms. _Thus, substitution plays a central role in the process._
A _substitution_ is a set of mappings from variables to terms.

**Tip:** Unification also serves as the parameter passing mechanism, and provides a constructor and
     selector of data structures.

If two variables unify with each other, then they _co-refer_: that is, the both refer to the
same term. The anonymous variable will unify with any term, and does not co-refer with any
term.

##### Execution model

Given a program consisting of clauses, the way to use the program is to pose _queries_
about it.

- When Prolog seeks to resolve a query or condition of a rule, it searches that data base of
  facts and rules from top to bottom for either a fact relation of the conclusion relation
  of a rule that will unify with the query of condition relation.

- A basic computational mechanism in Prolog is _relational resolution_. A query or condition
  relation and a conclusion relation _resolve_ if the have the same relation name, the same
  relation arity, and their terms in each respective component position unify.


#### Backtracking

_Backtracking is the process of going back to a previous goal and trying to resatisfy
it, i.e. to find another way of satisfying it._

_Backtracking is basically a form of searching._ In the context of Prolog, suppose that the
Prolog interpreter is trying to satisfy a sequence of goals **goal_1**, **goal_2**.
When the Prolog interpreter finds a set of variable bindings which allow **goal_1** to be
satisfied, it commits itself to those bindings, and then seeks to satisfy **goal_2**.

Eventually one of two things happens:
- **goal_2** is satisfied and finished with
- **goal_2** cannot be satisfied.

In either case, Prolog backtracks. That is, it _"un-commits"_ itself to the variable
bindings it made in satisfying **goal_1** and goes looking for a different set of variable
bindings that allow **goal_1** to be satisfied. If it finds a second set of such bindings,
it commits to them, and proceeds to try to satisfy **goal_2** again, with the new
bindings. In case (a), the Prolog interpreter is looking for extra solutions, while in
case (b) it is still looking for the first solution. So backtracking may serve to find
extra solutions to a problem, or to continue the search for a first solution, when a first
set of assumptions (i.e. variable bindings) turns out not to lead to a solution.

**Tip:** Automatic backtracking provides generate-and-test as the basic control flow
     model. This is more general than the strict unidirectional sequential flow of control in
     conventional languages.

#### Programming in Prolog

How do we know whether a grammatically correct program will do what it is supposed to? In
Prolog, the answer is perhaps clearer than in other programming languages. A Prolog
program is a knowledge base made up of _clauses_. Each of those clauses is an encoding of an
atomic or conditional sentence. To answer the question, we need to consider the truth of
these sentences. It is almost enough. There are two additional points to remember:

* _A program might not contain all possible truths about a matter._ So some truths might
  be missing, for a variety of good reasons. This must be accepted, and sometimes
  documented with comments.

* _But a program must include all relevant truths, explicitly or implicitly._ The reason is
  that this clause can be derived from other clauses using back-chaining; its truth is
  represented implicitly by the program. So to capture the whole truth, one needs to allow
  for the fact that some of it will be calculated by back-chaining over the knowledge
  base.

We can say it in other way:

If we want computers to act intelligent, we must help them. We must tell them all the
commonsense knowledge we have that they don't. This can be hard because this knowledge can
be so obvious to us that we don't realize that a computer doesn't know it too, but we must
try. There are two main kinds: _facts_ and _reasoning procedures_. Facts are things true
about the world, and reasoning procedures (or inferences) are ways to follow reasoning
chains between facts.

```
Writing a Prolog program involves building a knowledge base of clauses that
captures:

1. the truth, and nothing but,
2. the whole truth,
3. in a form suitable for back-chaining.
```

#### Recursion

* When writing recursive Prolog programs, there is no simple way to guarantee that they will
  terminate. However, a good rule of thumb is that when a clause is recursive, the
  _recursive predicate should appear toward the end of the clause, so that new
  variables can be instantiated._

* A recursive program will terminate if the query that matches the head of a clause is
  always bigger (according to the size measure being used) than the queries from the
  body of the clause.

#### Arithmetic

One restriction on the use of _arithmetic expressions_ is that the variables in them must
already have values. Prolog provides a built-in predicate for the purpose of evaluating
terms according to the rules of arithmetic. This predicate is called ```is```, and it can
be written as an infix operator.

```prolog
?- X is 2+2*2.
X=6

coeff(A,X,B,Y) :- Y is A*X+B.
?- coeff(2, 2, 2, 6).
yes
?- coeff(1+7, 2*2, 4, V).
Y=36
```

#### How Prolog finds solution?

```prolog
drinks(john, water).
drinks(jeremy, milk).
drinks(camilla, beer).
drinks(jeremy, X) :- drinks(john, X).
```
Find out what jeremy drinks.

![Prolog solution](how_prolog_finds_solutions.png "Solution")

### Implement Prolog in Common Lisp

* Prolog encourages the use of a single uniform data base.

In Prolog the assertions are called **clauses**, and they can be divided into two types:
_facts_, which state a relationship that holds between some objects e.g.
``` cl
(<- (likes Kim Robin))

;; We will use the macro <- to mark facts. Think of this as an assignment arrow which
;; adds a fact to the data base. It indicates logical implication, and it points backwards to
;; indicate backward chaining.
```
and _rules_, which are used to state contingent facts. For example, we can represent the
rule that Sandy likes _anyone_ who likes cats as follows:

```cl
;;     |---- head ----| |--- body ----|
   (<- (likes Sandy ?x) (likes ?x cats))
;; |------- predicate 'likes' ---------|

;; Exercise 11.2 suggest alternative syntax
(fact (likes Kim Robin))
(rule (likes Sandy ?x) if (likes ?x cats))
```
Getting computers to _remember_ complicated _interrelated facts_, and draw conclusions
from them is **inferences**.

In short: Facts are things true about the world, and reasoning procedures (or inferences)
are ways to follow reasoning chains between facts.

You can interpret this in two ways:<br/>
- _"For any X, Sandy likes Χ **if** Χ likes cats."_ - **declarative interpretation**

- _"If you ever want to show that Sandy likes some X, one way to do it is to show that_
  _Χ likes cats."_ - **procedural interpretation** (_backward-chaining_ interpretation)

**Tip:** Fact is just a rule that has no body

Here is the algorithm that searches for solutions:

    1. Try to locate Q(query) itself in the data base. If you can, then return success.
       _"If you ever find out that some Χ likes cats, then conclude that Sandy likes X."_
       This is _forward chaining_: reasoning from a premise (statements that are assumed
       to be true) to a conclusion.

    2. Otherwise, try to locate a conditional sentence of the form (P is a predicate)
       _If P1 and ... and Pn then Q_
       in the data base. If you cannot, then return failure.

    3. Otherwise, use backward-chaining to try to establish _P1, then P2, ... ,then Pn_.
       If these are all successful, then return success.

    4. Otherwise, go back to step 2 and look for another conditional.

* Prolog provides logic variables instead of "normal" variables.<br/>
  **Logic variables represent values which value you don't know yet.**

**ATTENTION:** With introduced Lisp syntax we can't import rules from another package.

### Unification and Pattern Matching

Within the unification framework, variables (such as `?x` and `?y`) are called _logic_
_variables_. Like normal variables, a logic variable can be assigned a value, or it can be
unbound. The difference is that a **logic variable can never be altered**.

A logic variable is bound by _unification_ rather than by assignment. _Unification_ is a
straightforward extension of the idea of pattern matching. In unification, two patterns,
each of which can contain variables, are **matched against each other.**
```cl
;; second argument do not contain variables
(pat-match '(?x + ?y) '(2 + 1)  ;=> ( (?Y . 1) (?X . 2))

;; search for ?x AND ?y
(unify '(?x + 1) '(2 + ?y)) ;=> ((?Y . 1) (?X . 2))
```

Another difference between simple pattern matching and unification is that unification
allows two variables to be matched against each other.

```cl
;; binding ?x to ?y
(unify '(f ?x) '(f ?y)) ;=> ((?X . ?Y))
```

It does provide a way of stating that variables are equal to other variables or
expressions. It does not provide a way of automatically solving equations or applying
constraints other than equality.

* Prolog provides automatic backtracking (once again)

In Prolog, each query leads to a search for relations in the data base that satisfy the
query. If there are several, they are considered one at a time. It is possible that one
answer is found, but that later in the back-chaining procedure, the answer leads to
failure. In this case, one must go back to where the answer was found, and see if there is
another answer to use in its place.

**Keep in mind that for some problems, the naive automatic search will be too inefficient,**
**and the programmer will have to restate the problem.**

**Rule**: _If a value is fully determined by other values, then avoid guessing the value_
_and later testing if it is correct._

```prolog
uniq3(A,B,C),         % Guess at A,B,C.
B is (A+C) mod 10     % Then test if B is OK.

% write this:

uniq2(A,C),          % Guess at A and C.
B is (A+C) mod 10,   % Calculate B once.
uniq3(A,B,C)         % Test that all values are unique.
```
Abstractly, the unification problem is the following: _Given two descriptions X and Y, can_
_we find an object Z that fits both descriptions?_

### Infinite unification

_infinite unification_ - attempt to unify a variable with a structure containing that variable.

```cl
;; X is matched against a structure that contains X itself
(unify '?x '(f ?x)) ;=> ((?X F ?X))
```
Here ```((?x f ?x))``` really means ```((?x . ((f ?x))))``` which is ```((?x . ((f ((f ?x))))))``` and so on.
_The easiest _way to deal with such infinite structures is just to ban them._ This is known in
unification circles as the **occurs check**.

### Implementation notes

-  A set of facts and rules about the same relation is called a **predicate**.<br/>
```(<- (likes Sandy cats))```. _'likes'_ is the predicate.

- _In Lisp, every symbol has a property list._ Property lists provide basically the same
  facilities as association lists and hash tables: You can store a value in a property list
  under a given key (called an **indicator**), and later look things up in the property list by
  supplying the indicator. You can see stored: ```(symbol-plist '<predicate-name>)```

It is very convenient to store in *db-predicates* only clause heads and using fact that it
is a symbol to add to it as plist clause body as list with indicator 'clauses'.

- The goals that are not matched against data base but rather causes some procedure to take
  action are called _primitives_, because they are built-in to the language, and new ones may
  not be defined by the user. _Primitives_ will be represented as Lisp functions.

- You can't port directly Prolog programs in Lisp-Prolog. For example:

```prlog
len([], 0).
len([H|T], N) :- len(T, Nt), N is Nt + 1.
```
and
```cl
(<- (length () 0))
(<- (length (?h . ?t) (1+ ?n)) (length ?t ?n))
```

This is because we do not have primitive ```is``` that should evaluate ```(1+ (1+ 0))```
to ```2```. To accomplish this **Exercise 11.11** should be made.

- It turns out the user must be concerned not only about the logic of the problem but also
with the flow of control. Prolog is smart enough to backtrack and find all solutions when
the search space is small enough, but when it is infinite (or even very large), the
programmer still has a responsibility to guide the flow of control.

- _Instead of enumerating complete candidate solutions, unification allows us to specify_
_partial candidates._ Unification serves the same purpose as the delay macro. It allows us
to delay deciding the value of some attribute as long as possible, but to immediately
reject a solution that tries to give two different values to the same attribute. That way,
we save time if we end up backtracking before the computation is made, but we are still
able to fill in the value later on.

- Prolog is generally not as efficient as an assembly language, but it can be more concise
as a specification language. The user writes specifications: lists of axioms that describe
the relationships that can hold in the problem domain. If these specifications are in the
right form, Prolog's automatic backtracking can find a solution, even though the
programmer does not provide an explicit algorithm.

- Note that there is no way in Prolog to express a **true definition**. We would like to say
that _"P is the parent of C if and only if C is the child of P,"_ but Prolog makes us express
the biconditional in one direction only.

- _What is a continuation?_

Continuation packages the current state of the computation into a function, which can be
stored away and invoked later. In this chapter continuations are stored in list and will
be invoked later (if needed). This definition will be used later in the book (```call/cc```).

This is not the meaning that is used in book - just list that contains data that could be
used for the next step.
