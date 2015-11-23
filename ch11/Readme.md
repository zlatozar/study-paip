## Chapter 11

The idea behind logic programming is that the programmer should state the _relationships_
that describe a problem and its solution. These relationships act as constraints on the
algorithms that can solve the problem, but the system itself, rather than the programmer,
is responsible for the details of the algorithm.

### Ideas behind the Prolog

* Prolog encourages the use of a single uniform data base.

In Prolog the assertions are called **clauses**, and they can be divided into two types:
_facts_, which state a relationship that holds between some objects e.g.
``` cl
(population SF 750000)
(capital Sacramento CA)
```
(We will use the macro ```<-``` to mark facts. Think of this as an assignment arrow which
adds a fact to the data base. It indicates logical implication, and it points backwards to
indicate backward chaining.)

```cl
;; The leftmost expression in a clause is called the head, and the remaining
;; ones are called the body.
(<- (likes Kim Robin))
```
and _rules_, which are used to state contingent facts. For example, we can represent the
rule that Sandy likes _anyone_ who likes cats as follows:
```cl
;; Use AND for clauses in body: Likes Lee AND Kim

;;  |--- head ---| |---------- body -----------|
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
```
You can interpret this in two ways:
    - "For any X, Sandy likes Χ if Χ likes cats." - _declarative interpretation_
    - "If you ever want to show that Sandy likes some X, one way to do it is to show that
      Χ likes cats." - _procedural interpretation_ (**backward-chaining** interpretation)

**Tip:** Fact is just a rule that has no body

Here is the algorithm that searches for solutions:

    1. Try to locate Q itself in the data base. If you can, then return success.
       _"If you ever find out that some Χ likes cats, then conclude that Sandy likes X."_
       This is _forward chaining_: reasoning from a premise to a conclusion.

    2. Otherwise, try to locate a conditional sentence of the form
       _If P1 and ... and Pn then Q_
       in the data base. If you cannot, then return failure.

    3. Otherwise, use backward-chaining to try to establish _P1, then P2, ... , then Pn_.
       If these are all successful, then return success.

    4. Otherwise, go back to step 2 and look for another conditional.

* Prolog provides logic variables instead of "normal" variables.<br/>
  **Logic variables represent values which value you don't know yet.**

Within the unification framework, variables (such as `?x` and `?y`) are called _logic_
_variables_. Like normal variables, a logic variable can be assigned a value, or it can be
unbound. The difference is that a **logic variable can never be altered**. A logic variable
is bound by _unification_ rather than by assignment. _Unification_ is a straightforward
extension of the idea of pattern matching. In unification, two patterns, each of which can
contain variables, are matched against each other. The difference between simple pattern
matching and unification is that unification allows two variables to be matched against
each other.
```cl
;; binding ?x to ?y
(unify '(f ?x) '(f ?y)) ;=> ((?X . ?Y))
```

* Prolog provides automatic backtracking.

In Prolog, each query leads to a search for relations in the data base that satisfy the
query. If there are several, they are considered one at a time. It is possible that one
answer is found, but that later in the back-chaining procedure, the answer leads to
failure. In this case, one must go back to where the answer was found, and see if there is
another answer to use in its place.

**Keep in mind that for some problems, the naive automatic search will be too inefficient,**
**and the programmer will have to restate the problem.**

Abstractly, the unification problem is the following: _Given two descriptions X and Y, can_
_we find an object Z that fits both descriptions?_

### Infinite unification

_infinite unification_ - attempt to unify a variable with a structure containing that
variable.
```cl
(unify '?x '(f ?x)) ;=> ((?X F ?X))
```
Here ```((?x f ?x))``` really means ```((?x . ((f ?x))))```, so `?X` is bound to `(F ?X)` _The easiest_
_way to deal with such infinite structures is just to ban them._ This is known in
unification circles as the occurs check.

### Programming in Prolog
