### Prolog language briefly

A Prolog program consists of a collection of _procedures_. Each procedure defines a
particular _predicate_, being a certain relationship between its arguments. A procedure
consists of one or more assertions, or _clauses_. It is convenient to think of two kinds of
clauses: _facts_ and _rules_.

_In Prolog, there's not often a finely detailed step-by-step recipe._ If you find the
relation you have to write few lines of code.

**Tip**: All Prolog code lives in a database so Prolog program can be regarded as a
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
friend(bonny,joe).  % "bonny is a friend of joe."
mother(sally,sue).  % "sally is the mother of sue."
```

**Tip:** Think of ```fact`` as a clarification.

#### Rules

A single program-clause can either be a ```fact``` or a ```rule```. Rules define complicated
relationships among objects by announcing that the _head_ of the rule is true if all of
the goals in the body of the rule are true. A rule has the form:

```
|-- head --|    |-- body --|
 B1......Bn  :-  A1......An

```

```:-``` can be read as the word ```if``` or ```is implied by```

Here is an examples:

```prolog

happy(X) -:    % Any X is happy if
   has(X,Y),   % X has Y and
   dog(Y).     % Y is a dog.
```

**All rules and fact with equal name form the definition of the procedure.**

Prolog programs are composed of facts and rules. Facts are stored with rules. A collection
of facts and rules is commonly referenced as _database_. There is no need Prolog programming 
to differentiate between programming and data.

#### Representing facts

Prolog syntax and semantics are much closer to _formal logic_.  The first thing is the
_predicate expression_. Formally, a predicate expression is a name - a predicate followed
by zero or more arguments enclosed in parentheses and separated by commas. In general
predicate symbol, is the name of a _relation_ that holds between the arguments. Predicate
names and arguments can be composed of any mixture of letters and numbers, except that
names for now must start with a **lower-case** letter. The underscore symbol ```_``` also
counts as a letter, and we will often use it to make names more readable. So these are all
predicate expressions:

```prolog
p(x)
q(y, 3)
city(monterey, california)
noarguments
pi(3.1416)
long_predicate_name(long_arguments_name, 3)
```

It could be in other language than English:

```prolog
кораб(българия)
пи(3.14)
дружба(русия, българия)
```

A **period** at the end will signal the end of a fact. We could also tell the computer:

```prolog
% this is a comment
ship(kennedy).
ship(vinson).

% this was an example of type predicate
```

Predicates can mean many things. But they do fall into categories:

|                        | Type pred.     | Predicate property  | Relationship pred.  | Database pred. | Function pred. | Probability pred.|
|:-----------------------|----------------|---------------------|---------------|----------------|------------------|------------------|
| Number of arguments    |      1         |       2             |      2        |   1 or more    | 2 or more        | 1 or more |
| Nature of arguments    | a thing | a thing and property| two things | a thing and property | last is result of operation on others |last is probability of the fact truth |
| Description            | give a class that the thing belongs to | gives property of the thing  | describes relationship of two things | like a data record  | describes a function mapping | variant of previous kinds for partly certain facts |
| Examples               | ship(Kennedy) vehicle(ship)  | color(kennedy, gray) location(kennedy, 14n35e) | part_of(kennedy, us_navy) a_kind_of(kennedy, ship) | ship(kennedy, gray, 14n35e, 16feb85) | sum(3, 4, 7) | color(kennedy, gray, 0.8)  |
| Meaning of the examples| "The Kennedy is a ship" "A ship is a vehicle" | "The color of the Kennedy is gray" "The location of the Kennedy is 14n35e" | "The Kennedy is part of the US Navy" "The Kennedy is a kind of ship" | "There is a ship record with entries Kennedy, gray, 14n35e and 16feb85" | "The sum of 3, 4, 7" | "We believe with certainty 0.8 that the Kennedy is gray|

NOTE: We've said these predicates(functors) are like the _types_ in computer languages, but there are
some differences. The main one is that they need never be defined anywhere. The _type_
(predicate, functor) names are just arbitrary codes used in look up.

#### Type of predicates (functors)

- Predicate property

Two-argument expressions in which the predicate name is the name of a property, the first
argument is the name of a thing, and the second argument is the value of the property. The
preceding example could be rewritten better as:

```prolog
ship(enterprise).
% shows the relation between 'gray' and 'enterprise' ( reveals some property)
color(enterprise, gray).
size(enterprise, big).
```

- Predicates for relationships or clarification

  They are important because a lot of human reasoning seems to use them - people need to
relate ideas. For instance, we can use a ```part_of``` predicate of two arguments which
says that its first argument is a component within its second argument.

We could give as facts:
```prolog
owns(tom, fido).
owns(tom, toms_car).
```
  It's easy to get confused about argument order in relationship-predicate expressions. So
we'll try to follow this convention: _if the predicate name is inserted between the two_
_arguments, the result will be close to an English sentence giving the correct meaning_. So
if we insert ```owns``` between ```tom``` and ```fido``` we get ```Tom owns Fido```.

  Special relationship predicate is frequently used in artificial intelligence. It's called
```a_kind_of``` or ```is_a``` (we prefer the first name, because "is" is vague), and it
can replace all type predicates. Its first argument is a thing, and its second argument is
the type of that thing (the predicate name in the one-argument form considered before).

  Pictures can make a complicated set of facts a lot clearer. There's a simple pictorial
way to show the predicate expressions we've been discussing: _the semantic network._
Unfortunately, there is a major restriction on it: semantic networks can only
directly represent predicates of two arguments (so type predicates must be in the
two-argument form). Let's illustrate with an example.

NOTE: This database will be used in following examples.

```prolog
a_kind_of(enterprise,ship).
a_kind_of(kennedy,ship).
part_of(enterprise,u_s_navy).
part_of(kennedy,u_s_navy).
part_of(u_s_navy,u_s_government).
a_kind_of(u_s_government,government).
color(ship,gray).
location(enterprise,15n35e).
has(u_s_government,civil_service_system).
```

Here is the picture:

![Semantic network](semantic_network.png "semantic_network")

- Loading a database

If this database is saved in a file called 'armada.pl' then start Prolog current directory
and type:

```prolog
?- consult('armada.pl').

% or

?-['armada.pl'].
```
after that you can do queries. Do not forget the dot at the end.

- Predicates with three or more arguments (database predicate, function predicates)

One idea is to include multiple property values and relationship information in a single
fact, much like adjectives and adverbs modifying a noun or verb. These sort of predicates
define a relational database of facts. Another important category of predicates with often
many arguments (though they can also have just two) is that representing results of
actions - in mathematical terminology, _functions_. Here is examples:

```prolog
sum(1,5,6).
sum(2,1,3).
sum(2,2,4).
sum(2,3,5).
sum(2,4,6).
```

NOTE: To avoid confusion, we follow the convention that **the last argument** _always_
_represents the result of the function._

- Probability predicate

We have assumed so far that facts are always completely certain. In many situations, facts
are only probably true.  Then we will use the mathematical idea of probability, the
expected fraction of the time something is true. _We will put an approximate probability_
_as a last argument to a predicate._

### Queries

Queries form provide a means for retrieving information from a logic program.
A query has the special form:

```
? - G1,G2,...,Gn
```

A query _initiates the execution of a program_ and is typically typed at the terminal with
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


The interpreter now knows two facts that state that ```joe likes apples``` and ```bonny is a friend
of joe```. The single rule expresses a more complex relationship: ```Any X likes any Y if X is
a friend of Z and Z likes Y```. The database, once loaded, can be interrogated with queries.

Consider the following query:

```prolog
?- likes(joe,X).
```

The Prolog interpreter will start at the top of the database and search for a _fact or rule_
_whose head matches our query_. The interpreter knows it is looking for facts or rules named
```likes``` with **two** arguments, the first of which must be ```joe``` or _able to be instantiated to_
```joe```. The second variable ```X``` has not yet been instantiated. The first fact encountered
that meets all the requircnents is _fact 1_ if ```X``` is instantiated to ```apples```. The interpreter
will respond to our query with:

```
X = apples
more (y/n)? y
no
```

The interpreter can not find any more facts or rules that it can match with our query.

#### Variables and queries

We can put facts into a computer. So what can we do with them? Well, we want to _reason_
about facts and conclude new facts - what's called _inference._

One thing we can do with facts in a computer is to look them up.  You're in query mode
when the Prolog interpreter types ```?-``` at the front of every line. Query mode is the
way database query languages work, like SQL. Facts form _Prolog database_. Example:

```prolog
% note the period at the end
?- port_of(kennedy,u_s_navy).
```

So ```yes``` means _"I found it"_ and ```no``` means _"I couldn't find it"._ We call a ```yes```
a query _success_ and ```no``` a query _failure._

- Queries with one variable

We might instead want to ask if a ```part_of``` fact has enterprise as its first argument
and anything at all as its second argument. We can do this by querying

```prolog
?- partr_of(enterprise,X).
```
Read this as _"Find me an X such that part\_of(enterprise,X) is true"_, or simply as
_"What is the Enterprise part of?"_ The Prolog interpreter will go through its two-
argument facts in order, trying to match each to the query. When it finds one that matches
in predicate name and first argument, it will type ```X = ``` followed by the fact's
second argument, instead of typing yes. Or in technical jargon, it binds or matches ```X``` to a
value and prints it. ```X``` here is a variable.

  Prolog variables are designated by a **capitalized** first letter in a word (followed by
other letters and numbers, either capitalized or uncapitalized). Variables can only be
arguments in Prolog; they can't appear as predicate names. This means Prolog represents
only _first-order logic_. First-order logic is sufficient for nearly all
artificial-intelligence applications. A variable can appear anywhere among the arguments
to a predicate expression in a query:

```prolog
?- part_of(X,u_s_navy).
```
This means Prolog can answer quite different questions depending on where we put variables
in the query. We can have more than one variable in a query. If we were to query for our
database:

```prolog
% Asking: "What things are part of other things?"
?- port_of(X,Y).
```

- Matching alternatives

More than one thing (value) can match (bind) a query variable. The Prolog interpreter
will find the first, print it out, and stop and wait. If just one answer is sufficient,
type a carriage return. But to see the next answer (if any), type a semicolon (```;```) and
a carriage return. We can keep typing semicolons, and it will keep finding new matches,
until it can't find any more and it must answer ```no```.

- Multicondition queries

A Prolog interpreter also lets us specify that several different conditions must succeed
together in a query. This lets us specify _"chains of reasoning"_, like those so important
to detectives in mystery fiction. Let's illustrate with an example - suppose we want to
know the color of the ```Enterprise```:

```prolog
% this is wrong
?- color(enterprise,C).
```

we get ```no``` with our example database, because the ```color``` fact is about ships in
general and not the ```Enterprise```. Let's think over this "error". If you see the database
there is one fact ```color(ship, gray)```. This means that _all ships are gray_.
```color``` rule "understands" only from ships so we have to tell somehow that
```Enterprise``` is a ship and then pass the "result" to ```color```.

```prolog
?- a_kind_of(enterprise,T), color(T,C).
```

This represents an "and" (_conjunction_) of two predicate expressions, both of which must
succeed for the whole match to succeed. It works this way: we first try to answer the
query ```a_kind_of(enferprise,T).``` Then for that particular ```T```, we answer the query
```color(T,C).```. Using our example database, we first match ```T``` to ship in the
first-listed fact. We then look for a color fact in which this ```T``` is the first argument,
and the seventh-listed fact qualifies; we can then match ```C``` to gray. Prolog answer ```T = ship, C = gray```

So commas between predicate expressions in a query line mean to query the predicate
expressions in order, reusing the same values for any same named variables. Commas are
much like a logical ```and``` since all the subqueries (predicate expressions) must succeed
for the whole query to succeed. But Prolog commas, unlike the logical ```and```, imply an
_order of processing._

STYLE: To make commas easier to spot, we'll often put spaces after them in
       queries; these spaces are ignored by the interpreter.
       Don't put spaces in predicate expressions.

Logical ```or``` (_disjunction_) is represented by a semicolon instead of a comma.  As with
```and```s, the expressions of the "or" are tried in order.  Parentheses can group
subexpressions of ```and```s and ```or```s. Example:

```prolog
?- part_of(enterprise,X); (part_of(enterprise,Y), part_of(Y,X)).
```

This reads: _"Find me an X such that either the Enterprise is part of it, or the
Enterprise is part of some Y that is part of it."_

**STYLE:** <br/>
       (1) ```and```s occur more often in applications,<br/>
       (2) they often require parentheses and so are hard to read, and <br/>
       (3) there is a better way to get the effect of an ```or```, we see later<br/>

So we have ```and```s and ```or```s. All we need to complete a Boolean algebra is a negation or
```not```. This is accomplished by the built-in predicate not whose one argument is a predicate expression.

```prolog
?- not(color(enterprise,green)).
```

How will the Prolog interpreter ever be sure something is not true? Strictly speaking, it
can't, since facts that directly say something is false are not permitted in Prolog. So
```not``` is defined to mean the interpreter couldn't find a fact.

**Hint:** Questions in English about a database often map directly into Prolog queries.
Words like ```is```, ```are```, ```does``` and ```did``` at the beginning of a question suggest
queries without variables (yes/no queries). Words like ```what```, ```which```, ```who```,
```where```, ```when```, and ```how``` suggest variables.

### Prolog syntax

Prolog uses only one data-type, the ```term```. Any term has one of three forms:

```
1. a constant
2. a variable, or
3  a structure.
```

#### constant

A ```constant``` is either an ```atom``` or a ```number```.  An atom can be represented
either by a sequence of alphanumeric characters beginning with a lower-case, letter or by a
sequence of characters enclosed in single quotes.

#### variable

A variable in Prolog stands for some _definite_ but **unidentified** object. A variable is
expressed by a sequence of alphanumeric character or underscores, beginning with either an
upper-case letter or an underscore. Variables in Prolog do not designate storage-locations
in memory as do variables used in conventional languages. Instead, variables are used for
pattern-matching within a term. Example:

```
Constants       Variables
--------------------------
  a               A
  ab              Ab
  a_b             A_B
  'AB'            _AB5
  1              _1
```


#### structure

A ```compound term``` is composed of a ```functor``` and a sequence of one or more arguments
and is commonly referred to as a _structure_. A structure has the form:

```
foo(t1,....tn)
```

where ```foo```, the functor (or predicate), is an atom and ```t1,...,tn```, the arguments, are terms.
The number of arguments is referred to as the _arity_ of the term. The Prolog interpreter
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

A list is expressed with the following format: ```[Head|Tail]```. For example ```[1,2,3]```.

The head of a list is the first term (or element) in the list. The head of the list in
the above example is the element "1". The tail of a list is itself _a list_ that consists of
everything in the original list except the head(like in lisp). The tail of the above
example is the list [2,3]. Square brackets ```[]``` are used to denote an empty list.
The list ```[1,2,3]```, written in this form would be ```[1,2,3|[]]```.
 
Sometimes it is convenient to denote more than one element at the front of a list. To show
this, the representation is ```[X,Y,Z|Y]``` where ```X```, ```Y```, and ```Z``` are the first three elements of
the list and ```Y``` is the tail.

#### Punctuation

- ```;``` is logical **OR**

- ```,``` is logical **AND**, separates goals

- ```=``` represents unification in Prolog (see also arithmetic operations)

- ```\==``` stands for inequality

#### Control of Program Execution

A conventional algorithm written in either a procedural or functional language is thought
to be described as:

```
program = description of (logic + control)
```

with the logic component describing the domain-specific part of the algorithm and the
control part involving the solution strategy. Algorithms written in a procedural
or functional language must specify **both** the logic and the control components: _they are_
_intermixed inseparably in the code_. Algorithms written in a relational language
need only specify the logic component of the problem unless for reasons discussed later
it is advantageous for the programmer to modify the control component (see _cut_).

Prolog only requires the user to provide facts and rules describing relations that are
pertinent to the problem domain. Control, often viewed as the _"how"_ of the problem
is ideally left up to the machine. In logic programming, control and logic are separate
components of a program and may be specified separately. An algorithm written in a logic
programming language can be described as:

```
algorithm = logic - control
```

where the logic and the control components can be disjoint. The order of appearance of
the clauses in a logic program should have no logical (declarative) significance because each
clause states some property of the predicate independently. This holds true for Prolog as
long as control of program execution is left entirely up to the interpreter.

#### Unmodified Control

As an example of a problem that can be solve by allowing the Prolog interpreter to have
full control, consider the task of determining whether an element is a member of a
list. Any element X is a member of a list if

```
either (1): X is the head of the list
or     (2): X is a member of the tail of the list.
```

These relations are described in Prolog syntax as follows:

```prolog
% not optimal just an example
member(X,[X|Rest]).                   % rule 1
member(X,[Y|Rest]) :- member(X,Rest). % rule 2
```

 The name of this procedure is ```member```. The first Horn-clause states that the element ```X```
is a member of any list that has ```X``` as its head and ```Rest``` as its tail. If the element ```X``` is
the element we are searching for and it occurs at the head of the list then membership is
determined and true is returned to the invoking call. If the element ```X``` is not the head of
the list, the first clause fails and the second clause is invoked.
 The second clause states the second condition for membership: that is the element ```X``` is a
member of a list with an element ```Y``` as its head and ```Rest``` as its tail if the element ```X``` is a
member of ```Rest```.
 The logic of determining membership is completely specified while the control aspect is
left completely unspecified, leaving the interpreter to solve any queries regarding member
as it pleases.  Note that ```rule 1``` could be switched with ```rule 2``` and only the
efficiency of the execution of the procedure would be affected.

#### Modified Control

 Sometimes it can be advantageous to modify the control component in a logic
program. Modification is usually done to improve the efficiency of the algorithm.
Efficiency can be enhanced by using a special term in Prolog called the _"cut"_
represented by the symbol ```"!"```.
 A cut allows succeeds as a goal and allows a programmer some control over the search
mechanism by limiting the interpreter's access to specified branches of the search
tree. _The effect of the cut limits the number of solution: to the_ _first one found_ once
the "cut" has been encountered. Consider this modification to an example:

```prolog

a(1).
a(2) :- !.
a(3).
b(O).
b(2).
b(2).
b(3).

```

If the database is queried with the same query
```prolog
?- a(X),b(X).
```
the interpreter will proceed down the new search-tree, finding the first match between
```a(1)``` and ```b(1)```.

Proceeding on, the interpreter picks up ```a(2)``` and encountered, _the cut_. The interpreter
tries to match ```a(2)``` with ```b(O)``` and ```b(1)```  before finding
a match with ```b(2)```. Here is where the difference lies: _When the second match is_
_reported to the screen, the interpreter will not continue to search for additional_
_solutions_. Clearly the database contains one more match, namely ```X = 3```, but the cut
effectively "pruned" any other possible match after ```a(2)``` from the search-tree.
The execution of the query could result in only one answer, ```X = 2```.  It is important that
the modification of the control strategy should only affect the behavior of the computer
and not the meaning of the program. For example a programmer may want to go to
a local convenience store.

Cut has several uses:

1. To change a non-deterministic predicate into a deterministic (functional) one. For
example, we wish to check whether ```X``` is a member of a list ```L```. If it is a member, we wish to
discard the alternative choices. This is done by a deterministic ```membercheck``` predicate,
which might be more efficient than the usual member, but cannot be used to generate
multiple solutions.

```prolog
membercheck(X, [X|_]) :- !.
membercheck(X, [_|L]) :- membercheck(X, L).
```
```
?- membercheck(X, [a, b, c]).
X= a;
no.
```

NOTE: ```_``` name of the variable indicates that we do not interested of its
value. That's why do not pollute name space. 


**Tip:** _Cut at the end of a rule means you want only one solution to the query_
_of that rule._

2. To specify the exclusion of some cases by 'committing' to the current choice.
```
s(X) = if p(X) then q(X) else r(X).
```

For example, the goal ```max(X,V,Z)``` instantiates ```Z``` to the greater of ```X``` and ```V```:

```prolog
max(X, V, X) :- X >= V.
max(X, V, V) :- X < V.
```

If max is called with ```X>=Y```, the first clause will succeed, and the cut will assure that
the second clause (the alternative choice) is never made. The advantage is that Prolog can
disregard the second clause as an alternative backtracking choice. One consequence of the
max program being written using a cut is that the test does not have to be made twice if ```X<Y```.

```prolog
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y) :- X < Y.
```

**Tip:** The cut prunes Prolog's search tree. Place the cut (if you need one) at the exact
point where you know that the current path is the correct one.

```prolog
p(X) :-
    a(X), b(X), !, c(X), d(X).
```

Here, ```a(X)``` and ```b(X)``` can be tests that are allowed to fail, but ```c(X)``` and ```d(X)``` should
always succeed right away, or we may accidentally cut off a correct answer.

If he were to write a logic program outlining all the methods of transportation available,
along with the constraints relating them, the computer could be used to suggest the best
method of travel given a set of existing conditions. One such condition might be that it
is raining; in such a case, walking would not be appropriate. The control of the program's
execution could be altered to exclude exploring any modes of transportation that would
expose the programmer to the elements. By doing so, the execution of the program is made
more efficient by immediately ruling out entire classes of inappropriate solutions: no
time is wasted exploring unfruitful paths.

#### Importance of Ordering Clauses and Goals

Limitations imposed by the interpreter require the programmer to give careful
consideration to the order in which the clauses of a program and the goals of a body are
listed. _Improper ordering of clauses can affect the efficiency of the search process and,_
_in some cases, cause the program not to execute properly_. Consider the rule:

```prolog
ancestor(X,Y) :-
   ancestor(X,Z), parent(Z,Y).
```

which can be interpreted literally as ```Y is an ancestor of Y if X is an ancestor of Z and Z is a_
_parent of Y```.

 The ordering of the two goals of the body makes logical sense but when the Prolog
interpreter attempts to invoke the procedure, the first goal recursively calls itself
creating an infinite loop. If the two goals in the body are swapped, the logical meaning
of the rule is preserved but the interpreter will attempt to satisfy the parent-goal first
and an infinite loop will not result. 
 Often, careless ordering of clauses will not cause a program to execute improperly but
will affect the efficiency of execution. Efficiently of execution can be affected by:

1. the order of appearance of procedures in a program;
2. the order of appearance of clauses in a procedure; and
3. the order of appearance of goals in a rule body.

#### Writing in Prolog

- The programmer analyzes the significant entities, functions, and relations in the
application domain, and chooses symbols to represent each of them.

- The programmer defines each of the significant functions and relations semantically. In
the case of a relation, this entails specifying which instances of it are true and which
are false.

- The programmer defines each relation axiomatically with Prolog clauses. An axiomatic
definition of a relation is successful if it captures the sense of the semantic
definition. The set of axiomatic definitions of all significant relations in the
application domain is a program that embodies the structure of the domain.

- The programmer or user uses the Prolog interpreter to evaluate queries to the set of
program clauses. The interpreter's answer to a query reflects the structure of the
application domain, and can be used to solve problems in the application domain.

#### Models - declarative and procedural

- Declarative

The declarative semantic model of Prolog specifies the truth value of instances of
relations. The word _declarative_ is used because a Prolog clause declares that a relation
holds between its arguments if all of the conditions of the clause are met. For instance,
the following clause:

```prolog
executive(Name, Salary) :-
    employee(Name, Salary), Salary > 70000.
```

can be read as:

```
Anyone (Name) is an executive if
    he or she is an employee
    with a salary greater than $70,000.
```

Read according to the declarative model, Prolog clauses are formulas
of first order predicate logic written in Horn clause form. The only
logical connectives that can occur are ```if```, ```and```, and ```or```.

- Procedural

According to the procedural semantic model of Prolog, the conditions of a clause specify a
_process_ to establish the truth value of the conclusion of the clause. A set of clauses
with the same predicate name and the same number of arguments is understood as a
procedure. A query with the same predicate name and same number of arguments as a
procedure is understood to be a call to that _procedure_. For a query to succeed, the
procedure it calls must be successfully evaluated. Each condition of a clause is also
understood as a call to a procedure.

Read procedurally, the meaning of the "executive" clause above is:

```
One way to find an executive is:
    first, find an employee,
    then second, verify that the salary
    of the employee is greater than $70,000.
```

#### Recursion

Recursion is one of the most powerful features of Prolog because recursion can be
exploited to perform repetitive tasks. Let's see again ```member`` relation:

```prolog
% not optimal just an example
member(X,[X|Rest]).                   % rule 1
member(X,[Y|Rest]) :- member(X,Rest). % rule 2
```

 Each time X is not the element at the head of the list, ```rule 1``` fails and the interpreter
descends another level of recursion until X occurs at the head of the list or Rest is
exhausted. Each time ```rule 2``` is invoked, the tail of our list will be successively divided
into an instantiation for ```Y``` and ```Rest``` until the end of the list is encountered. The end of
a list is denoted by an empty list ("[]") and is used to form a boundary condition. Using
the empty list to form a boundary condition for the member example can be interpreted
literally as telling the Prolog interpreter ```if you look through the entire list of
candidate elements and cannot find an occurrence of X then stop your search because X is
not a member of the list```.
 For this particular example, the boundary condition does not need to be stated explicitly
because the interpreter will progressively examine the elements of the list starting from
the head and working towards the tail. If the end of the list is encountered ```[]```, there
could not have been an occurrence of ```X``` in the list: hence, the search fails and the
interpreter stops looking.
 Most logic problems require the boundary conditions to be explicitly declared in
order to avoid infinite loops. If a non member procedure were desired, the boundary
condition would have to be explicitly defined in order to instruct the interpreter when to
stop looking.  The boundary condition, written in Prolog, would look like:
```prolog
non-member(X, [])
```

and would be literally interpreted as ```it is true that X is not a member of an empty list```.
The remainder of the Prolog code for non-member is:

```prolog
non-member(X,[Y|L]) :- X \== Y, non-member(X,L).
```

#### Tail recursion

For a predicate defined by a recursive clause, the self-invocation should be the last goal in its body.
And, for the Prolog system to discard all references to the goals preceding the last one (and thereby
freeing up memory), a cut (```!```) should be introduced just before the self-invocation.

```prolog

% Calculating the sum of the (integer) entries in a list
sum([],S,S).
sum([H|T],Acc,S) :- NewAcc is Acc + H, !, sum(T,NewAcc,S).
```

#### Arithmetic

**Prolog is not the programming language of choice for carrying out heavy-duty**
**mathematics**. It does, however, provide arithmetical capabilities. The pattern for
evaluating arithmetic expressions is (where Expression is some arithmetical expression)

```
X is Expression
```

Here is an example:

```prolog
% Not optimal just an example
factorial(0, 1).
factorial(X, R) :- N > 0, X1 is X-1, factorial(X1, S), R is S*X.
```
