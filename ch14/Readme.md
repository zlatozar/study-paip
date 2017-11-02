### Chapter 14

 The ```expert-system``` approach offered an alternative. The key to solving hard problems
was seen to be the acquisition of special-case rules to break the problem into easier
problems.

The field of knowledge representation concentrated on providing clear semantics for such
representations, as well as providing algorithms for manipulating the knowledge. Much of
the emphasis was on finding a good trade-off between expressiveness and efficiency.

#### A Taxonomy of Representation Languages

• Logical Formulae (Prolog)
• Networks (semantic nets, conceptual graphs)
• Objects (scripts, frames)
• Procedures (Lisp, production systems)


#### A Logical Language: Prolog

Probably because Prolog is a compromise between a representation language and a
programming language.  Given two specifications that are logically equivalent, one can be
an efficient Prolog program, while the other is not.

Prolog's problems fall into three classes. First, in order to make the language
efficient, its expressiveness was restricted. It is not possible to assert that a person's
name is either Jan or John in Prolog (although it is possible to ask if the person's
name is one of those). Similarly, it is not possible to assert that a fact is false;
_Prolog does not distinguish between false and unknown_. Second, Prolog's inference
mechanism is neither sound nor complete. Because it does not check for circular
unification, it can give incorrect answers, and because it searches depth-first it can
miss correct answers. Third, Prolog has no good way of adding control information
to the underlying logic, making it inefficient on certain problems.

#### Problems with Prolog's Expressiveness

If Prolog is programming in logic, it is not the full predicate logic we are familiar with.
The main problem is that Prolog can't express certain kinds of indefinite facts. It can
represent definite facts: the capital of Rhode Island is Providence. It can represent
_conjunctions of facts_: the capital of Rhode Island is Providence and the capital of
California is Sacramento. But it can **not** represent _disjunctions or negations_: that the
capital of California is not Los Angeles, or that the capital of New York is either New
York City or Albany. We could try this:

(<- (not (capital LA CA)))
(<- (or (capital Albany NY) (capital NYC NY)))

but note that these last two facts concern the relation ```not`` and ```or`` , not the relation
capital. Thus, they will not be considered when we ask a query about capital!

Fortunately, the assertion "Either NYC or Albany is the capital of NY" can be rephrased
as two assertions: "Albany is the capital of NY if NYC is not" and "NYC is the capital
of NY if Albany is not:"

(<- (capital Albany NY) (not (capital NYC NY)))
(<- (capital NYC NY) (not (capital Albany NY)))

Unfortunately, Prolog's not is different from logic's ```not```. When Prolog answers "no"
to a query, it means the query cannot be proven from the known facts.

The problem is that Prolog equates "not proven" with "false". Prolog makes what
is called the closed world assumption - it assumes that it knows everything that is true.

We have seen that Prolog is not very good at representing disjunctions and negations.
But there is no good translation for "Jan likes someone."

(<- (likes Jan p1))
(<- (person p1) )

Here we have invented a new symbol, ```p1```, to represent the unknown person(someone) that Jan
likes, and have asserted that ```p1``` is a person. Notice that ```p1``` is a constant, not a
variable.

#### Problems with Predicate Calculus's Expressiveness


#### Problems with Completeness

#### Problems with Efficiency: Indexing
