## Chapter 6

- If we left out the prompt, we could write a complete Lisp interpreter using just four
symbols:
```cl
(loop (print (eval (read))))
```
- Is this possible? **Yes!**

We would need a **lexical analyzer** and a **symbol table** manager. This is a
considerable amount of work, but it is all handled by READ.  We would need a **syntactic
parser** to assemble the lexical tokens into statements, READ also handles this, but only
because Lisp statements have trivial syntax: _the syntax of lists and atoms_. Thus READ
serves fine as a syntactic parser for Lisp, but would fail for Pascal. Next, we need the
**evaluation** or **interpretation** part of the interpreter; EVAL does this nicely, and
could handle Pascal just as well if we parsed Pascal syntax into Lisp expressions, PRINT
does much less work than READ or EVAL, but is still quite handy.

**The important point is not whether one line of code can be considered an**
**implementation of Lisp; it is to recognize common patterns of computation**.

For recurent patterns alternative is to create _an abstraction_, in the form of functions
and perhaps data structures, and refer explicitly to that abstraction in each new
application - in other words, to capture the abstraction in the form of a useable
software tool.

- Patterns matching could be improved - add a way to specify that a position is to be
filled by a member of some class of atoms. That class might be the numbers, or the
atoms of a particular length. Introduce the restriction feature code looks like this:
```cl
(pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) '(x = 3)) ;=> ((?N . 3))

```
- Both ELIZA and Lisp can be seen as interactive interpreters that read some input,
transform or evaluate the input in some way, print the result, and then go back for more
input.
- Common Pattern of Computation:
```cl
(defun program 0
    (loop
        (print prompt)
        (print (transform (read)))))
```
- There are **two** ways to make use of recurring patterns like this: _formally_ and
_informally_.
- The **informal** alternative is to treat the pattern as a cliche or idiom that will
occur frequently in our writing of programs but will vary from use to use. Easy to read.
- The **formal** alternative is to _create an abstraction_, in the form of functions and
perhaps data structures, and refer explicitly to that abstraction in each new application-
in other words, to capture the abstraction in the form of a useable software tool. Easy
for maintenance.
- The problem in designing a **"general"** tool is deciding what features to provide.
We can try to define features that might be useful, but it is also a good idea to make
the list of features open-ended, so that new ones can be easily added when needed.
- Features be added by **generalizing** or **specializing** existing.
- When the description of a problem gets this complicated, it is a good idea to
attempt a more formal specification.
- This style of programming, where _pattern/action_ pairs are stored in a table, is called
**data-driven programming**. It is a very flexible style that is appropriate for writing
extensible systems. In our case it is very convenient because pattern is the same only
function(predicate) that should be matched is different.

What actually is **data-driven programming**?

In data-driven programming, the data determines what function or program gets executed.
There is no central program that decides on the operations and flow of control.
    One way of realizing a data-driven system is to attach functions to data or types of data
and to invoke them when the data is encountered. Thus, the behavior of a data-driven
system is determined not by a central program but by the collection of functions attached
to the data. It is also important modularity device.
    Another way is to have table with types and actions (functions)

- A function that looks up a data-driven function and calls it is called a **dispatch**
function.

##### A Set of Searching Tools

- In general, a _search problem_ involves exploring from some starting state and
investigating neighboring states until a solution is reached. Read [here](generalizing-search.md)
how search is generalized step by step.
- Search problems are called _nondeterministic_ because there is no way to determine what
is the best step to take next. AI problems, by their very nature, tend to be
nondeterministic.

Abstractly, a search problem can be characterized by four features:
```
1. The start state.
2. The goal state (or states).
3. The successors, or states that can be reached from any other state.
4. The strategy that determines the order in which we search.
```
- **State space** - set of all possible states.
We can view the states as _nodes_ and the _successor relation_ as links in a graph.
- **Depth-First Search** - the longest paths are considered first. In other words, we
generate the successors of a state, and then work on the first successor first. We only
return to one of the subsequent successors if we arrive at a state that has no successors
at all. Much faster if it happens to find the goal at all.
- **Breadth-First Search** - the shortest path is extended first at each step. Requires
more storage, because it saves more intermediate states.
- The big difference between DFS and BFS is in the number of states considered at one
time.
- While breadth-first search is more methodical, neither strategy is able to take
advantage of **any knowledge about the state space**. They both search blindly.

In most real applications we will have some estimate of how far a state is from the solution.
- **Best-First Search** - the state that _appears_ to be best is searched first. To do
that  we need a **cost function** that gives an estimate of how far a given state is from
the goal.

The idea: discard unpromising paths. This runs the risk of failing to find a solution at
all, but it can save enough space and time to offset the risk.
- A best-first search that _keeps only a fixed number of alternative states_ at any one time
is known as a **beam search**. The difference is that beam search looks down _several_ paths
at once, instead of just one, and chooses the best one to look at next.
- We can find a goal either by looking at more states, or by being smarter about the
states we look at.
- Beam search is a best-first search when beam width is infinity.

- When beam width is one it is DFS with no backup also known as **hill-climbing**.
Think of a mountaineer trying to reach a peak in a heavy fog. One strategy would be for
the mountaineer to look at adjacent locations, climb to the highest one, and look
again. This strategy may eventually hit the peak, but it may also get stuck at the top of
a foothill, or local maximum.

- ```(defstruct (structure-name (option value)...) 'Optional doc' slot...)```
For example:
```cl
(defstruct (path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))
```
It uses the _:print-function_ option to say that all paths are to be printed with the
function `print-path`.

- Representing paths would lead to an advantage: _we could return the path as the solution,_
_rather than just return the goal state._
- The notation `#<...>` is a Common Lisp convention for printing output that can **not**
be reconstructed by ```READ```.
- `#<Path ~,1f km: ~{~:(~a~)~^ - ~}>` - what it means?

**Tip:** Remember that evaluation functions refer to the future, cost functions to the
         past.

#### Guessing versus Guaranteeing a Good Solution

- An algorithm that will _probably_ return a solution that is _close_ to the best solution,
but gives no guarantee is known as **non-admissible heuristic search**.
- **Iterative Widening** technique - start with a narrow beam width, and if that does
not lead to an acceptable solution, widen the beam and try again.

#### Searching Graphs

Any graph can be treated as a tree, if we ignore the fact that certain nodes are
identical.

**A*** is a search algorithm that can find the optimal path. It is essentially best-first
search where the cost of any search state is the cost of getting to the state from the
start state, plus a heuristic estimate of the distance from the state to the goal.

Provided that the value of the heuristic estimating function never exceeds the true
distance between the current state and a goal state, A* will always find a shortest
path. This is known as the **admissibility** of the A* algorithm.  Consider the heuristic
function always returns zero; A* with this heuristic is admissible, since it never exceeds
the true distance. A* with this heuristic function is the same as "uniform cost" search.
Although uniform cost search is **guaranteed** to find the shortest path, a more informed
heuristic will find it faster.

_An algorithm A1 is said to be more informed than an algorithm A2 if the heuristic_
_information of A1 permits it to compute an estimate h1 that is everywhere larger than_
_h2, the estimate computed by A2._

The algorithm for A* in pseudo code:

```Pascal
function A*(start,goal)

    // The set of nodes already evaluated.
    closedset := the empty set

    // The set of tentative nodes to be evaluated, initially containing the start node
    openset := {start}

    // The map of navigated nodes.
    came_from := the empty map

    // Cost from start along best known path
    g_score[start] := 0

    // Estimated total cost from start to goal through y.
    f_score[start] := g_score[start] + heuristic_cost_estimate(start, goal)

    while openset is not empty
        current := the node in openset having the lowest f_score[] value
        if current = goal
            return reconstruct_path(came_from, goal)

        remove current from openset
        add current to closedset
        for each neighbor in neighbor_nodes(current)
            if neighbor in closedset
                continue
            tentative_g_score := g_score[current] + dist_between(current,neighbor)

            if neighbor not in openset or tentative_g_score < g_score[neighbor]
                came_from[neighbor] := current
                g_score[neighbor] := tentative_g_score
                f_score[neighbor] := g_score[neighbor]
                                         + heuristic_cost_estimate(neighbor, goal)
                if neighbor not in openset
                    add neighbor to openset

    return failure

function reconstruct_path(came_from,current)
    total_path := [current]
    while current in came_from:
        current := came_from[current]
        total_path.append(current)
    return total_path
```

- When to use _search-all_ function?

In some applications, we may want to look at several solutions, or at all possible
solutions.  Other applications are more naturally seen as optimization problems, where we
don't know ahead of time what counts as achieving the goal but are just trying to find
some action with a low cost.

##### GPS as Search

- Pattern matching is one of the most important tools for AI.
