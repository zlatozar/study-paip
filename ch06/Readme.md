#### Chapter 6

- If we left out the prompt, we could write a complete Lisp interpreter using just four
symbols: `(loop (print (eval (read))))`
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
extensible systems.
- A function that looks up adata-driven function and calls it is called a **dispatch**
function.
- In general, a _search problem_ involves exploring from some starting state and
investigating neighboring states until a solution is reached.
- Search problems are called _nondeterministic_ because there is no way to determine what
is the best step to take next. AI problems, by their very nature, tend to be
nondeterministic.
- Abstractly, a search problem can be characterized by four features:
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
- In most real applications we will have some estimate of how far a state is from the
solution.
- **Best-First Search** - the state that _appears_ bot be best is searched first. To do
that  we need a **cost function** that gives an estimate of how far a given state is from
the goal.
- A best-first search that keeps only a fixed number of alternative states at any one time
is known as a **beam search**. The difference is that beam search looks down _several_ paths
at once, instead of just one, and chooses the best one to look at next.
- We can find a goal either by looking at more states, or by being smarter about the
states we look at.
- Beam search is a best-first search when beam width is infinity.
- When beam width is one it is DFS with no backup also known as **hill-climbing**.
- Hill climbing:
Think of a mountaineer trying to reach a peak in a heavy fog. One strategy would be for
the mountaineer to look at adjacent locations, climb to the highest one, and look
again. This strategy may eventually hit the peak, but it may also get stuck at the top of
a foothill, or local maximum.
- Representing paths would lead to an advantage: we could return the path as the solution,
rather than just return the goal state.
- The notation `#<...>` is a Common Lisp convention for printing output that can not be
reconstructed by READ.
- `#<Path ~,1f km: ~{~:(~a~)~^ - ~}>` - what it means?
- An algorithm that will _probably_ return a solution that is close to the best solution,
but gives no guarantee is known as **non-admissible heuristic search**.
- **Iterative Widening** technique - start with a narrow beam width, and if that does
not lead to an acceptable solution, widen the beam and try again.
- Pattern matching is one of the most important tools for AI.
