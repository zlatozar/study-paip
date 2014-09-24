#### Chapter 4

- General Problem Solver(**GPS**) - a single computer program that could solve any problem,
given a suitable description of the problem.
It was the first program to separate its problem-solving strategy from its knowledge of
particular problems, and it spurred much further research in problem solving.

- Basic structuring of a problem (GPS was a goal seeking program)

  * define the goals e.g., take my son to nursery school
  * define preconditions for the goals
    e.g., the precondition for dropping my son off at nursery school is that my son is at
    home and the car works
  * define the means ("operators") for turning one set of conditions into another (states
    are specified)
    e.g., to ensure the car repair shop has my money, we can define an operator "give shop
    money" that changes the world so that "shop has money" is true and "I have money" is
    false.

- We'll say that an operator is made up of:

  * the action (e.g., "give shop money")
  * the preconditions (e.g., "have money")
  * the change of conditions resulting from taking the action; we specify this as conditions added and conditions deleted:
    giving the shop money adds the condition "shop has money"
    giving the shop money deletes the condition "have money" (which is also the precondition)

- GPS can solve any problem that can be posed in this way. However, as we shall see, it
  employs a kind of search process that would require far too much time to solve complex
  problems (such as chess playing). It can suffer from a "combinatorial explosion" of
  possibilities.

- The process of developing and AI computer program:

```
1. Describe the problem in vague terms
Rough idea - usually in English prose
2. Specify the problem in algorithmic terms
Program specification, where we redescribe the problem in terms that are closer to a
computable procedure.
3. Implement the problem in a programming language
4. Test the program on representative examples
5. Debug and analyze the resulting program, and repeat the process
```
- AI programming is largely exploratory programming; the aim is often to discover
more about the problem area rather than to meet a clearly defined specification.

- The basic algorithm of the GPS search process is as follows:

```
begin:
- save initial conditions as "current state"
- try to achieve all goals

how to achieve all goals:
- for each goal:
  - try to achieve the goal
- if all goals were achieved, return success

how to achieve a goal:
- if goal is already met (in current state), return success (it's achieved)
- else, for each operator:
  - if operator's "add list" contains the goal,
    - try to apply the operator
    - if successful, return success
- if no operators were successful,
  - return failure

how to apply an operator:
- try to achieve all of the operator's preconditions (treat them as goals)
- if successful (all preconditions met),
  - mark the operator as "applied"
  - add conditions in the operator's "add list" to the current state
  - delete conditions in the operator's "delete list" from the current state
  - return success
- else,
  - return failure
```

- Common Lisp idiom to assign to variable:
```cl
;; With passed values *state* and *ops* will be initialized
(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (every #'achieve goals) 'solved))
```
- `debug` and `undebug` functions are in _../auxfns.lisp_ file

- GPS Problems

  * Prerequisite clobbers sibling goal

Since each goal is achieved in a certain order, we might find that achieving a subsequent
goal erases the gains we made with a prior goal.
For example, if we have:
```
    - initial states: son at home, have money, car works
    - goal states: have money, son at school
```
Then the solution is: `drive son to school`

However, if we have:
```
    - initial states: son at home, car needs battery, have money, have phone book
    - goal states: have money, son at school
```
Then our simple GPS search finds the solution to be:
```
    1. look up number
    2. telephone shop
    3. tell shop problem
    4. give shop money
    5. shop installs battery
    6. drive son to school
```
However, the result is we don't have any money, because it was spent at the shop. GPS saw
"have money" as the first goal, and considered it already achieved (it was an initial
state). So then it moved on to the next goal, which actually spent all the money. **But GPS
did not go back and reconsider whether the first goal was still met.**

  * Leaping before you look

One could reorder the goals from the prior example to be "son at school, have money." In
this case, GPS will indicate to us that there is no solution. In a simpler example,
consider the goals:
```
    - jump off cliff, land safely
```
GPS will first find a way to achieve "jump off cliff," but only once achieved, figure out
how to "land safely." Our GPS algorithm has no way to "back track" during its search when
it discovers that not all parts of the goal can be met with the partial plan built so far.

  * Recursive subgoals

Suppose we had a goal like "call shop" which required "know phone number" which can be
achieved by "ask friend for a phone number." Further suppose asking a friend for a phone
number requires "know phone number." Then GPS will get stuck in a loop trying to figure
out how to get the friend's phone number from another friend in order to call the shop,
even if other operators are available for finding a phone number (such as looking in a
phone book).

  * Lack of intermediate information

Our simplistic GPS algorithm does not indicate why it failed to achieve its goals. It just
says "nope, can't do it." It would be useful to know which goals could not be achieved
and, perhaps, what are the best options for us (as humans) in order to make progress.

- Functions that return nil as an indication of failure and return some useful value
otherwise are known as **semipredicates**.
Be careful when use semipredicates:
  * Decide if nil could ever be a meaningful value
  * Insure that the user can't corrupt the program by supplying nil as a value
  * Insure that the program can't supply nil as a value

- The Not Looking after You Don't Leap Problem

In domains like the block world and maze world, repair often works, because all steps are
reversible. But in the taxi example, no amount of plan repair can get the money back once
it is spent, so the whole plan fails.
There are two ways around this problem. The first approach is to examine all possible
solutions, not just the first solution thatachieves each subgoal. The language Prolog do
it in this way. The second approach is to have 'achieve' and 'achieve-all' keep track of
a list of goals that must be protected.

- The Lack of Descriptive Power Problem

Often we want to characterize a state in terms of something more abstract than a
list of conditions.

  * we need to be able to state some kind of constraint on the goal state, rather than
    just listing its components.
  * It also is important, in many domains, to be able to state problems dealing with time:
    we want to achieve X before time T0, and then achieve Y before time T2,
    but not before T1.

- The Perfect Information Problem

But we have no way to represent a payoff "once in a while." Similarly, we have no way to
represent unexpected difficulties of any kind.

- The Interacting Goals Problem
People tend to have multiple goals, rather than working on one at a time.
