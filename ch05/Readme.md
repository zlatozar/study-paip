#### Chapter 5

- `eliza1.lisp` contains basic version, `eliza.lisp` advanced one.

- What is a pattern?

```
var              match any one expression
constant         match just this atom
segment-pat      match something against a sequence
```

Pattern examples:
```cl
'(I need ?x)
'((?* ?p) need (?* ?x))
```

- Pattern matching

Pattern matching is the process of _comparing_ symbolic expressions to see if one is
similar to another.

- Symbols are not atoms

So far we have dealt with symbols as atoms - objects with no internal structure.
In particular, symbols have names, which are strings and are accessible through
the ```symbol-name``` function. Strings in turn have elements that are characters,
accessible through the function ```char```.

- How to handle symbols in LISP?

Two simple ways to transform a symbol name to a string in LISP are to call the
function ```symbol-name``` or the function ```string```. E.g.,

```cl
(symbol-name '_np)
"_NP"

(string '_np)
"_NP"
```

**Note that the result is always uppercase.**
Then you can use the usual sequence handling functions, such as subseq or find, to
manipulate the resulting strings.

```cl
(subseq "abcd" 2 4)
"cd"
```
You can also break a string into a list of characters, using the function ```coerce```:

```cl
(coerce "ABC" 'list)
(#\A #\B #\C)
```

Then you can apply character equality checks such as ```char-equal``` and ```char=```.
(```eq```, ```eql```, and ```equal``` will also work.) And you can put characters back
together into a string by again using the ```coerce``` function:

```cl
(coerce '(#\A #\B #\C) 'string)
"ABC"
```
- **Variables** in Common Lisp are _naming convention_ and we agreed to start with question mark.

- When *match* succeeds, the variables, if any, become _associated_ with those parts
of the data that variable elements match.

- **single variables** (which take the form `?x`) match a single word.

- **segment variables** - variables in any position that match a sequence of items (phrases)
  in the input. We choose a list of the form `(?* ?variable)` to noted them.

The notation `(?* ?x)` denotes a _sequence of zero or more alphanumerical characters_ in
the user input bound to the variable **x**. There is a long tradition in symbolic AI to
start variable names with the **?** character. Thus, the pattern ```((?* ?x) hello (?* ?y))```
operationally means the following:

**Match zero or more alphanumerical characters and place the string into the variable X,
swallow all white space, match "hello" exactly, swallow all white space, match a zero or
more alphanumerical characters and place the match into the variable Y.**

You can think of `(?* ?x)` and `(?* ?y)` as two different _groups_ with the variables X and Y
acting as backreferences.

**NOTE:** _Backreferences_ enable the programmer to refer **back** to the saved matching strings.

The responses(output patterns) are just strings that should be printed out. If a response
contains a variable, it means that its matching binding from the pattern should be
substituted into the response before the response is printed.

Examples:
```cl
((?P MR HULOT AND I) (?X A VACATION))
((?X WHAT HE IS) (?Y FOOD))
```

- A **rule** is a data structure that consist of a pattern and a set of responses.<br/>
For example:

```
RULE:
    PATTERN: 'X I want Y'
    RESPONSES:
    {'What would it mean if you got Y',
     'Why do you want Y'
     'Suppose you got Y soon'}
 ```

- After list of bindings is found there is still two important problems:
  * What if multiple rules' patterns match?
  * How is a specific response is chosen within matched rule?

- Let's summarize:

The variable names contained in an input pattern should be the same as those in the
corresponding output pattern, and each segment variable `(?* ?x)` in an input
pattern corresponds to the **single** variable `?x` in the output pattern.

- How to recognize when there is an existing function that will do a large part the task
  at hand? _See p. 877_

- How to name constants?

`defconstant` is used to indicate that these values will not change. It is
customary to give special variables names beginning and ending with asterisks, but this
convention usually is not followed for constants. The reasoning is that asterisks
shout out, _"Careful! I may be changed by something outside of this lexical scope."_
Constants, of course, will not be changed.

**STYLE**: As an option you can use plus to name _constants_ - ```(+const_name+ ...)```.

- How ```segment-match``` works?

As usual start with specialize and use and example:
```
(pat-match '((?* ?p) need (?* ?x)) '(Mr Hulot and I need a vacation))
((?P MR HULOT AND I) (?X A VACATION))
```

We have to match the pattern `((?* var) . pat)` against input so what is the `var`
and `pat`?

```cl
(var (second (first pattern)))
(pat (rest pattern))
```

Enter the problem as clear the problem. Start from definition:
***What is segment-variable?*** - Variables in any position that match a _sequence of_
_items (phrases)_ in the input.

AHA! _The important question is how much of the input the segment variable should match._

Using example - everything before `need` should be matched by ```(?* ?p)```, everything
after by ```(?* ?x)```. Hmm, we could use `need` _position_ to build algorithm then!
Before to find position word `need` should exist in the input of course.

1. If can't find the word after segment variable - `fail`.
2. If word is found we have to continue from there

* find the position
```cl
(let (pos (position (first pat) input
                    :start start :test #'equal)))
```
* what means - _"Continue(do the same) from there?"_

AHA! I have to do the same so recursive call comes naturally.</br>
Let's call results after next word `b2`
```cl
(let (b2 (pat-match pat (subseq input pos)
                    (match-variable var (subseq input 0 pos)
                                    bindings))))
```
Let's analyze.

Rest of the input is given by: ```(subseq input pos)``` - from `post` to the end.
Next we have to pass the bindings up to the `need` - pass the input with ```(subseq input
0 pos)```. Now we have to construct the whole picture after recursion. `b2` contains
possible bindings or `fail`.

**There is always two cases**

3. If `b2` succeeds it is easy case - just use returned bindings:
```cl
(match-variable var (subseq input 0 pos) b2)
```
4. What if `b2` fails?
Specialize! Illustrate with an example.

```
(pat-match '((?* ?p) need (?* ?x))
           '(Mr Hulot and I and our children need a vacation))
```
We have to expand the region that ```(?* ?p)``` matches - include
```and our children```. AHA! We will retry with next word up to the end
or word that is in pattern and input. To do that ```&optional (start 0)``` is added
and this case is handled by ```(segment-match pattern input bindings (+ pos 1))```.

Of course _termination_ should be considered and we check for end of the pattern first.

```cl
(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (second (first pattern)))
  (pat (rest pattern)))

    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))

          (if (null pos)
              fail
              (let ((b2 (pat-match pat (subseq input pos) bindings)))
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    (match-variable var (subseq input 0 pos) b2))))))))
```
_Always doubt - be sure that solution is solid._

Is it possible to have two (segment)variables in a pattern? The answer is **yes**,
but they should match the same thing. For example:

```cl
(pat-match '((?* ?x ) is next to (?* ?x)) '(book is next to book))
((?X BOOK))
(pat-match '(?x is next to ?x) '(book is next to book))
((?X . BOOK))
```

It is possible to create an illusion and first variable to be bound too "early".
```cl
(pat-match  '((?* ?x) a b (?* ?x))  '(1 2 a b a b 1 2 a b))
```

Our algorithm will bound `X` to `(1 2)` and that will be wrong because next `X` will
be bound to `(a b)` and match will fail. This is a common parsing problem. If first
binding "waits" for longer sequence in our case `(1 2 a b)` match will succeed.
_How to do that?_ We could try for the _rest_ of the pattern with longer and longer
bindings. AHA! We have to change `b2` assignment:

```cl
(let ((b2 (pat-match
           pat (subseq input pos)
           (match-variable var (subseq input 0 pos)
                           bindings))) ....)
```
Then after recursion if `b2` succeed we return just `b2` because it is the solution.
```
PAIP> (pat-match  '((?* ?x) a b (?* ?x))  '(1 2 a b a b 1 2 a b))
  0: (MATCH-VARIABLE ?X (1 2) ((T . T)))
    1: (EXTEND-BINDINGS ?X (1 2) ((T . T)))
    1: EXTEND-BINDINGS returned ((?X 1 2))
  0: MATCH-VARIABLE returned ((?X 1 2))
  0: (MATCH-VARIABLE ?X (A B 1 2 A B) ((?X 1 2)))  ; <-- fails with longer one
  0: MATCH-VARIABLE returned NIL
  0: (MATCH-VARIABLE ?X (1 2 A B) ((T . T)))
    1: (EXTEND-BINDINGS ?X (1 2 A B) ((T . T)))
    1: EXTEND-BINDINGS returned ((?X 1 2 A B))
  0: MATCH-VARIABLE returned ((?X 1 2 A B))
  0: (MATCH-VARIABLE ?X (1 2 A B) ((?X 1 2 A B)))
  0: MATCH-VARIABLE returned ((?X 1 2 A B))

((?X 1 2 A B))
```
- Here is ELIZA algorithm:

```
while (true) {
    input = get_input_from_the_user()
    applicable_rules = find_applicable_rules(input, rule_database)
    chosen_rule = choose_applicable_rule(applicable_rules)

    // form response
    chosen_responses = choose_response(chosen_rule)
    chosen_responses = substitute_matches(chosen_rule)
    print_response(chosen_responses)
```
The procedure is to look for specific patterns, based on a key word
or words in the input.
