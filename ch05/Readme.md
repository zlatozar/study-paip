#### Chapter 5

- Pattern matching

Pattern matching is the process of comparing symbolic expressions to see if one is
similar to another.

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

- **Variables** in Common Lisp are naming convention and starts with question mark.

- When *match* succeeds, the variables, if any, become associated with those parts
of the data that variable elements match.

- Good example of **conditional consing** (adding):
``` cl
(defun extend-bindings (var val bindings)
  (cons (cons var val)
        (if (eq bindings no-bindings)
            nil
            bindings)))
```
- **single variables** (which take the form `?x`) match a single word.

- **segment variables** - variables in any position that match a sequence of items (phrases)
  in the input. We choose a list of the form `(?* ?variable)` to noted them.

- **output pattern** is the result of match. This variables are only single variables.

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
pattern corresponds to the single variable `?x` in the output pattern.

In each rule, the first element is the pattern and the rest are responses. For example:<br/>
`((?* ?x) hello (?* ?y))`<br/>
is a pattern and<br/>
`(How do you do. Please state your problem.)` is the only response.

Similarly, `((?* ?x) computer (?* ?y))` is a pattern and there are four responses
are:
```
1. (Do computers worry you?);
2. (What do you think about machines?);
3. (Why do you mention computers?);
4. (What do you think machines have to do with your problem?).
```
The notation `(?* ?x)` denotes a _sequence of zero or more alphanumerical characters_ in
the user input bound to the variable **x**. There is a long tradition in symbolic AI to
start variable names with the **?** character. Thus, the pattern `((?* ?x) hello (?* ?y))`
operationally means the following: **match zero or more alphanumerical characters and
place the string into the variable x, swallow all white space, match "hello" exactly,
swallow all white space, match a zero or more alphanumerical characters and place the
match into the variable y.**

You can think of `(?* ?x)` and `(?* ?y)` as two different groups with the variables x and y
acting as backreferences.

**NOTE:** _Backreferences_ enable the programmer to refer back to the saved matching strings.

The responses are just strings that should be printed out. If a response contains a
variable, it means that its matching binding from the pattern should be substituted into
the response before the response is printed.

- How to recognize when there is an existing function that will do a large part the task
  at hand? _See p. 877_
- How to name constants?

`defconstant` is used to indicate that these values will not change. It is
customary to give special variables names beginning and ending with asterisks, but this
convention usually is not followed for constants. The reasoning is that asterisks
shout out, _"Careful! I may be changed by something outside of this lexical scope."_
Constants, of course, will not be changed.

**STYLE**: As an option you can use plus to name constants - **+const_name+**.
