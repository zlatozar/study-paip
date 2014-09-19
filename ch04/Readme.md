#### Chapter 4

- General Problem Solver(**GPS**) - a single computer program that could solve any problem,
given a suitable description of the problem.
It was the first program to separate its problem-solving strategy from its knowledge of
particular problems, and it spurred much further research in problem solving.
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
- Common Lisp idiom to assign to variable:
```cl
;; With passed values *state* and *ops* will be initialized
(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (every #'achieve goals) 'solved))
```
- `debug` and `undebug` functions are in ../auxfns.lisp file
