#### Chapter 10

What happens when you already are using the best imaginable algorithms, and performance is
still a problem? One answer is to find what parts of the program are used most frequently
and make micro-optimizations to those parts. This chapter covers the following six
optimization techniques.

    * Use declarations.
    * Avoid generic functions.
    * Avoid complex argument lists.
    * Provide compiler macros.
    * Avoid unnecessary consing.
    * Use the right data structure.

## Use declarations

On general-purpose computers running Lisp, much time is spent on type-checking. You can
gain efficiency at the cost of robustness by declaring, or _promising_, that certain
variables will always be of a given type.

You should declare a function ```inline``` when it is short and the function-calling
overhead will thus be a significant part of the total execution time. You should not
declare a function ```inline``` when the function is recursive, when its definition is
likely to change, or when the function's definition is long and it is called from many
places.

## Avoid generic functions

- Avoid Complex Argument Lists
