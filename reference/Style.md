## Lisp Style

Read also **25.15 A Style guide to Lisp** on p. 887

#### Widely used "need-to-know" conventions

| Example           | Description                                         |
| :---------------- | :--------------------------------                   |
| foo-bar           | "-" is used as a word delimiter                     |
| *foo*             | (global) special variable                           |
| foo*              | slightly different variant of the foo operator      |
| &foo              | lambda list keyword. These symbols will be in       |
|                   | the lambda-list-keywords list.                      |
| nfoo              | (possibly) destructive (non-consing) function       |
| foop              | predicate (also foo-p, be consistent)               |
| foof              | place changing (like in *SETF*, *INCF*, ...)        |
| +foo+             | constant, or single CLOS instance                   |
| %foo              | low-level, fast, dangerous function, or Lisp system |
|                   | specific implementation of foo                      |
| make-foo          | create a foo and return it                          |
| define-foo        | (globally) define a new foo                         |
| with-foo          | create a dynamic context with a foo                 |
| do-foo            | iterate through a foo                               |
| foo-case          | foo-specific case expression                        |
| foo-bar           | type-slot, converting FOO to BAR                    |
| foo-to-bar        | converting FOO to BAR                               |
| <class-name>      | Surround class name with "<" and ">"                |

#### Comments

**80 column** maximum width

;  for inline comment<br/>
;;  for in function comment<br/>
;;;   for between function comment<br/>
;;;;   for section header

#### Multi-Line Strings

In case of a multi-line string as a literal constant consider
instead using read-time evaluation and a call to format:
``` cl
(defclass document () ()
  (:documentation #.(format nil "Hey look, this is a ~
                                 very nice multi-line string ~
                                 and a haiku too.")))
```

#### Naming Functions And Variables

- Functions are usually actions. Therefore name them accordingly with a verb first then an
  object:
`
    generate-first-deck
    pick-letter
    begin-game
`
Some functions return a boolean. In this case put the subject first and the verb next:
`
    word-exist
    number-is-even
    file-lock
`
This is so that conditional statements resemble English:
`
    (if (word-exist
    (when (not file-lock
`
**Avoid double negative like the pest!**

- Name variables as explicitly as possible, mostly use two word when one is not sufficient.
- Never put and **'s'** at the end, it's a source of typos.
- Don't abbreviate character to **char** or index to **i**. What you gain in typing is lost
in refactoring or code reading. Here are some variables examples:
`
    time-left
    bad-word
    deck-letter
`

#### Class

Add `:type` to each slots

``` cl
(defclass <aluminium> (<metal>)
    ((color :type string
            :initarg :color
            :initform "white")
     (solidity :type (or integer <solidity>)
               :initarg :solidity
               :initform (make-instance '<solidity>))
     (cost :type (or integer null)
           :initarg :cost))
  (:documentation "A class represents Aluminium."))
```

Don't forget a type `null` for optional slots.
