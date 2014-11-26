### Pecan Emacs

This reference assumes that you use [Pecan Emacs](https://github.com/zlatozar/pecan)

### Common default Emacs key prefixes

Key&nbsp;Binding  |  Description
----------  | :-----------
`C-h C-m`   |  Shows available key bindings for current major mode.
`C-c x n`   |  Manage minor mode.
`C-c`       |  Commands particular to the current editing mode.
`C-x`       |  Commands for files and buffers.
`C-h`       |  Help commands.
`M-x`       |  Literal function name.
`C-h b`     |  Show all key bindings.
`C-h m`     |  Show key bindings for current mode.

When you have long combination and you know only first part `C-c .` Then typing `C-c .` and
then `C-h` Emacs will tell you all possibilities.

### Emacs window-manipulation commands

Key&nbsp;Binding  |  Description
----------        | :---------------------------------------
`C-x 4 f`         |  Open a new file in a new buffer, drawing it in a new vertical window.
scroll-all-mode   |  Toggle the `scroll-all` minor mode. When it's on, all windows displaying the buffer in the current window are scrolled simultaneously and in equal, relative amounts.
`C-x 3`           |  Split the current window in half down the middle, stacking the new buffers horizontally.
`follow-mode`     |  Toggle follow, a minor mode. When it's on in a buffer, all windows displaying the buffer are connected into a large virtual window.
`C-x ^`           |  Make the current window taller by a line; preceded by a negative, this makes the current window shorter by a line.
`C-x }`           |  Make the current active window thinner by a single column.
`C-x {`           |  Make the current active window wider by a single column.
`C-x -`           |  Reduce the current active window to the smallest possible size for the buffer it contains.
`C-x +`           |  Balance the size of all windows, making them approximately equal.
compare&#8209;windows |  Compare the current window with the next window, beginning with point in both windows and  moving point in both buffers to the first character that differs until reaching the end of the buffer.

### Emacs text manipulation commands

Key&nbsp;Binding   |    Emacs&nbsp;Function       |  Description
----------  | ---------                  |          :-----------
`C-x Tab`   | indent-rigidly             |  Indents lines in the region (or at point).
`undefined` | `fill-region`              |  Fills all paragraphs in the region.
`M-q`       | fill-paragraph             |  Fills the single paragraph at point.
`M-\`       | delete&#8209;horizontal&#8209;space   | Removes any horizontal space to the right and left of point.
`M-t`       | transpose-words            |  Transposes the single words to the right and left of point.
`C-x C-t`   | transpose-lines            |  Transposes the line at point with the line before it.
`M-u`       | uppercase-word             |  Converts the text at point to the end of the word to uppercase letters.
`M-l`       | downcase-word              |  Converts the text at point to the end of the word to lowercase letters.
`C-x C-l`   | downcase-region            |  Converts the region to lowercase letters.
`C-x C-u`   | upcase-region              |  Converts the region to uppercase letters.
`M-m`       | back-to-indentation        |  This command, given anywhere on a line, positions point at the first non blank character on the line.
`M-z`       | zap-to-car                 |  Deletes everything to given character **including**.
`M-Z`       |                            |  Deletes everything to given character **excluding**.
`C-0 C-k`   |                            |  Delete from point to beginning of line.
`C-s C-w`   |                            |  This command, add the (rest of the) word at the pointer for `isearch`.
`C-c SPC`   | ace-jump-mode              |  Enabling fast/direct cursor movement in current view.
`qj`        | jump-char-forward          |  Navigate by char.
`C-c x i`   | my/ispell&#8209;word&#8209;then&#8209;abbrev |  Calls `ispell-word` then create `abbrev`.
`qd`        | duplicate-thing            |  Duplicate line.
`C-c n l`   |                            |  Show line numbers.
`C-c n r`   |                            |  Show **relative** line numbers.
`C-c C-i`   | ido-imenu                  |  Find method definition in the current buffer.
`C-c n d`   | dedicated mode             |  Pin buffer - `dedicated-mode`.
`qp`        | helm-browse-project        |  Find file in a `.git` project.
`F5`        | neotree-toggle             |  File browser.
`C-c p f`   | cleanup-buffer             |  Align file, clean up white spaces and remove trailing spaces.
`C-c C-z`   |                            |  Switch to **REPL**.
`C-c ;`     | iedit                      |  Edit same words (then `'M-H'` to narrow to function).
`C-c x .`   | goto-last-change           |  Go to the last change.


### Paragraphs and Regions

Key&nbsp;Binding   |    Emacs&nbsp;Function       |  Description
----------  | --------            |                :-----------
`M-h`         | mark-paragraph      |  Mark select paragraph.
`C-x n n`     | narrow-to-region    |  Narrow buffer to the current region:
`C-x n w`     |                     |  Restore ("widen") buffer.

### Emacs commands for using registers

Emacs registers are general-purpose storage mechanisms that can store one of many things,
including text, a rectangle, a position in a buffer, or some other value or setting. Every
register has a label, which is a single character that you use to reference it. A register
can be redefined, but it can contain only one thing at a time.  Once you exit Emacs, all
registers are cleared.

Key&nbsp;Binding   |    Emacs&nbsp;Function       |  Description
----------    | --------            |                :-----------
`C-x r space X` | point-to-register   | Save point to register X.
`C-x r s X`     | copy-to-register    | Save the region to register X.
`undefined`     | view-register       | View the contents of a given register.
`C-x r j X`     | jump-to-register    | Move point to the location given in register X.
`C-x r i X`     | insert-register     | Insert the contents of register X at point.


### Macro Commands

Key&nbsp;Binding   |    Emacs&nbsp;Function       |  Description
----------     |      --------               |     :-----------
`C-x (`          | start-kbd-macro             | Start a new macro definition.
`C-x )`          | end-kbd-macro               | End the current macro definition.
`undefined`      | name-last-kbd-macro         | Name the last macro before saving it then you can execute it using `M-x <your-macro-name>`.
`undefined`      | apply&#8209;macro&#8209;to&#8209;region&#8209;lines | Apply macro to selected region.

### Emacs commands for using rectangles (cua-selection-mode)

_(to be added)_

### (Lisp) Programming

_(to be added)_

### Find and replace recursively

_(to be added)_

### Useful reference

[Emacs Tutorials](http://www.ibm.com/developerworks/views/aix/libraryview.jsp?search_by=emacs+editing+environment)

[Emacs Key Bindings](http://www.cb1.com/~john/computing/emacs/beyond-tutorial.html)
