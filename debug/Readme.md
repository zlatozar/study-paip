#### Debugging in Lisp

_(Sorry, I can't remember from where I took it)_

The idea of restarting a Lisp program to test a fix or find a bug is as _alien_ to most
Lisp programmers. It's just a different development cycle.

In the programming languages most people are used to, there's a cycle
something like this:

- Edit the source code
- Compile it
- Run a test case
- Note the most obvious bug
- Run the debugger
- Set a breakpoint in the region of the bug
- Examine the program's state around the breakpoint
- Think of a solution
- Return to step 1

*Why does this cycle occur?* Because in most languages, a program must be complete before
it can be compiled, and compiled before it can be run, and must run subroutines in the
natural order of running. This cycle is pretty much mandated by these prerequisites.
This is reflected in the tool structure: the editor, compiler, execution shell, and
debugger are all separate programs.

In Lisp, things are different. In Lisp, you're sitting "within" the running program at all
times. The execution shell is the Lisp REPL. The debugger is always present, and takes
control whenever there's an unhandled exception.  The compiler is just part of your
execution shell. And the shell has access to all of your subroutines. The only thing
that's not necessarily integrated is the editor, and most Lisp coders use editors that
integrate themselves with Lisp; usually, this is SLIME under Emacs, or an editor built
into the vendor's Lisp.

Let's suppose you had the following simple program to test numbers that a user enters, and
tell whether they're divisible by 10. It has an obvious bug.

``` cl
(defun divisible-by-10-p (number)
  (zerop (mod number 11))) ; <-- bug

(defun list-multiples-of-10 ()
  (loop
   for number = (progn (print "Enter a number, or NIL to exit:") (read))
   while number
   when (divisible-by-10-p number)
   do (print "It's divisible by 10!")))
```

Now, as I said, this has an obvious bug: **it lists numbers that are divisible by 11
instead of 10**.

Let's suppose you've loaded this into your Lisp. When you run it, the code prints some
numbers that aren't right. **You want to set a breakpoint**. You don't need to restart
your test case; you don't even need to stop your main loop. _Just leave it running as it
is_.  **Remember, in Lisp, you're inside the development system, rather than outside of
it.**

Now, in your editor, you go exactly where you want the breakpoint to be.  Add a (break):
``` cl
(defun divisible-by-10-p (number)
  (break)
  (zerop (mod number 11)))
```
(Don't be afraid to add `(break "foo=~s" foo)` and similar calls to the code you're
debugging.)

In most Lisp editors like Emacs , you can give it an _"evaluate defun"_ command of some
sort.  In SLIME, I press "C-c C-c" while I'm in the defun.  Again, in most Lisp editors,
this will happen while you're still in the main loop.  You'll still be in the main loop,
right where you left off. Enter new number and voila you enter breakpoint.

Here, I used the example of adding a **break**, although as many people mentioned, most
Lisp environments have the ability to turn on a **break-on-entry** without you needing to
modify your code.  But to fix the bug, you will need to edit the code, and the same thing
applies there.

You can see how this would scale very well. For example, the main loop could be a large
program's GUI event handler. You may have gone through several steps to get the bug's
reproduction scenario set up. Being able to modify your code and test it while your
program is still running may be very, very valuable. It's valuable in production programs
too: the same idea to load patches into a running server without needing to take 30
minutes to repopulate its data.

Let's suppose you don't have a Lisp editor.  Maybe you haven't learned Emacs or your
vendor's editor yet, and running your Lisp from a command prompt.  That's okay too.  You
can press a break key (usually **Ctr-C**) while your program is running, and you'll get a
debugger prompt.  From there, you can either load your modified source file, or if the new
function is short, just type it in by hand.  Then tell the debugger to continue where it
left off.

If you want, then at the debugger prompt, you can call your inner function by itself, such
as `(divisible-by-10-p 10)`.  You can look at what it does, and call it again and again as
you refine it.  If you need variables, such as complex data structures, that your program
maintains while it's running, they're all available.  All your subroutines are available.
In this respect, it's similar to **gdb**'s "call" and "print" commands, with one important
difference: you can edit your function while you're testing.

Eventually, either because you think you've got things working or because you don't feel
like testing your function in the debugger, you tell the debugger to continue.  Your
program picks up right where it left off, and you can try the errant command again (in
this case, submit test numbers like "10" and "11" back to the loop's prompt).

Remember, this entire debugging session:

- editing
- tracing
- debugging
- correcting
- testing

took place while your program was still running.  You don't need separate tools for these
steps that run outside of your program; they're integrated with the environment your
program is running in.  And once you've tested your addition, you can compile it with
`(compile 'divisible-by-10-p)` if you want to. Or just leave it interpreted while you go
debug another part of your program, still within the same session.

In real life, I rarely use the debugger like this, though.  Usually, I'll test an inner
function directly from the REPL.  If it fails, I hypothesize about the problematic
function and call it directly, or possibly turn on some **traces** instead.  Since you
have all of your program's subroutines and variables at the Lisp REPL, you can do one-shot
tests from the prompt, without having to write and compile tests.

In Lisp, though, things are different. Since your entire program is available, you can
have it load up its data in the normal way - by interacting with the user, or reading its
data files, or whatever.  Then you can call small functions individually, editing their
definitions as you go, and check the results. You don't need to write specialized printers
for your test cases, though: _Lisp has a built-in customizable printer for most data
structures, DESCRIBE can get you through the rest._

Once you've got your program's data loaded, you can go wild with development. It's not
uncommon for me to have a session that lasts a week, debugging existing code and writing
new code, all without ever restarting my program.

There's a lot of things available to make debugging easy in Lisp that aren't so easy in
other languages. *But the key concept, and this is one that takes time to get used to, is
that the development cycle is different*.  When you're at the REPL, you're immersed in
both your program and all of Lisp's tools.  In a very real sense, you're within your
program.  If you think in these terms - and it's hard at first, after years of
*ALGOL*-descended conditioning - then Lisp debugging will make a lot more sense.

#### SLIME helpful commands

Example of debugging session could be found [here](debugging-sbcl.md).

- **C-c C-y** - ```slime-call-defun```<br/>
Type it when you are inside the function. It will sends expression ```(defun <func-name> ...)``` to REPL.<br/>
You have to fill parameters only (if any).

- **C-c M-o** - ```slime-repl-clear-buffer```<br/>
Type it when you are in REPL. It will clear the buffer like **C-l** in shell.

- **C-c E** - ```slime-edit-value```<br/>
When there is a error - go to the frame and type 'v' to see buggy code. Next go to the<br/>
particular value in a function and type **C-c E** to change it. Finally **C-M-x** to re-evaluate _defun_.

- **C-c ~** -  ```slime-sync-package-and-default-directory```
- **C-u C-c C-c** - compile function for debug
- **C-u C-c C-k** - compile file for debug
- **M-- C-c C-k** - compile file for speed
- **C-c C-d C-d** - describe symbol at point
- **C-c C-d h** - HyperSpec look up
- **C-c C-b** - ```slime-interrupt```

- **C-x C-e** - ```slime-eval-last-expression```<br/>
Evaluates form immediately preceding cursor

- **C-c C-m** - ```slime-macroexpand-1``` <br/>
Place the cursor on the opening parenthesis of a macro form in your source code.

- **M-.** - jumps to source definition (try it from: Debugger(SLDB), REPL and source file)<br/>
Did you notice that when compilation error occurs, SLIME do not show the file and the line?<br/>
Well, go to the last green line in SLDB and type **M-.** and you will be in the right<br/>
file right function. Of course use **M-,** to return.

- **C-c <** - ```slime-list-callers```
- **C-c >** - ```slime-list-callees```

- Examining Frames in SLDB
  * **t** toggle details
  * **v** show source (very helpful)
  * **e** eval in frame
  * **i** inspect in frame (you can go deeper and deeper with pressing 'Enter')

#### Is there a way to reset the state of the environment?

There is no way without restarting the process.
In SLIME you can use **M-x** _slime-restart-inferior-lisp_.

- In *.sbclrc* put following code:
``` cl
;; Add maximum debug information
(sb-ext:restrict-compiler-policy 'debug 3)
```
- For better SBCL command line place the following in **.bashrc**:
``` cl
alias lisp='rlwrap sbcl'
```

#### How Lisp helps (p. 85)

- ```compile```, ```compile-file```, ```load```
- ```describe```, ```documentation```
- ```apropos```
- ```trace```
- ```step``` (**step** _(function-name params)_)
- ```inspect```
- ```assert``` (see example on p. 89)
- ```time```
