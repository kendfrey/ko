# Ko

This is intended to be an informal and non-exhaustive specification of the Ko language.

## Table of Contents

- TODO

## Overview

Ko is an imperative language, where a program is a sequence of commands. The internal state of a Ko program consists of a program counter and a [Go](https://en.wikipedia.org/wiki/Go_(game)) board, that is, a m×n rectangular matrix where each element of the matrix is either an empty intersection, a black stone, or a white stone. The program interacts with the board by placing black and white stones on empty intersections. The program cannot directly remove stones from the board.

Stones played on the board follow the basic rules of Go. Every "chain" of one or more orthogonally connected stones of a single colour must always have at least one "liberty", that is, an adjacent empty intersection. Stones on the edge of the board do not get liberties from the space outside the board. If a chain is surrounded by stones of a different colour so that it has no liberties, that chain is captured and its stones are removed from the board.

A stone cannot be played in a location that would cause it to be immediately captured. This is a "suicide" move. The exception to this rule is when the played stone captures an opponent's chain.

During its execution, a program may attempt a variety of moves.

- Playing a stone where a stone of the same colour already exists has no effect.
- Playing a stone where a stone of the opposing colour already exists causes the program to terminate with an error.
- Playing a stone in suicide causes the program to terminate with an error.
- Playing a stone in apparent suicide is a legal move if it captures an opponent's chain.
- Playing a move outside the bounds of the board causes the program to terminate with an error.
- Playing a stone on any empty intersection that is not suicide is a legal move.
- Unlike the actual game, there is no ko rule (somewhat ironically); any stone may be captured at any point.
- Multiple stones of the same colour may be played consecutively; black and white are not required to alternate turns.

Likewise, a program may read from the board.

- Reading from outside the bounds of the board causes the program to terminate with an error.
- Reading from anywhere on the board returns the bit 1 if any stone is on the intersection, and the bit 0 otherwise.

### Execution

A program begins with an empty board. Execution starts at the first command, and executes commands sequentially thereafter. Some commands may cause execution to jump to other locations in the program. If execution passes the last command in the program, the program terminates with an error. The only legal way to terminate a program is with the `pass` command.

### Program Syntax

A Ko program consists of zero or more statements.

```bnf
<program> ::= <statement>*
```

A statement is either a pragma or a command, and is terminated by a semicolon.

```bnf
<statement> ::= (<pragma> | <labelled-command>) ";"
```

A pragma is a statement that cannot be executed, but controls the interpretation of the program. A command is a statement that can be executed to affect the internal state of the program.

Each statement begins with the name of the pragma or command, followed by zero or more arguments. Pragma names begin with an underscore. All statement names are in lowercase.

Commands may optionally be preceded by a label, which can be referenced by flow control commands elsewhere in the program.

```bnf
<labelled-command> ::= [":" <identifier>] <command>
```

## Commands

### `goto`

The `goto` command unconditionally jumps execution to another location, specified by a label.

```bnf
<goto-command> ::= "goto" <identifier>
```

#### Arguments

- label - identifier

#### Example

```
:loop
goto loop; # Loop forever
```

---

### `pass`

The `pass` command terminates the program.

```bnf
<pass-command> ::= "pass"
```

#### Arguments

- none

#### Example

```
pass;
```

This is analogous to both players passing consecutively to end a game of Go.

---

### `black`/`white`

The `black` and `white` commands play black and white stones respectively.

```bnf
<black-command> ::= "black" <pointer-expression>
<white-command> ::= "white" <pointer-expression>
```

#### Arguments

- ptr - pointer

#### Example

```
black [4,4]; # Play a black stone at (4,4)
white <10,20,20,2,1>; # Play 10 white stones at (20,20), (22,21), (24,22), ... (38,29)
```

When playing multiple stones at once using a vector pointer, each stone is played as its own move, with capture and suicide rules applying.

---

### `if`

The `if` command evaluates an expression and jumps execution to another location if the result is nonzero.

```bnf
<if-command> ::= "if" <expression> <identifier>
```

#### Arguments

- condition - expression
- label - identifier

#### Example

```
:loop
if [4,4] loop; # Loop while there is a stone at (4,4)
```

If the expression evaluates to 0, execution continues with the next command.

If the program attempts to jump to a label which is not defined anywhere, the program terminates with an error.

---

### `case`

The `case` command evaluates an expression and jumps execution to another location depending on the result.

```bnf
<case-command> ::= "case" <expression> <identifier>*
```

#### Arguments

- condition - expression
- labels - array of identifiers

#### Example

```
case <2,4,4,1,0> zero one two three;
:zero pass; # (4,4) and (5,4) are empty.
:one pass; # There is a stone at (4,4).
:two pass; # There is a stone at (5,4).
:three pass; # There are stones at (4,4) and (5,4).
```

The labels are treated as a 0-indexed array. If the expression evaluates to a valid index into the array, execution jumps to the corresponding label. Otherwise, execution continues with the next command.

If the program attempts to jump to a label which is not defined anywhere, the program terminates with an error.

---

### `copy`

The `copy` command copies ones or more stones on the board by playing stones of the appropriate colour.

```bnf
<copy-command> ::= "copy" <pointer-expression> <pointer-expression>
```

#### Arguments

- from - pointer
- to - pointer

#### Example

```
copy [4,4] [3,3]; # Repeat the last move at (4,4) by playing at (3,3)
```

A black stone is copied by playing another black stone, and similarly for white. Copying an empty intersection has no effect.

When playing multiple stones at once using a vector pointer, each stone is played as its own move, with capture and suicide rules applying.

When copying from one vector pointer to another, the first position of the "from" vector is repeated at the first position of the "to" vector, the second position is repeated at the second position, and so on.

When the sizes of the pointers do not match, the number of positions copied is the smaller of the two.

---

## Pragmas

### `_size`

The `_size` pragma sets the size of the board used by the program.

```bnf
<size-pragma> ::= "_size" <number> <number>
```

#### Arguments

- width - number
- height - number

#### Example

```
_size 19 19; # Use a 19×19 board
```

The `_size` pragma cannot occur more than once in a program, and it cannot occur after the first command.

The `_size` pragma is optional. If it is not specified, the program uses a 256×256 board.

---

### `_define`

The `_define` pragma assigns a name to an expression which can be used later in the program.

```bnf
<define-pragma> ::= "_define" <identifier> <expression>
```

#### Arguments

- name - identifier
- expr - expression

#### Example

```
_define tengen [10,10]; # Define "tengen" to be the intersection at (10,10)
```

Expressions are evaluated using the board state at the time of execution. For example, when the above expression `tengen` is used in a command, it uses the state of the (10,10) intersection at the time the command is being executed.

The same name cannot be defined multiple times in a program.

Referencing a name which hasn't been defined results in an error.

Names are *not* directly replaced by their definitions as token sequences:

```
_define a 2-1;
_define b 3-a; # This becomes equivalent to 3-2+1
```

---

## Expressions

TODO

## Pointers

### Cell

TODO

### Vector

TODO

### Arithmetic

TODO

## Lexical Syntax

### Tokens

TODO

### Whitespace & Comments

TODO