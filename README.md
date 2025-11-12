![Formin logo](./Icons/Formin-logo.svg)

# Formin Language 
> A symbolic, flow-based programming language written in Fortran - now delivered as a compiled toolchain for performance and portability.

**Version:** 1.1.5
**Author:** Capinol  
**Toolchain:** `forminc` (compiler) + `forminvm` (virtual machine)

> Looking for the classic interpreter docs? See [`README_interpreter.md`](Info/README_interpreter.md).

---

## Overview

Formin is a **flow-based, label-driven programming language** that uses symbolic instruction syntax for complete control over execution flow.  
Commands follow a consistent structure:

command#/token1|token2|token3/#


Each instruction starts with a **command name**, followed by arguments (called *tokens*) separated by `|`.  
String literals are wrapped in single quotes (`'`), while variables are referenced directly.

Example:
```formin
create#/greeting|'Hello, World!'/#
spew#/greeting/#

```

Source files (`.fmn`) are compiled with `forminc` into compact bytecode (`.fbc`) that is executed by the portable `forminvm` runtime.

---

- [Overview](#overview)
- [Installation](#installation)
    - [Linux/MacOS](#linux--macos)
    - [Windows](#windows-powershell-or-cmd)
    - [Licenses](#license)
- [Command line arguments](#command-line-arguments)
- [Quick start](#quick-start)
- [Examples](#examples)
- [Commands](#commands)
  - [Format](#format)
  - [Execution](#execution)
  - [Variables](#variables)
  - [Arithmetic](#arithmetic)
  - [File Handling](#file-handling)
  - [Strings](#strings)
  - [Lists](#lists)
  - [Suffixes](#suffixes)
- [Related links](#related-links)

---

## Installation

Formin now ships as a **compiled toolchain** consisting of:
- `forminc` – the ahead-of-time compiler that emits `.fbc` bytecode
- `forminvm` – the lightweight virtual machine that executes the compiled program

Download the pair from the [Releases page](https://github.com/CapinolDev/formin/releases) and place them somewhere on your `PATH`.

---

### Linux / macOS
1. Download the latest release and extract **`forminc`** and **`forminvm`**.
2. Make them executable:
   ```bash
   chmod +x forminc forminvm
   ```
3. Move both to a system path:
   ```bash
   sudo mv forminc forminvm /usr/local/bin/
   ```
4. Compile and run a sample:
   ```bash
   forminc Examples/helloWorld.fmn
   forminvm Examples/helloWorld.fmn.fbc
   ```

### Windows (PowerShell or CMD)
1. Download the latest release of **`forminc.exe`** and **`forminvm.exe`**.
2. Optional: add their folder to `PATH`, or keep them next to your project.
3. Compile and run:
   ```powershell
   .\forminc.exe Examples\helloWorld.fmn
   .\forminvm.exe Examples\helloWorld.fmn.fbc
   ```
If Windows blocks the binaries, right-click → Properties → Unblock, or run PowerShell as Administrator.

---
## Command line arguments

### `forminc`
- `forminc source.fmn` – compiles a source file into `source.fmn.fbc`.

### `forminvm`
- `forminvm ver` – prints the VM version.
- `forminvm program.fbc` – runs a compiled program.
- `forminvm program.fbc loud` – runs the program and prints total execution time.

## Quick Start

1. Create a new file called `hello.fmn`:
   ```formin
   create#/name|'World'/#
   spew#/'Hello,'|name|!/#
   ```
2. Compile it to bytecode:
   ```bash
   forminc hello.fmn
   ```
3. Run the compiled program:
   ```bash
   forminvm hello.fmn.fbc
   ```
   Output: `Hello, World!`

###

---

## Examples
- [Hello World](Examples/helloWorld.fmn)
- [Simple calculator](Examples/simpleCalc.fmn)
- [Greet user](Examples/userGreet.fmn)
---

### License
Formin binaries are provided under the MIT License.
Logo © Capinol 2025, licensed under CC BY-SA 4.0.
---

## Commands:

If you want to put comments, see [this](#any-other-suffix-comment)

### Format
#### spew#//# 
prints things, supports multiple things at once, automatically adds space between them.

Example: ``` spew#/'Hello'|'World!/# ```

#### spewmult#//#
prints things, supports multiple things at once. Unline spew, it doesn't add space between each token printed.

Example: ``` spewmult#/'Hello'|'World!/# ```

#### color#//#
sets the print color to the given color.
Accepted colors: reset, black, red, green, yellow, blue, magenta, cyan, white, bright_red, bright_green, bright_yellow, bright_blue, bright_magenta, bright_cyan, bright_white
Only works on modern terminals.

Example: ``` color#/green/# ```

#### clear#//#
clears the terminal, same as 'clear' on UNIX and 'cls' on Win.
Example: ``` clear#//# ```

### Execution

#### bye
ends the program.
Example: ``` bye ```

#### mark#//#
sets a marker at given line of program, can be returned to with go#//#
Example: (infinite loop)

```formin
mark#/loop/#
spew#/'You are in the loop!'/#
go#/loop/#
```
#### go#//#
goes to a marker.
Example: in mark#//#

#### goback#//#
goes back to the last go#//# call
Example: (prints 1,2,3)
```formin
spew#/'1'/#
go#/print2/#
go#/print3/#
bye

mark#/print2/#
spew#/'2'/#
goback#//#

mark#/print3/#
spew#/'3'/#
goback#//#
```

#### getos#//#
gets the users os (Either win or unix) and saves to a var
[Example](Examples/getOs.fmn)

#### sys#//#
writes the input into the terminal/powershell of the computer running it.  
Pair it with [`ins`](#ins) to pipe scripted stdin into interactive programs.
Example (unix): ``` sys#/'ps aux'/# ```

#### cputime#//#
gets the current cpu time since start of program and writes it in a variable.
Example:
```formin

cputime#/startVar/#
... code in between ...
cputime#/endVar/#
sub#/execTime|endVar|startVar/#
spew#/Execution time:|execTime/#
```
### Variables

#### create#//#
makes a variable and sets it to a value.
Has 3 types: int, real, string
Example: ``` create#/x|10|int/# ```

#### set#//#
sets an existing variable to a different value/type
Example: ``` set#/x|'a'|str/#

#### ask#//#
Asks the user a question, and stores it in a variable
Example: 
```formin
    ask#/'What is your name?'|name/#
    spew#/'Hello'|name/#
```

#### ins#//#
Buffers lines that will be piped into the next `sys` command. Each token becomes one stdin line, letting you interact with spawned programs (like another `forminvm` run) without manual typing.
Example:
```formin
    ins#/'1'|'5'|'7'/#
    sys#/'forminvm Examples/simpleCalc.fmn.fbc'/#
```

#### ifgo#//#
Goes to given line if condition is met, and to a different line if it isnt met.
Format: ``` ifgo#/*x*|*comparer*|*y*|*go1*|*go2*/# ```
Format explanation:
x: first thing to compare
comparer: how to compare them
``` 
is
isnt
<
>
>=
<=
```
y: second thing to compare
go1: marker to go to if condition is met - can be _ to ignore
go2: marker to go to if condition isnt met - can be _ to ignore
Example: 
```formin
ask#/'Enter a number:'|number/#
ifgo#/x|is|6|isSix|isntSix/#

mark#/isSix/#
spew#/'Your number is six!'/#
bye

mark#/isntSix/#
spew#/'Your number isnt six! '/#
bye

``` 

#### type#//#
gets the type of a var and stores it in another var.
[Example](Examples/type.fmn)

#### randi#//#
gets a random value from (0 to 1)*userMod and saves it in a var.
Syntax: ```formin randi#/*var to store output*|*userMod*/# ```
[Example](Examples/numberGuess.fmn)


### Arithmetic
#### add#//#
Adds two numbers (can be variable), and stores the result in a given variable
Syntax: ``` add#/*variable to store result*|*first num*|*second num*/# ```
Example: (add 1 to x) 
``` add#/x|x|1/# ```

#### sub#//#
Substracts number 1 by number 2 (can be variable), and stores the result in a given variable
Syntax: ``` sub #/*variable to store result*|*first num*|*second num*/# ```
Example: (sub x by 1) 
``` sub#/x|x|1/# ```

#### mult#//#
Multiplies two numbers (can be variable), and stores the result in a given variable
Syntax: ``` mult#/*variable to store result*|*first num*|*second num*/# ```
Example: (mult x by 2) 
``` mult#/x|x|2/# ```

#### div#//#
Divides number 1 by number 2 (can be variable), and stores the result in a given variable
Syntax: ``` sub#/*variable to store result*|*first num*|*second num*/# ```
Example: (Divide x by 2) 
``` div#/x|x|2/# ```
Watch out for divison by zero!

#### mod#//#
Gets the remainder of a number after division
[Example](Examples/mod.fmn)

#### pow#//#
Gets the power of first variable to power of second variable
[Example](Examples/pow.fmn)

#### sqrt#//#
Gets the square root of a number.
Example: ``` sqrt#/x|16/# ``` > x is 4

#### abs#//#
Gets the absolute value of a number.
[Example](Examples/abs.fmn)

#### floor#//#, ceiling#//#
Gets the floor/ceiling of a number.
[Example](Examples/round.fmn)


### File handling

#### open#//#
Opens a file, creates new one if supplied file doesn't exist.
Syntax: ``` open#/*how the file be reffered as*| *filepath*/# ```
Example: ```open#/fileHello|hello.txt/# ```
#### read#//#
Reads a line in supplied file, then advances the line to next one (for later reading)
Syntax: ``` read#/*how the file is reffered as| *variable to store line content in*/# ```
Example: (read first line in a file called hello.txt)
```formin
open#/file|hello.txt/#
read#/file|line/#
spew#/line/#
close#/file/#
```
#### close#//#
Closes given file
Example: 
```formin
open#/file|hello.txt/#
read#/file|line/#
spew#/line/#
close#/file/#
```
ALWAYS CLOSE FILES AFTER OPENING - NOT DOING SO MAKES THE FILE PERSIST IN MEMORY EVEN AFTER CLOSING PROGRAM.

### Strings
>This section will contain all tokens of the str#//# command.
>[str example](Examples/str.fmn) 

#### str#//#
The string command can be used to manipulate strings.
Syntax:
```formin
str#/*method*|*method variables*/#
```
Methods:

##### cat
Syntax:
```formim
str#/cat|*var to store result*|*string 1*|*string 2*|*spacing*/#
```
by default, cat doesnt add space between the 2 strings; in order to do so,
write sp in the spacing token.
The spacing token can be left out.
[Example](Examples/cat.fmn)

##### rev, low, up, len
Syntax: 
```formin
str#/*any of the 3*|*var to store result*|*string*/#
```
Either reverses the string and stores it to the var,
or converts it into uppercase/lowercase and stores in the var
or gets the lenght and stores in the var.
Example:
```formin
create#/anadrome|'drawer'/#
spew#/anadrome/#
str#/rev|anadrome|anadrome/#
spew#/anadrome/#
```

### Lists

lists function the same as arrays.
syntax: ``` list#/*method*|*params*/# ```
list methods:

create/new : makes a new list with set name. 
Example: ``` list#/create|names/# ```

push: pushes value to a list (puts it at the end of the list)
Example: ``` list#/push|names|'Robert'/# ```

get: gets value of list item at an index
Example: ``` list#/get|firstName|names|1/# ```
The example above gets the first item of the list 'names' and stores it in a variable 'firstName'.

set: sets value of an item at an index
Example: ``` list#/set|names|1|'Josh'/# ```
Example above sets the first value in the list 'names' to 'Josh'.

len: gets the lenght of a list
Example: ``` list#/len|listLenght|names/# ```
Example above gets the lenght of the list 'names' and stores it in a var 'listLenght'.

pop: removes last item of list and saves it
Example: ``` list#/pop|lastName|names/# ```
Example above removes the last item of the list 'names' and stores it in a var 'lastName'.

clear: clears a list.
Example: ``` list#/clear|names/# ```

### Trigonometry

#### sin,cos, tan
Gets the sine, cosine, tangent of a number.
Syntax: ``` sin#/*where to store result*|*dg/rad*|*number*/# ```
[Example](Examples/trig.fmn)



### Suffixes
Suffixes are special characters at the end of a command, to change the way it behaves.
They are 1 character long
Example: ``` spew#/'Hello world!'/#*suffix* ```
Example above shows the location of suffixes
#### no suffix
if you don't use a suffix, nothing about the command changes (wow).

#### ?
the **?** suffix makes it so that the command can only be executed once.
Example:
```formin
create#/x|1/#
spew#/x/#

mark#/loop/#
add#/x|x|1/#?
spew#/x/#
go#/loop/#
```
because of the ? suffix at the end of the add command, x is increased by 1 only once.


#### !
the **!** suffix makes it so that if a command fails, the program exits early.
Example:
```formin
create#/x|1/#
... some code between ...
create#/x/#!
```

because of the ! suffix, instead of just printing a warning, the program exits.

#### any other suffix (comment)
If you use any suffix that's not declared here, the line acts as a comment.
It's the best way to make comments without the interpreter shouting at you.



--- 

## Related Links
- [Formin on Esolangs.org](https://esolangs.org/wiki/Formin)
- [Interpreter Source (Fortran)](interpreter.f95)
