![Formin logo](./Icons/Formin-logo.svg)

# Formin Language 
> A symbolic, flow-based programming language written in Fortran - combining simplicity, structure, and creativity.

**Version:** 1.0.3
**Author:** Capinol  
**Interpreter:** Fortran (interpreter.f95)

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

---

- [Overview](#overview)
- [Installation](#installation)
    - [Linux/MacOS](#linux---macos)
    - [Windows](#windows-using-powershell-or-cmd)
    - [Licenses](#license)
- [Quick start](#quick-start)
- [Examples](#examples)
- [Commands](#commands)
  - [Format](#format)
  - [Execution](#execution)
  - [Variables](#variables)
  - [Arithmetic](#arithmetic)
  - [File Handling](#file-handling)
- [Related links](#related-links)

---

## Installation

Formin is distributed as a **stand-alone binary** - no installation or dependencies required.  
Just download it from the [Releases page](https://github.com/CapinolDev/formin/releases) and run it.

---

###  Linux /  macOS
1. Download the latest **`formin`** binary from the [Releases page](https://github.com/CapinolDev/Formin/releases/latest).
2. Make it executable:
   ```bash
   chmod +x formin ```
3. Move it to a system path (so you can run it anywhere):
    ```bash
     sudo mv formin /usr/local/bin/ ```
4. Run Formin: 
```bash
formin examples/hello.formin
```
### Windows (using PowerShell or CMD)

1. Download the latest **`formin`** binary from the [Releases page](https://github.com/CapinolDev/Formin/releases/latest).
2. Open PowerShell in the folder where the binary is located.
3. Run 
```powershell
    .\formin examples\hello.formin
```
If Windows blocks the binary, right-click → Properties → check Unblock, or run PowerShell as Administrator.

---
## Quick Start

1. Create a new file called `hello.fmn`:
   ```formin
   create#/name|'World'/#
   spew#/'Hello,'|name|!/#
2. Run it
    ```bash
   formin hello.fmn
Output:
    ```Hello, World! ```

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

### Variables

#### create#//#
makes / sets a variable to a value.
Has 3 types: int, real, string
Example: ``` create#/x|10|int/# ```

#### ask#//#
Asks the user a question, and stores it in a variable
Example: 
```formin
    ask#/'What is your name?'|name/#
    spew#/'Hello'|name/#
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

--- 

## Related Links
- [Formin on Esolangs.org](https://esolangs.org/wiki/Formin)
- [Interpreter Source (Fortran)](interpreter.f95)
