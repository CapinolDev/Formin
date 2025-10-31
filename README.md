![Formin logo](./Icons/Formin-logo.svg)

# Formin Language Syntax Reference

**Version:** 1.0.1  
**Author:** Capinol  
**Interpreter:** Fortran (formin-interpreter.f90)

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





