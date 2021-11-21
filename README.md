# sampletex

A Language that compiles to LaTeX.   
**note : under constant development.**  
*This is a no-batteries included project. Nothing but the compiler is provided. Users are __expected__ to know LaTeX and have tools for compiling LaTeX.*  

### This is alice's personal pet-project. She ***will*** without further warning restructurize the whole thing when she feels like it. **You've been warned.**

## Features
- Direct to PDF compilation (with an user-chosen engine via the `--engine` parameter)
- Multiple file compilation
- Decent-ish error reporting
- Simplified (compared to LaTeX) syntax

## What this currently doesn't have
- DFS-based checks on the `#include` pragma. If you try to break it by doing an `b.sample : #include "a" -> a.sample : #include "b"` loop, you ***WILL.*** The compiler will *e x p a n d*  that macro ad infinitum and I won't be there to help you. No one will. Say goodbye to your cpu and ram.
- `page` `sec` keywords (which were avaliable under a previous version of SampleTex).
- Bogus code. ***hopefully***.

# Utilization
Its the same as it always were `samplec [files]`, you can't get it wrong.  
Use `samplec --help` if you ever got it wrong.

# Spec
See [`this issue`](https://github.com/roridev/sampleTeX/issues/13), its also *pinned* for ease of access.  
Follow that and the compiler should work just fine. If it doesn't open an issue and i'll attempt to fix it.  
