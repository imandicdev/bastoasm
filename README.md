# bastoasm

TinyBASIC `.bas` to x64 NASM `.asm` transpiler in Zig 0.15.x.

Author: Ilija Mandic

## What This Project Is

- A compiler/transpiler pipeline for TinyBASIC source.
- Target output is x64 NASM assembly text.
- Intended to pair with the TinyBASIC ecosystem and native build flow.

## Pipeline

1. Lex BASIC source
2. Parse into AST
3. Run semantic validation
4. Lower AST to IR
5. Apply IR optimizations
6. Emit x64 NASM assembly

## Architecture

- Front-end: `lexer -> parser -> semantic`
- Middle-end: AST -> IR
- Back-end: IR -> NASM text
- Runtime boundary: emitted assembly calls `tb_*` functions from `runtime/tb_runtime.c`

## Historical Context

The language model and IL conventions are based on the original Tiny BASIC IL design published in 1975.  
The historical implementation targeted Intel Intellec 8 / MOD 80 (8080-era) hosts, while this project keeps the model portable in modern tooling.

## Build

```powershell
zig build -Doptimize=ReleaseSmall
zig-out/bin/bastoasm transpile ./examples_demo.bas ./examples_demo.asm
```

On Windows, the binary is `zig-out/bin/bastoasm.exe`.

## Assemble and Link Example

```powershell
nasm -f win64 ./examples_demo.asm -o ./examples_demo.obj
zig cc -target x86_64-windows -Oz -s -c ./runtime/tb_runtime.c -o ./runtime/tb_runtime_release.obj
zig cc -target x86_64-windows -Oz -s ./examples_demo.obj ./runtime/tb_runtime_release.obj -luser32 -lgdi32 -o ./examples_demo.exe
./examples_demo.exe
```

For the Space Invaders release build:

```powershell
powershell -ExecutionPolicy Bypass -File ./scripts/build_space_invaders_release.ps1
./SpaceInvadersPro_release.exe
```
