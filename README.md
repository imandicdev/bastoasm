# bstoasm

Tiny BASIC (`.bas`) to x64 NASM (`.asm`) transpiler in Zig 0.15.x.

## Current Pipeline

1. Parse BASIC source into AST
2. Semantic validation
3. Lower to IR
4. IR optimization (constant folding + simple jump cleanup)
5. Emit x64 NASM source

## Transpile

```powershell
cd D:\programiranje\Zig\bstoasm
..\zig-x86_64-windows-0.15.1\zig.exe build -Doptimize=ReleaseSmall
.\zig-out\bin\bastoasm.exe transpile .\examples_demo.bas .\examples_demo.asm
```

## Assemble + Link (Windows x64, Small Release)

`bastoasm` intentionally stops at `.asm`, but you can build a runnable `.exe` with NASM and Zig's C toolchain.

```powershell
cd D:\programiranje\Zig\bstoasm
C:\Program Files\NASM\nasm.exe -f win64 .\examples_demo.asm -o .\examples_demo.obj
..\zig-x86_64-windows-0.15.1\zig.exe cc -target x86_64-windows -Oz -s -c .\runtime\tb_runtime.c -o .\runtime\tb_runtime_release.obj
..\zig-x86_64-windows-0.15.1\zig.exe cc -target x86_64-windows -Oz -s .\examples_demo.obj .\runtime\tb_runtime_release.obj -luser32 -lgdi32 -o .\examples_demo.exe
.\examples_demo.exe
```

For your game:

```powershell
cd D:\programiranje\Zig\bstoasm
powershell -ExecutionPolicy Bypass -File .\scripts\build_space_invaders_release.ps1
.\SpaceInvadersPro_release.exe
```

Current measured sizes (this workspace):
- `bastoasm.exe` (`ReleaseSmall`): ~266 KB
- `SpaceInvadersPro_release.exe` (`-Oz -s`): ~29 KB

## Runtime Symbols Expected by Generated ASM

- `tb_print_i32`
- `tb_print_cstr`
- `tb_print_newline`
- `tb_data_init`
- `tb_read_data`
- `tb_poke`
- `tb_peek`
- `tb_call`
- `tb_exit`

Reference implementation is in `runtime/tb_runtime.c`.
