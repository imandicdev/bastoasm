Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repo = Split-Path -Parent $PSScriptRoot
Set-Location $repo

$zig = "D:\programiranje\Zig\zig-x86_64-windows-0.15.1\zig.exe"
$nasm = "C:\Program Files\NASM\nasm.exe"
$bas = "D:\programiranje\Zig\tinybasic\zig-out\bin\SpaceInvadersPro.bas"

& $zig build -Doptimize=ReleaseSmall
.\zig-out\bin\bastoasm.exe transpile $bas .\SpaceInvadersPro.asm
& $nasm -f win64 .\SpaceInvadersPro.asm -o .\SpaceInvadersPro.obj
& $zig cc -target x86_64-windows -Oz -s -c .\runtime\tb_runtime.c -o .\runtime\tb_runtime_release.obj
& $zig cc -target x86_64-windows -Oz -s .\SpaceInvadersPro.obj .\runtime\tb_runtime_release.obj -luser32 -lgdi32 -o .\SpaceInvadersPro_release.exe

Get-Item .\zig-out\bin\bastoasm.exe, .\SpaceInvadersPro_release.exe |
    Select-Object Name, Length, LastWriteTime |
    Format-Table -AutoSize
