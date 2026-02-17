// =============================================================================
// Project : bstoasm
// File    : bstoasm\src\main.zig
// Author  : Ilija Mandic
// Purpose : CLI entry point and pipeline orchestration for transpilation.
// =============================================================================
const std = @import("std"); // Import Zig standard library for printing and basic utilities.
const lexer = @import("lexer.zig"); // Import our lexer module so we can run a tokenization demo.
const parser = @import("parser.zig"); // Import parser module so we can build AST from source.
const ast = @import("ast.zig"); // Import AST types for readable summary output.
const semantic = @import("semantic.zig"); // Import semantic validator for post-parse checks.
const ir = @import("ir.zig"); // Import IR module that lowers AST into backend-friendly instruction stream.
const codegen_x64_nasm = @import("codegen_x64_nasm.zig"); // Import x64 NASM backend that emits textual assembly from IR.

const demo_source = // Keep one built-in BASIC example so default run remains useful for learning and smoke tests.
    \\10 LET A = 42
    \\20 PRINT "HELLO", A
    \\30 IF A >= 10 THEN GOTO 50
    \\40 PRINT "LOW PATH"
    \\50 END
    \\
; // End built-in demo BASIC listing.

const max_source_bytes: usize = 16 * 1024 * 1024; // Set defensive max input size so accidental huge files fail fast.

pub fn main() !void { // Program entry point dispatches between demo mode and real transpile mode.
    const allocator = std.heap.page_allocator; // Use page allocator because process-lifetime allocations are simple in CLI flow.
    const args = try std.process.argsAlloc(allocator); // Read full argv list into allocator-owned slice.
    defer std.process.argsFree(allocator, args); // Release argv memory before process exit.

    if (args.len == 1) { // No CLI args means default demo mode.
        try runDemo(allocator); // Execute verbose demo pipeline that prints tokens, AST, IR, and assembly.
        return; // Exit after demo flow completes.
    } // End default demo dispatch.

    if (std.mem.eql(u8, args[1], "transpile")) { // `transpile` command converts BASIC file to NASM file on disk.
        if (args.len != 4) { // Command requires exactly two positional paths.
            printUsage(); // Show usage text to explain expected arguments.
            return error.InvalidArguments; // Exit with argument error status.
        } // End transpile-arg-count guard.
        try transpileFile(allocator, args[2], args[3]); // Run full parse/semantic/IR/codegen pipeline and write output file.
        return; // Exit after successful transpile.
    } // End transpile command dispatch.

    if (std.mem.eql(u8, args[1], "demo")) { // `demo` command explicitly requests verbose built-in demo.
        try runDemo(allocator); // Reuse demo pipeline function.
        return; // Exit after demo.
    } // End explicit demo command dispatch.

    if (std.mem.eql(u8, args[1], "--help") or std.mem.eql(u8, args[1], "-h")) { // Help flags print command reference.
        printUsage(); // Emit CLI usage text.
        return; // Exit cleanly after help display.
    } // End help command dispatch.

    printUsage(); // Unknown command falls back to usage text so user sees valid options.
    return error.InvalidArguments; // Return argument error for unknown subcommand.
} // End main.

fn runDemo(allocator: std.mem.Allocator) !void { // runDemo executes the full pipeline and prints every stage for learning/debug.
    var lex = lexer.Lexer.init(demo_source); // Create lexer state initialized at start of built-in demo source.

    std.debug.print("=== bstoasm lexer demo ===\n", .{}); // Print demo header so output is easy to read.
    std.debug.print("{s}\n", .{demo_source}); // Print original source so we can compare text and produced tokens.
    std.debug.print("=== tokens ===\n", .{}); // Print section header before token stream.

    while (true) { // Repeatedly ask lexer for next token until EOF appears.
        const tok = lex.next() catch |err| { // Request one token and handle lexer errors explicitly.
            std.debug.print("Lex error: {s}\n", .{@errorName(err)}); // Print readable lexer error name.
            return err; // Propagate error so command exits with failure code.
        }; // End guarded token read.

        printToken(tok); // Print one token in a structured debug format.

        if (tok.tag == .eof) break; // Stop loop after EOF token is emitted.
    } // End token loop.

    std.debug.print("=== parser ===\n", .{}); // Print parser section header.

    var arena = std.heap.ArenaAllocator.init(allocator); // Create arena allocator so parse/lower allocations are reclaimed in one call.
    defer arena.deinit(); // Release all parser/IR/codegen allocations at end of demo.

    const parsed = try parseAndValidate(arena.allocator(), demo_source); // Parse and semantic-check built-in demo source.

    std.debug.print("AST lines: {d}\n", .{parsed.program.lines.len}); // Print count of parsed BASIC lines.
    for (parsed.program.lines) |line| { // Iterate each AST line and print short summary.
        std.debug.print( // Format one line summary with number and statement kind.
            "line_no={d} src=({d}:{d}) stmt={s}\n", // Human-readable one-line summary format.
            .{ line.number, line.line, line.column, stmtName(line.stmt) }, // Fill placeholders from AST line.
        ); // End line summary print.
    } // End AST summary loop.

    std.debug.print("Semantic: OK\n", .{}); // Print success message when semantic pass is clean.
    std.debug.print("=== ir ===\n", .{}); // Print IR section header.
    ir.dumpProgram(parsed.ir_program); // Dump optimized IR listing to stdout for inspection.
    std.debug.print("=== asm (x64 nasm) ===\n", .{}); // Print ASM section header.
    std.debug.print("{s}\n", .{parsed.asm_text}); // Print generated assembly text.
} // End runDemo.

fn transpileFile(allocator: std.mem.Allocator, input_path: []const u8, output_path: []const u8) !void { // transpileFile performs production path from BASIC file to NASM output file.
    const source = try std.fs.cwd().readFileAlloc(allocator, input_path, max_source_bytes); // Read BASIC source file into memory with max-size guard.
    defer allocator.free(source); // Release source buffer after compilation completes.

    var arena = std.heap.ArenaAllocator.init(allocator); // Use arena to simplify temporary allocations for parse/lower/codegen.
    defer arena.deinit(); // Free all arena allocations at function end.

    const parsed = try parseAndValidate(arena.allocator(), source); // Parse, validate, lower, optimize, and emit assembly.
    try std.fs.cwd().writeFile(.{ .sub_path = output_path, .data = parsed.asm_text }); // Persist emitted NASM text to requested output file.

    std.debug.print("Transpiled '{s}' -> '{s}'\n", .{ input_path, output_path }); // Print success summary with source and output paths.
    std.debug.print("Lines: {d}, IR: {d} instructions\n", .{ parsed.program.lines.len, parsed.ir_program.instructions.len }); // Print compact metrics for quick sanity check.
} // End transpileFile.

const PipelineResult = struct { // PipelineResult bundles parsed AST, optimized IR, and emitted NASM text.
    program: ast.Program, // Parsed and validated AST program.
    ir_program: ir.Program, // Lowered and optimized IR program.
    asm_text: []u8, // Final emitted NASM text.
}; // End PipelineResult struct.

fn parseAndValidate(allocator: std.mem.Allocator, source: []const u8) !PipelineResult { // parseAndValidate runs full compiler front/middle/backend pipeline for one BASIC source string.
    const program = parser.parseProgram(allocator, source) catch |err| { // Parse source into AST and report parser failures.
        std.debug.print("Parse error: {s}\n", .{@errorName(err)}); // Print readable parser error.
        return err; // Propagate parser error to caller.
    }; // End parse call.

    semantic.validateProgram(allocator, program) catch |err| { // Run semantic checks after successful parse.
        std.debug.print("Semantic error: {s}\n", .{@errorName(err)}); // Print semantic failure reason.
        return err; // Propagate semantic error to caller.
    }; // End semantic validation call.

    const lowered = ir.lowerProgram(allocator, program) catch |err| { // Lower AST into linear IR.
        std.debug.print("Lower error: {s}\n", .{@errorName(err)}); // Print lowering failure reason.
        return err; // Propagate lowering error.
    }; // End lowering call.

    const optimized = ir.optimizeProgram(allocator, lowered) catch |err| { // Run IR optimization pass before backend emission.
        std.debug.print("Optimize error: {s}\n", .{@errorName(err)}); // Print optimization failure reason.
        return err; // Propagate optimization error.
    }; // End optimization call.

    const asm_text = codegen_x64_nasm.emitProgram(allocator, optimized) catch |err| { // Emit textual NASM source from optimized IR.
        std.debug.print("Codegen error: {s}\n", .{@errorName(err)}); // Print codegen failure reason.
        return err; // Propagate codegen error.
    }; // End codegen call.

    return .{ // Return all useful artifacts so caller can print/report or write file.
        .program = program, // Return parsed AST.
        .ir_program = optimized, // Return optimized IR.
        .asm_text = asm_text, // Return emitted assembly text.
    }; // End result literal.
} // End parseAndValidate.

fn printUsage() void { // printUsage writes CLI usage reference.
    std.debug.print("bastoasm usage:\n", .{}); // Print usage header.
    std.debug.print("  bastoasm                         (demo pipeline)\n", .{}); // Print default demo mode.
    std.debug.print("  bastoasm demo                    (demo pipeline)\n", .{}); // Print explicit demo mode.
    std.debug.print("  bastoasm transpile <in.bas> <out.asm>\n", .{}); // Print production transpile mode.
    std.debug.print("  bastoasm --help\n", .{}); // Print help mode.
} // End printUsage.

fn printToken(tok: lexer.Token) void { // Pretty-printer for one token record.
    if (tok.int_value) |value| { // Number tokens carry parsed integer value in optional field.
        std.debug.print( // Print format for tokens that include numeric value.
            "tag={s: <14} line={d: >3} col={d: >3} lexeme=\"{s}\" int={d}\n", // Aligned columns improve readability.
            .{ @tagName(tok.tag), tok.line, tok.column, tok.lexeme, value }, // Fill placeholders with token fields.
        ); // End numeric-token print.
        return; // Return early so generic print path below is not used.
    } // End numeric-token branch.

    std.debug.print( // Print format for non-numeric tokens.
        "tag={s: <14} line={d: >3} col={d: >3} lexeme=\"{s}\"\n", // Same aligned structure without integer field.
        .{ @tagName(tok.tag), tok.line, tok.column, tok.lexeme }, // Fill placeholders from token fields.
    ); // End non-numeric print.
} // End printToken.

fn stmtName(stmt_ref: ast.StmtRef) []const u8 { // Convert statement tag to short text used in parser summary.
    return @tagName(stmt_ref.tag); // Return enum tag name directly.
} // End stmtName.
