// =============================================================================
// Project : bstoasm
// File    : bstoasm\src\semantic.zig
// Author  : Ilija Mandic
// Purpose : Semantic checker for AST correctness and constraints.
// =============================================================================
const std = @import("std"); // Import std for hash maps and allocator-based helpers.
const ast = @import("ast.zig"); // Import AST node types to validate parser output.

pub const SemanticError = error{ // SemanticError enumerates validation failures after syntactic parsing.
    LineNumberOutOfRange, // BASIC line number must be in accepted numeric range.
    DuplicateLineNumber, // Same BASIC line number appears more than once.
    InvalidVariableIndex, // Variable index is outside A..Z range.
    MissingGotoTarget, // Constant GOTO target line does not exist in program.
    DynamicGosubNotSupported, // GOSUB currently requires compile-time constant target line.
    MissingGosubTarget, // Constant GOSUB target line does not exist in program.
    ChainNotSupportedInNativeTranspile, // CHAIN cannot be lowered to single native executable in current compiler stage.
}; // End SemanticError error set.

pub fn validateProgram(allocator: std.mem.Allocator, program: ast.Program) SemanticError!void { // Validate entire AST program in two passes.
    var line_set = std.AutoHashMap(i32, void).init(allocator); // Build set of existing line numbers for duplicate and target checks.
    defer line_set.deinit(); // Always release map allocations on function exit.

    for (program.lines) |line| { // First pass: validate and collect declared line numbers.
        if (line.number < 1 or line.number > 65535) return SemanticError.LineNumberOutOfRange; // Enforce legal BASIC line number range.
        if (line_set.contains(line.number)) return SemanticError.DuplicateLineNumber; // Reject duplicate line numbers early.
        line_set.put(line.number, {}) catch return SemanticError.DuplicateLineNumber; // Insert line number into set.
    } // End first pass loop.

    for (program.lines) |line| { // Second pass: validate statement/expression semantics with full line map available.
        try validateStmt(line.stmt, &line_set); // Validate one statement subtree.
    } // End second pass loop.
} // End validateProgram.

fn validateStmt(stmt_ref: ast.StmtRef, line_set: *const std.AutoHashMap(i32, void)) SemanticError!void { // Validate one statement node recursively.
    const stmt = stmt_ref.*; // Copy dereferenced statement for concise field access in switch branches.

    switch (stmt.tag) { // Dispatch per statement kind.
        .let_stmt => { // LET statement semantic checks.
            try validateVarIndex(stmt.data.let_stmt.var_index); // Validate assigned variable index bounds.
            try validateExpr(stmt.data.let_stmt.value); // Validate assigned expression tree.
        }, // End LET branch.
        .print_stmt => { // PRINT statement semantic checks.
            for (stmt.data.print_stmt.items) |item| { // Validate each print item.
                switch (item) { // Print item can be string literal or expression.
                    .string => {}, // String literal has no extra semantic constraints in this stage.
                    .expr => |expr_ref| try validateExpr(expr_ref), // Validate expression item recursively.
                } // End print item switch.
            } // End print-item loop.
        }, // End PRINT branch.
        .goto_stmt => { // GOTO statement semantic checks.
            const target_expr = stmt.data.goto_stmt; // Read GOTO target expression node.
            try validateExpr(target_expr); // Validate target expression subtree first.
            if (target_expr.tag == .number) { // If target is compile-time constant, verify line exists.
                const target_line = target_expr.data.number; // Extract numeric GOTO target.
                if (!line_set.contains(target_line)) return SemanticError.MissingGotoTarget; // Reject unresolved constant jump target.
            } // End constant-target check.
        }, // End GOTO branch.
        .gosub_stmt => { // GOSUB statement semantic checks.
            const target_expr = stmt.data.gosub_stmt; // Read GOSUB target expression node.
            try validateExpr(target_expr); // Validate target expression subtree first.
            if (target_expr.tag != .number) return SemanticError.DynamicGosubNotSupported; // Keep first implementation explicit: only constant targets are supported in native lowering.
            const target_line = target_expr.data.number; // Extract numeric GOSUB target line.
            if (!line_set.contains(target_line)) return SemanticError.MissingGosubTarget; // Reject unresolved constant subroutine target.
        }, // End GOSUB branch.
        .return_stmt => {}, // RETURN has no extra semantic checks in this compiler stage.
        .if_then_stmt => { // IF THEN statement semantic checks.
            try validateExpr(stmt.data.if_then_stmt.left); // Validate left comparison expression.
            try validateExpr(stmt.data.if_then_stmt.right); // Validate right comparison expression.
            try validateStmt(stmt.data.if_then_stmt.then_stmt, line_set); // Validate nested THEN statement recursively.
        }, // End IF THEN branch.
        .data_stmt => {}, // DATA values are compile-time literals already validated by parser.
        .read_stmt => try validateVarIndex(stmt.data.read_stmt), // READ target variable index must be in A..Z range.
        .poke_stmt => { // POKE statement semantic checks.
            try validateExpr(stmt.data.poke_stmt.addr); // Validate POKE address expression.
            try validateExpr(stmt.data.poke_stmt.value); // Validate POKE value expression.
        }, // End POKE branch.
        .call_stmt => try validateExpr(stmt.data.call_stmt), // CALL argument expression must be semantically valid.
        .chain_stmt => return SemanticError.ChainNotSupportedInNativeTranspile, // CHAIN requires multi-file runtime orchestration and is currently rejected explicitly.
        .end_stmt => {}, // END has no payload and no semantic checks in this stage.
    } // End statement switch.
} // End validateStmt.

fn validateExpr(expr_ref: ast.ExprRef) SemanticError!void { // Validate one expression subtree recursively.
    const expr = expr_ref.*; // Copy dereferenced expression for concise branch access.

    switch (expr.tag) { // Dispatch by expression kind.
        .number => {}, // Numeric literal is always valid by itself.
        .variable => try validateVarIndex(expr.data.variable), // Variable reference must map to A..Z index.
        .peek => { // PEEK expression semantic checks.
            try validateExpr(expr.data.peek); // Validate PEEK address expression recursively.
        }, // End PEEK branch.
        .unary => { // Unary expression checks.
            try validateExpr(expr.data.unary.expr); // Validate unary operand recursively.
        }, // End unary branch.
        .binary => { // Binary expression checks.
            try validateExpr(expr.data.binary.left); // Validate left operand subtree.
            try validateExpr(expr.data.binary.right); // Validate right operand subtree.
        }, // End binary branch.
    } // End expression switch.
} // End validateExpr.

fn validateVarIndex(idx: u8) SemanticError!void { // Verify variable index is inside valid compiler range.
    if (idx >= 26) return SemanticError.InvalidVariableIndex; // A..Z maps to 0..25 only.
} // End validateVarIndex.
