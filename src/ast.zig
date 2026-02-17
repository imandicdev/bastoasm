// =============================================================================
// Project : bstoasm
// File    : bstoasm\src\ast.zig
// Author  : Ilija Mandic
// Purpose : AST model for BASIC statements and expressions.
// =============================================================================
const std = @import("std"); // Import std so this module can use standard types consistently.

pub const Program = struct { // Program is the AST root node for one BASIC source file.
    lines: []Line, // lines holds all parsed source lines in original order.
}; // End Program struct.

pub const Line = struct { // Line models one numbered BASIC source line.
    number: i32, // number is the BASIC line number (for example 10, 20, 30).
    stmt: StmtRef, // stmt points to the statement node belonging to this line.
    line: u32, // line is source file line index for diagnostics (1-based).
    column: u32, // column is source file column where this AST line starts (1-based).
}; // End Line struct.

pub const Stmt = struct { // Stmt is a tagged statement node.
    tag: StmtTag, // tag identifies which statement variant is active.
    data: StmtData, // data stores payload for the active statement variant.
}; // End Stmt struct.

pub const StmtTag = enum { // StmtTag enumerates all statement kinds in this compiler stage.
    let_stmt, // let_stmt corresponds to `LET A = expr`.
    print_stmt, // print_stmt corresponds to `PRINT ...`.
    goto_stmt, // goto_stmt corresponds to `GOTO line`.
    gosub_stmt, // gosub_stmt corresponds to `GOSUB line`.
    return_stmt, // return_stmt corresponds to `RETURN`.
    if_then_stmt, // if_then_stmt corresponds to `IF expr relop expr THEN stmt`.
    data_stmt, // data_stmt corresponds to `DATA n[, n ...]`.
    read_stmt, // read_stmt corresponds to `READ A`.
    poke_stmt, // poke_stmt corresponds to `POKE addr, value`.
    call_stmt, // call_stmt corresponds to `CALL n`.
    chain_stmt, // chain_stmt corresponds to `CHAIN "file.bas"`.
    end_stmt, // end_stmt corresponds to `END`.
}; // End StmtTag enum.

pub const StmtData = union(StmtTag) { // StmtData carries per-statement payload keyed by StmtTag.
    let_stmt: LetStmt, // Payload for LET statement.
    print_stmt: PrintStmt, // Payload for PRINT statement.
    goto_stmt: ExprRef, // Payload for GOTO target expression.
    gosub_stmt: ExprRef, // Payload for GOSUB target expression.
    return_stmt: void, // RETURN has no payload.
    if_then_stmt: IfThenStmt, // Payload for IF THEN statement.
    data_stmt: DataStmt, // Payload for DATA statement.
    read_stmt: u8, // Payload for READ variable index.
    poke_stmt: PokeStmt, // Payload for POKE statement.
    call_stmt: ExprRef, // Payload for CALL argument expression.
    chain_stmt: []const u8, // Payload for CHAIN target file path.
    end_stmt: void, // END has no payload.
}; // End StmtData union.

pub const LetStmt = struct { // LetStmt stores variable assignment data.
    var_index: u8, // var_index is 0..25 mapping to A..Z.
    value: ExprRef, // value is expression assigned into variable.
}; // End LetStmt struct.

pub const PrintStmt = struct { // PrintStmt stores list of printable items.
    items: []PrintItem, // items can be string literals and/or expressions.
}; // End PrintStmt struct.

pub const IfThenStmt = struct { // IfThenStmt models one conditional statement.
    left: ExprRef, // left expression of relational comparison.
    op: RelOp, // op is relational operator between left and right.
    right: ExprRef, // right expression of relational comparison.
    then_stmt: StmtRef, // then_stmt is the statement executed when condition is true.
}; // End IfThenStmt struct.

pub const DataStmt = struct { // DataStmt stores one DATA line payload as compile-time integer list.
    values: []i32, // values are raw numeric constants consumed by READ at runtime.
}; // End DataStmt struct.

pub const PokeStmt = struct { // PokeStmt stores address/value expressions for POKE.
    addr: ExprRef, // addr expression computes destination memory cell index.
    value: ExprRef, // value expression computes integer to store.
}; // End PokeStmt struct.

pub const RelOp = enum { // RelOp enumerates supported relational operators.
    equal, // '='
    less, // '<'
    less_equal, // '<='
    greater, // '>'
    greater_equal, // '>='
    not_equal, // '<>' or '><'
}; // End RelOp enum.

pub const PrintItem = union(enum) { // PrintItem is one argument inside PRINT list.
    string: []const u8, // string stores string-literal content without quotes.
    expr: ExprRef, // expr stores expression item to evaluate and print.
}; // End PrintItem union.

pub const Expr = struct { // Expr is a tagged expression node.
    tag: ExprTag, // tag identifies active expression variant.
    data: ExprData, // data stores payload for active expression variant.
}; // End Expr struct.

pub const ExprTag = enum { // ExprTag enumerates expression categories.
    number, // Integer literal.
    variable, // Variable reference A..Z.
    peek, // PEEK(addr) memory read expression.
    unary, // Unary plus/minus expression.
    binary, // Binary arithmetic expression.
}; // End ExprTag enum.

pub const ExprData = union(ExprTag) { // ExprData carries per-expression payload keyed by ExprTag.
    number: i32, // Payload for integer literal.
    variable: u8, // Payload for variable index 0..25.
    peek: ExprRef, // Payload for PEEK argument expression.
    unary: UnaryExpr, // Payload for unary expression.
    binary: BinaryExpr, // Payload for binary expression.
}; // End ExprData union.

pub const UnaryExpr = struct { // UnaryExpr stores unary operator and operand.
    op: UnaryOp, // op is unary operator.
    expr: ExprRef, // expr is operand expression.
}; // End UnaryExpr struct.

pub const BinaryExpr = struct { // BinaryExpr stores binary operator and both operands.
    op: BinaryOp, // op is binary arithmetic operator.
    left: ExprRef, // left operand expression.
    right: ExprRef, // right operand expression.
}; // End BinaryExpr struct.

pub const UnaryOp = enum { // UnaryOp enumerates supported unary operators.
    plus, // Unary '+'.
    minus, // Unary '-'.
}; // End UnaryOp enum.

pub const BinaryOp = enum { // BinaryOp enumerates supported binary operators.
    add, // '+'
    sub, // '-'
    mul, // '*'
    div, // '/'
}; // End BinaryOp enum.

pub const StmtRef = *Stmt; // StmtRef is pointer alias used to keep AST node references compact.
pub const ExprRef = *Expr; // ExprRef is pointer alias used to keep AST node references compact.

pub fn freeProgram(allocator: std.mem.Allocator, program: Program) void { // freeProgram releases AST heap nodes allocated by parser.
    for (program.lines) |line| { // Iterate each parsed line.
        freeStmt(allocator, line.stmt); // Free statement tree for this line.
    } // End line loop.
    allocator.free(program.lines); // Free top-level line array slice.
} // End freeProgram.

fn freeStmt(allocator: std.mem.Allocator, stmt: StmtRef) void { // freeStmt recursively frees one statement subtree.
    switch (stmt.tag) { // Dispatch by statement kind.
        .let_stmt => { // LET branch.
            freeExpr(allocator, stmt.data.let_stmt.value); // Free assigned expression.
        }, // End LET branch.
        .print_stmt => { // PRINT branch.
            for (stmt.data.print_stmt.items) |item| { // Iterate print items.
                switch (item) { // Free only expression items.
                    .string => {}, // String item owns no heap allocation here.
                    .expr => |expr| freeExpr(allocator, expr), // Free expression item.
                } // End print item switch.
            } // End print item loop.
            allocator.free(stmt.data.print_stmt.items); // Free print-item slice.
        }, // End PRINT branch.
        .goto_stmt => { // GOTO branch.
            freeExpr(allocator, stmt.data.goto_stmt); // Free target expression.
        }, // End GOTO branch.
        .gosub_stmt => { // GOSUB branch.
            freeExpr(allocator, stmt.data.gosub_stmt); // Free target expression.
        }, // End GOSUB branch.
        .return_stmt => {}, // RETURN has no heap-owned payload.
        .if_then_stmt => { // IF THEN branch.
            freeExpr(allocator, stmt.data.if_then_stmt.left); // Free left comparison expression.
            freeExpr(allocator, stmt.data.if_then_stmt.right); // Free right comparison expression.
            freeStmt(allocator, stmt.data.if_then_stmt.then_stmt); // Free nested THEN statement.
        }, // End IF THEN branch.
        .data_stmt => { // DATA branch.
            allocator.free(stmt.data.data_stmt.values); // Free owned DATA value list slice.
        }, // End DATA branch.
        .read_stmt => {}, // READ payload is plain variable index.
        .poke_stmt => { // POKE branch.
            freeExpr(allocator, stmt.data.poke_stmt.addr); // Free address expression subtree.
            freeExpr(allocator, stmt.data.poke_stmt.value); // Free value expression subtree.
        }, // End POKE branch.
        .call_stmt => { // CALL branch.
            freeExpr(allocator, stmt.data.call_stmt); // Free CALL argument expression subtree.
        }, // End CALL branch.
        .chain_stmt => {}, // CHAIN path string is a slice into source text; no owned allocation here.
        .end_stmt => {}, // END has no heap-owned payload.
    } // End statement switch.
    allocator.destroy(stmt); // Destroy statement node object itself.
} // End freeStmt.

fn freeExpr(allocator: std.mem.Allocator, expr: ExprRef) void { // freeExpr recursively frees one expression subtree.
    switch (expr.tag) { // Dispatch by expression kind.
        .number => {}, // Number literal has no child nodes.
        .variable => {}, // Variable reference has no child nodes.
        .peek => { // PEEK expression branch.
            freeExpr(allocator, expr.data.peek); // Free PEEK argument expression subtree.
        }, // End PEEK branch.
        .unary => { // Unary expression branch.
            freeExpr(allocator, expr.data.unary.expr); // Free unary operand.
        }, // End unary branch.
        .binary => { // Binary expression branch.
            freeExpr(allocator, expr.data.binary.left); // Free left operand.
            freeExpr(allocator, expr.data.binary.right); // Free right operand.
        }, // End binary branch.
    } // End expression switch.
    allocator.destroy(expr); // Destroy expression node object itself.
} // End freeExpr.
