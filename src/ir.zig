// =============================================================================
// Project : bstoasm
// File    : bstoasm\src\ir.zig
// Author  : Ilija Mandic
// Purpose : IR definition, lowering pass, and optimization pass.
// =============================================================================
const std = @import("std"); // Import std for allocators, arrays, and writer formatting.
const ast = @import("ast.zig"); // Import AST model that this module lowers from.

pub const LowerError = error{} || std.mem.Allocator.Error; // LowerError currently wraps allocation failures for lowering stage.

pub const Program = struct { // Program is the IR root container returned by lowerProgram.
    instructions: []Instruction, // instructions stores flat linear IR stream.
    data_values: []i32, // data_values stores flattened DATA literals consumed by READ operations.
}; // End Program struct.

pub const Instruction = struct { // Instruction is one IR operation in linear instruction list.
    tag: Tag, // tag selects active payload variant in `data`.
    data: Data, // data stores operation-specific payload.
}; // End Instruction struct.

pub const Tag = enum { // Tag enumerates all IR opcodes used in current compiler stage.
    label, // label marks a control-flow destination.
    load_const, // load_const pushes immediate integer value.
    load_var, // load_var pushes variable value by index.
    store_var, // store_var pops value and stores into variable index.
    neg, // neg applies unary minus to top of expression stack.
    add, // add pops two values and pushes sum.
    sub, // sub pops two values and pushes difference.
    mul, // mul pops two values and pushes product.
    div, // div pops two values and pushes quotient.
    cmp, // cmp compares two top values with relational op and pushes boolean-like result.
    jump, // jump transfers control to label unconditionally.
    jump_if_false, // jump_if_false pops condition and jumps when false.
    jump_indirect, // jump_indirect transfers control to runtime-computed target line.
    read_data, // read_data loads next DATA item into variable cell.
    poke, // poke pops value/address and stores into runtime memory.
    peek, // peek pops address and pushes runtime memory value.
    call, // call pops call-id expression and dispatches runtime builtin.
    print_str, // print_str emits string literal bytes.
    print_value, // print_value pops numeric value and prints it.
    print_newline, // print_newline emits end-of-line print behavior.
    halt, // halt terminates program execution.
}; // End Tag enum.

pub const Data = union(Tag) { // Data stores payload for each IR opcode variant.
    label: Label, // Payload for label opcode.
    load_const: i32, // Payload for load_const opcode.
    load_var: u8, // Payload for load_var opcode.
    store_var: u8, // Payload for store_var opcode.
    neg: void, // Payload for neg opcode.
    add: void, // Payload for add opcode.
    sub: void, // Payload for sub opcode.
    mul: void, // Payload for mul opcode.
    div: void, // Payload for div opcode.
    cmp: ast.RelOp, // Payload for cmp opcode.
    jump: Label, // Payload for jump opcode.
    jump_if_false: Label, // Payload for jump_if_false opcode.
    jump_indirect: void, // Payload for jump_indirect opcode.
    read_data: u8, // Payload for read_data opcode stores destination variable index.
    poke: void, // Payload for poke opcode.
    peek: void, // Payload for peek opcode.
    call: void, // Payload for call opcode.
    print_str: []const u8, // Payload for print_str opcode.
    print_value: void, // Payload for print_value opcode.
    print_newline: void, // Payload for print_newline opcode.
    halt: void, // Payload for halt opcode.
}; // End Data union.

pub const Label = union(enum) { // Label identifies control-flow destination in IR.
    line: i32, // line label references BASIC line-number entry block.
    temp: usize, // temp label references compiler-generated local block.
}; // End Label union.

pub fn lowerProgram(allocator: std.mem.Allocator, program: ast.Program) LowerError!Program { // lowerProgram converts AST program into linear IR.
    var builder = Builder.init(allocator); // Initialize lowering builder state with empty instruction list.
    defer builder.deinitOnError(); // Clean temporary instruction buffer if lowering exits with error.

    for (program.lines, 0..) |line, i| { // Lower each AST line in source order.
        try builder.emit(.label, .{ .label = .{ .line = line.number } }); // Emit entry label for this BASIC line.
        const next_line = if (i + 1 < program.lines.len) program.lines[i + 1].number else null; // Compute default fallthrough target line.
        try builder.lowerStmt(line.stmt, next_line); // Lower statement with knowledge of natural next line.
    } // End AST line lowering loop.

    return .{ // Freeze builder buffers into owned slices and return complete IR program.
        .instructions = try builder.instructions.toOwnedSlice(builder.allocator), // Move emitted instruction stream out of builder.
        .data_values = try builder.data_values.toOwnedSlice(builder.allocator), // Move flattened DATA literal table out of builder.
    }; // End IR program result literal.
} // End lowerProgram.

pub fn optimizeProgram(allocator: std.mem.Allocator, program: Program) LowerError!Program { // optimizeProgram applies lightweight IR peephole optimizations for cleaner backend output.
    var optimized: std.ArrayList(Instruction) = .empty; // Create mutable list that will receive optimized instruction stream.
    errdefer optimized.deinit(allocator); // Ensure temporary optimized buffer is released on optimization failure.

    var i: usize = 0; // Track current instruction index during single forward optimization pass.
    while (i < program.instructions.len) : (i += 1) { // Walk original IR instructions in order.
        const inst = program.instructions[i]; // Read current instruction for optimization checks.

        if (inst.tag == .jump and i + 1 < program.instructions.len) { // Check unconditional jump that could target immediate next label.
            const next = program.instructions[i + 1]; // Read instruction directly after current jump.
            if (next.tag == .label and labelsEqual(inst.data.jump, next.data.label)) continue; // Remove jump-to-next-label because control already falls through naturally.
        } // End jump-to-next-label peephole.

        try optimized.append(allocator, inst); // Keep instruction when no optimization removed it.
    } // End optimization loop.

    return .{ // Return finalized optimized program while preserving flattened DATA table.
        .instructions = try optimized.toOwnedSlice(allocator), // Use optimized instruction stream as new program body.
        .data_values = program.data_values, // Reuse DATA literal table collected by lowering pass.
    }; // End optimized program literal.
} // End optimizeProgram.

pub fn dumpProgram(program: Program) void { // dumpProgram prints human-readable IR listing for debugging and learning.
    for (program.instructions, 0..) |inst, idx| { // Iterate each IR instruction with index.
        std.debug.print("{d: >3}: ", .{idx}); // Print aligned instruction index prefix.
        dumpInstruction(inst); // Print one formatted instruction line.
        std.debug.print("\n", .{}); // Terminate output line.
    } // End instruction dump loop.
} // End dumpProgram.

fn dumpInstruction(inst: Instruction) void { // dumpInstruction prints one IR instruction in readable text form.
    switch (inst.tag) { // Dispatch by opcode kind.
        .label => dumpLabel("label", inst.data.label), // Print label directive with concrete id.
        .load_const => std.debug.print("load_const {d}", .{inst.data.load_const}), // Print load_const with immediate.
        .load_var => std.debug.print("load_var {d}", .{inst.data.load_var}), // Print load_var with index.
        .store_var => std.debug.print("store_var {d}", .{inst.data.store_var}), // Print store_var with index.
        .neg => std.debug.print("neg", .{}), // Print neg opcode.
        .add => std.debug.print("add", .{}), // Print add opcode.
        .sub => std.debug.print("sub", .{}), // Print sub opcode.
        .mul => std.debug.print("mul", .{}), // Print mul opcode.
        .div => std.debug.print("div", .{}), // Print div opcode.
        .cmp => std.debug.print("cmp {s}", .{relOpName(inst.data.cmp)}), // Print cmp relational operator.
        .jump => dumpLabel("jump", inst.data.jump), // Print unconditional jump target with concrete id.
        .jump_if_false => dumpLabel("jump_if_false", inst.data.jump_if_false), // Print conditional jump target with concrete id.
        .jump_indirect => std.debug.print("jump_indirect", .{}), // Print indirect jump opcode.
        .read_data => std.debug.print("read_data {d}", .{inst.data.read_data}), // Print READ opcode with destination variable index.
        .poke => std.debug.print("poke", .{}), // Print POKE opcode.
        .peek => std.debug.print("peek", .{}), // Print PEEK opcode.
        .call => std.debug.print("call", .{}), // Print CALL opcode.
        .print_str => std.debug.print("print_str \"{s}\"", .{inst.data.print_str}), // Print string literal output op.
        .print_value => std.debug.print("print_value", .{}), // Print numeric print opcode.
        .print_newline => std.debug.print("print_newline", .{}), // Print newline output opcode.
        .halt => std.debug.print("halt", .{}), // Print halt opcode.
    } // End instruction print switch.
} // End dumpInstruction.

fn relOpName(op: ast.RelOp) []const u8 { // relOpName maps AST relational operator enum to short text token.
    return switch (op) { // Dispatch each relational operator variant.
        .equal => "eq", // '=' mapping.
        .less => "lt", // '<' mapping.
        .less_equal => "le", // '<=' mapping.
        .greater => "gt", // '>' mapping.
        .greater_equal => "ge", // '>=' mapping.
        .not_equal => "ne", // '<>' mapping.
    }; // End mapping switch.
} // End relOpName.

fn dumpLabel(op_name: []const u8, label: Label) void { // dumpLabel prints operation name plus concrete label value.
    switch (label) { // Dispatch by label variant.
        .line => |line_no| std.debug.print("{s} L{d}", .{ op_name, line_no }), // Print source line label.
        .temp => |tmp_id| std.debug.print("{s} T{d}", .{ op_name, tmp_id }), // Print compiler temporary label.
    } // End label dump switch.
} // End dumpLabel.

const Builder = struct { // Builder owns mutable lowering state while transforming AST into IR.
    allocator: std.mem.Allocator, // allocator used for instruction buffer growth.
    instructions: std.ArrayList(Instruction), // instructions is mutable collection of emitted IR operations.
    data_values: std.ArrayList(i32), // data_values collects flattened DATA literal stream in source order.
    next_temp_label: usize, // next_temp_label is monotonic counter for generated temp labels.

    fn init(allocator: std.mem.Allocator) Builder { // init constructs clean builder state.
        return .{ // Return initialized builder.
            .allocator = allocator, // Save allocator.
            .instructions = .empty, // Start with empty instruction list.
            .data_values = .empty, // Start with empty DATA literal table.
            .next_temp_label = 0, // First generated temp label id starts from zero.
        }; // End init literal.
    } // End init.

    fn deinitOnError(self: *Builder) void { // deinitOnError releases temporary instruction buffer when caller exits by error path.
        self.instructions.deinit(self.allocator); // Free dynamic instruction array storage.
        self.data_values.deinit(self.allocator); // Free dynamic DATA table storage.
    } // End deinitOnError.

    fn emit(self: *Builder, tag: Tag, data: Data) !void { // emit appends one instruction to builder list.
        try self.instructions.append(self.allocator, .{ .tag = tag, .data = data }); // Push instruction at end of list.
    } // End emit.

    fn freshTempLabel(self: *Builder) Label { // freshTempLabel allocates new unique temporary label id.
        const id = self.next_temp_label; // Capture current label counter value.
        self.next_temp_label += 1; // Advance label counter for next request.
        return .{ .temp = id }; // Return label value for captured id.
    } // End freshTempLabel.

    fn emitContinue(self: *Builder, next_line: ?i32) !void { // emitContinue emits default fallthrough behavior after simple statements.
        if (next_line) |ln| { // When next source line exists, continue by explicit jump.
            try self.emit(.jump, .{ .jump = .{ .line = ln } }); // Emit jump to next BASIC line label.
        } else { // Last source line falls through to program termination.
            try self.emit(.halt, .{ .halt = {} }); // Emit halt at end of program.
        } // End fallthrough branch.
    } // End emitContinue.

    fn lowerStmt(self: *Builder, stmt_ref: ast.StmtRef, next_line: ?i32) !void { // lowerStmt converts one AST statement into IR sequence.
        const stmt = stmt_ref.*; // Copy dereferenced statement for concise field access.

        switch (stmt.tag) { // Dispatch by AST statement kind.
            .let_stmt => { // LET lowering branch.
                try self.lowerExpr(stmt.data.let_stmt.value); // Emit IR for right-hand expression.
                try self.emit(.store_var, .{ .store_var = stmt.data.let_stmt.var_index }); // Store computed value into variable.
                try self.emitContinue(next_line); // Continue to next BASIC line.
            }, // End LET lowering.
            .print_stmt => { // PRINT lowering branch.
                for (stmt.data.print_stmt.items) |item| { // Lower each print item in order.
                    switch (item) { // Dispatch by item variant.
                        .string => |s| try self.emit(.print_str, .{ .print_str = s }), // Emit direct string output.
                        .expr => |expr_ref| { // Expression print item branch.
                            try self.lowerExpr(expr_ref); // Emit IR to compute expression value.
                            try self.emit(.print_value, .{ .print_value = {} }); // Emit print value opcode.
                        }, // End expression item branch.
                    } // End print item switch.
                } // End print-item lowering loop.
                try self.emit(.print_newline, .{ .print_newline = {} }); // Match BASIC PRINT line termination behavior.
                try self.emitContinue(next_line); // Continue to next line after PRINT.
            }, // End PRINT lowering.
            .goto_stmt => { // GOTO lowering branch.
                if (stmt.data.goto_stmt.tag == .number) { // Compile-time constant target branch.
                    try self.emit(.jump, .{ .jump = .{ .line = stmt.data.goto_stmt.data.number } }); // Emit direct jump to constant line label.
                } else { // Dynamic target branch for computed GOTO expression.
                    try self.lowerExpr(stmt.data.goto_stmt); // Emit IR to compute target at runtime.
                    try self.emit(.jump_indirect, .{ .jump_indirect = {} }); // Emit indirect jump opcode.
                } // End GOTO target-kind branch.
            }, // End GOTO lowering.
            .if_then_stmt => { // IF THEN lowering branch.
                const false_label = self.freshTempLabel(); // Allocate label that marks false-condition path.
                try self.lowerExpr(stmt.data.if_then_stmt.left); // Emit left comparison expression.
                try self.lowerExpr(stmt.data.if_then_stmt.right); // Emit right comparison expression.
                try self.emit(.cmp, .{ .cmp = stmt.data.if_then_stmt.op }); // Emit relational compare op.
                try self.emit(.jump_if_false, .{ .jump_if_false = false_label }); // Branch to false path when condition evaluates false.
                try self.lowerStmt(stmt.data.if_then_stmt.then_stmt, next_line); // Lower THEN statement on true branch.
                try self.emit(.label, .{ .label = false_label }); // Mark false branch start position.
                try self.emitContinue(next_line); // False branch continues with natural next line.
            }, // End IF THEN lowering.
            .data_stmt => { // DATA lowering branch.
                try self.data_values.appendSlice(self.allocator, stmt.data.data_stmt.values); // Append DATA literal list to flattened program DATA stream.
                try self.emitContinue(next_line); // DATA is non-executable at runtime, so control falls through.
            }, // End DATA lowering.
            .read_stmt => { // READ lowering branch.
                try self.emit(.read_data, .{ .read_data = stmt.data.read_stmt }); // Emit READ opcode targeting variable index.
                try self.emitContinue(next_line); // Continue execution after READ.
            }, // End READ lowering.
            .poke_stmt => { // POKE lowering branch.
                try self.lowerExpr(stmt.data.poke_stmt.addr); // Emit expression that computes memory address.
                try self.lowerExpr(stmt.data.poke_stmt.value); // Emit expression that computes value to store.
                try self.emit(.poke, .{ .poke = {} }); // Emit POKE opcode.
                try self.emitContinue(next_line); // Continue execution after POKE.
            }, // End POKE lowering.
            .call_stmt => { // CALL lowering branch.
                try self.lowerExpr(stmt.data.call_stmt); // Emit expression that computes CALL id.
                try self.emit(.call, .{ .call = {} }); // Emit CALL opcode.
                try self.emitContinue(next_line); // Continue execution after CALL.
            }, // End CALL lowering.
            .end_stmt => { // END lowering branch.
                try self.emit(.halt, .{ .halt = {} }); // END maps to immediate program termination.
            }, // End END lowering.
        } // End statement lowering switch.
    } // End lowerStmt.

    fn lowerExpr(self: *Builder, expr_ref: ast.ExprRef) !void { // lowerExpr converts one AST expression subtree into stack-style IR.
        if (constEvalExpr(expr_ref)) |constant| { // First try constant folding so fully constant expressions emit one load_const.
            try self.emit(.load_const, .{ .load_const = constant }); // Emit folded immediate value directly.
            return; // Stop lowering because folded constant already represents full expression.
        } // End constant-folding fast path.

        const expr = expr_ref.*; // Copy dereferenced expression node for concise field access.

        switch (expr.tag) { // Dispatch by AST expression kind.
            .number => { // Number literal branch.
                try self.emit(.load_const, .{ .load_const = expr.data.number }); // Push immediate number value.
            }, // End number lowering.
            .variable => { // Variable reference branch.
                try self.emit(.load_var, .{ .load_var = expr.data.variable }); // Push variable value by index.
            }, // End variable lowering.
            .peek => { // PEEK expression branch.
                try self.lowerExpr(expr.data.peek); // Emit expression that computes runtime memory address argument.
                try self.emit(.peek, .{ .peek = {} }); // Emit PEEK opcode to fetch memory cell value.
            }, // End PEEK lowering.
            .unary => { // Unary expression branch.
                try self.lowerExpr(expr.data.unary.expr); // Lower unary operand first.
                switch (expr.data.unary.op) { // Handle unary operator variant.
                    .plus => {}, // Unary plus is no-op in IR.
                    .minus => try self.emit(.neg, .{ .neg = {} }), // Unary minus emits neg opcode.
                } // End unary operator switch.
            }, // End unary lowering.
            .binary => { // Binary expression branch.
                try self.lowerExpr(expr.data.binary.left); // Lower left operand first.
                try self.lowerExpr(expr.data.binary.right); // Lower right operand second.
                switch (expr.data.binary.op) { // Emit arithmetic opcode for binary operator.
                    .add => try self.emit(.add, .{ .add = {} }), // '+' operator lowering.
                    .sub => try self.emit(.sub, .{ .sub = {} }), // '-' operator lowering.
                    .mul => try self.emit(.mul, .{ .mul = {} }), // '*' operator lowering.
                    .div => try self.emit(.div, .{ .div = {} }), // '/' operator lowering.
                } // End binary operator switch.
            }, // End binary lowering.
        } // End expression lowering switch.
    } // End lowerExpr.
}; // End Builder struct.

fn labelsEqual(a: Label, b: Label) bool { // labelsEqual compares two IR labels by variant and payload.
    return switch (a) { // Dispatch on first label variant.
        .line => |line_a| switch (b) { // Compare line labels by numeric line id.
            .line => |line_b| line_a == line_b, // Equal when both variants are line and ids match.
            .temp => false, // Different variants cannot be equal.
        }, // End line-label comparison branch.
        .temp => |temp_a| switch (b) { // Compare temp labels by generated temp id.
            .line => false, // Different variants cannot be equal.
            .temp => |temp_b| temp_a == temp_b, // Equal when both variants are temp and ids match.
        }, // End temp-label comparison branch.
    }; // End label comparison switch.
} // End labelsEqual.

fn constEvalExpr(expr_ref: ast.ExprRef) ?i32 { // constEvalExpr returns folded constant when expression is compile-time pure and in i32 range.
    const expr = expr_ref.*; // Copy expression node for branch-friendly access.

    return switch (expr.tag) { // Dispatch by expression kind for recursive constant evaluation.
        .number => expr.data.number, // Number literal is already a compile-time constant.
        .variable => null, // Variable value is runtime data, so expression is not a compile-time constant.
        .peek => null, // PEEK reads runtime memory, so expression cannot be folded at compile time.
        .unary => blk: { // Unary branch folds nested constant expressions.
            const inner = constEvalExpr(expr.data.unary.expr) orelse break :blk null; // Fold unary operand first; abort if not constant.
            const folded: i64 = switch (expr.data.unary.op) { // Apply unary operator in i64 domain for safe range checks.
                .plus => inner, // Unary plus keeps value unchanged.
                .minus => -@as(i64, inner), // Unary minus negates operand.
            }; // End unary operation fold.
            break :blk fitI32(folded); // Return folded value only when result fits i32.
        }, // End unary folding branch.
        .binary => blk: { // Binary branch folds both operands and then applies arithmetic operator.
            const left = constEvalExpr(expr.data.binary.left) orelse break :blk null; // Fold left operand; abort if runtime-dependent.
            const right = constEvalExpr(expr.data.binary.right) orelse break :blk null; // Fold right operand; abort if runtime-dependent.

            const lhs = @as(i64, left); // Widen left operand to i64 for safe intermediate arithmetic.
            const rhs = @as(i64, right); // Widen right operand to i64 for safe intermediate arithmetic.

            const folded: i64 = switch (expr.data.binary.op) { // Apply binary operator as compile-time arithmetic.
                .add => lhs + rhs, // Fold addition.
                .sub => lhs - rhs, // Fold subtraction.
                .mul => lhs * rhs, // Fold multiplication.
                .div => { // Division needs extra runtime-equivalent guards.
                    if (rhs == 0) break :blk null; // Keep runtime behavior for divide-by-zero by refusing compile-time fold.
                    break :blk fitI32(@divTrunc(lhs, rhs)); // Fold division with truncation semantics and i32 range check.
                }, // End division fold branch.
            }; // End binary operation fold.

            break :blk fitI32(folded); // Return folded value when i32-safe; otherwise keep runtime evaluation path.
        }, // End binary folding branch.
    }; // End constEvalExpr switch.
} // End constEvalExpr.

fn fitI32(value: i64) ?i32 { // fitI32 validates i64 result range before narrowing to i32.
    if (value < std.math.minInt(i32) or value > std.math.maxInt(i32)) return null; // Reject folded values that exceed i32 runtime range.
    return @intCast(value); // Narrow safely after explicit range check.
} // End fitI32.
