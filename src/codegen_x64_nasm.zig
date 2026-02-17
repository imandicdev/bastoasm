const std = @import("std"); // Import std for allocator, maps, and string formatting helpers.
const ir = @import("ir.zig"); // Import IR model that this backend converts into NASM source text.
const ast = @import("ast.zig"); // Import AST relational operator enum reused in IR compare payload.

pub const CodegenError = error{} || std.mem.Allocator.Error; // CodegenError currently wraps allocation failures in emitter pipeline.

pub fn emitProgram(allocator: std.mem.Allocator, program: ir.Program) CodegenError![]u8 { // emitProgram builds complete NASM text from IR program.
    var strings = std.StringHashMap(usize).init(allocator); // Map literal string content to deterministic string label ids.
    defer strings.deinit(); // Release string map allocations on function exit.

    var next_string_id: usize = 0; // Counter used to assign unique data-label index for each distinct string literal.

    for (program.instructions) |inst| { // First pass collects all print_str payloads into map.
        if (inst.tag != .print_str) continue; // Ignore non-string instructions during string-table scan.
        const s = inst.data.print_str; // Read literal text payload from IR instruction.
        if (strings.contains(s)) continue; // Skip duplicate literals to avoid repeated data definitions.
        try strings.put(s, next_string_id); // Register new literal with next available id.
        next_string_id += 1; // Advance id counter for subsequent new strings.
    } // End string collection pass.

    var out: std.ArrayList(u8) = .empty; // Dynamic output buffer for generated NASM source text.
    errdefer out.deinit(allocator); // Ensure temporary buffer is released if codegen exits with error.

    try appendLine(&out, allocator, "default rel"); // Emit NASM mode directive for RIP-relative addressing.
    try appendLine(&out, allocator, "global main"); // Export `main` symbol as process entry point used by linker/runtime.
    try appendLine(&out, allocator, "extern tb_print_i32"); // Declare runtime print function for integer values.
    try appendLine(&out, allocator, "extern tb_print_cstr"); // Declare runtime print function for zero-terminated C strings.
    try appendLine(&out, allocator, "extern tb_print_newline"); // Declare runtime newline function.
    try appendLine(&out, allocator, "extern tb_data_init"); // Declare runtime DATA stream initialization function.
    try appendLine(&out, allocator, "extern tb_read_data"); // Declare runtime DATA read function.
    try appendLine(&out, allocator, "extern tb_poke"); // Declare runtime POKE function.
    try appendLine(&out, allocator, "extern tb_peek"); // Declare runtime PEEK function.
    try appendLine(&out, allocator, "extern tb_call"); // Declare runtime CALL dispatch function.
    try appendLine(&out, allocator, "extern tb_exit"); // Declare runtime exit function.
    try appendLine(&out, allocator, ""); // Emit blank separator line.

    try appendLine(&out, allocator, "section .text"); // Begin executable code section.
    try appendLine(&out, allocator, "main:"); // Emit function entry label for generated program.
    try appendLine(&out, allocator, "    push rbp"); // Save base pointer to establish stack frame.
    try appendLine(&out, allocator, "    mov rbp, rsp"); // Initialize frame pointer.
    try appendLine(&out, allocator, "    sub rsp, 32"); // Reserve 32 bytes (Win64 shadow space + alignment cushion).
    try appendLine(&out, allocator, "    lea rcx, [rel tb_data_values]"); // Pass pointer to flattened DATA table into runtime.
    try appendFmt(&out, allocator, "    mov edx, {d}\n", .{program.data_values.len}); // Pass DATA item count into runtime.
    try appendLine(&out, allocator, "    call tb_data_init"); // Initialize READ pointer before executing first BASIC line.

    if (program.instructions.len > 0) { // If IR contains instructions, jump to first source label to start execution.
        if (program.instructions[0].tag == .label) { // Only emit bootstrap jump when first instruction is a label.
            try emitJumpToLabel(&out, allocator, "    jmp ", program.instructions[0].data.label); // Emit initial jump into first IR label.
        } // End first-instruction label guard.
    } // End bootstrap jump branch.

    for (program.instructions) |inst| { // Main pass converts each IR instruction into one or more NASM instructions.
        switch (inst.tag) { // Dispatch per IR opcode.
            .label => try emitLabel(&out, allocator, inst.data.label), // Emit label definition line.
            .load_const => { // Emit stack-push for immediate integer.
                try appendFmt(&out, allocator, "    mov rax, {d}\n", .{inst.data.load_const}); // Load immediate constant into RAX.
                try appendLine(&out, allocator, "    push rax"); // Push value onto virtual expression stack.
            }, // End load_const emission.
            .load_var => { // Emit variable read and push.
                const off = @as(usize, inst.data.load_var) * 4; // Compute byte offset in `vars` dword array.
                try appendFmt(&out, allocator, "    movsxd rax, dword [rel vars+{d}]\n", .{off}); // Load signed 32-bit var and extend to 64-bit.
                try appendLine(&out, allocator, "    push rax"); // Push loaded variable value on stack.
            }, // End load_var emission.
            .store_var => { // Emit stack-pop into variable cell.
                const off = @as(usize, inst.data.store_var) * 4; // Compute byte offset in `vars` dword array.
                try appendLine(&out, allocator, "    pop rax"); // Pop expression result from stack.
                try appendFmt(&out, allocator, "    mov dword [rel vars+{d}], eax\n", .{off}); // Store low 32 bits into variable slot.
            }, // End store_var emission.
            .neg => { // Emit unary negation.
                try appendLine(&out, allocator, "    pop rax"); // Pop operand into RAX.
                try appendLine(&out, allocator, "    neg rax"); // Negate value.
                try appendLine(&out, allocator, "    push rax"); // Push result back to stack.
            }, // End neg emission.
            .add => try emitBinaryArith(&out, allocator, "add"), // Emit addition sequence.
            .sub => try emitBinaryArith(&out, allocator, "sub"), // Emit subtraction sequence.
            .mul => { // Emit multiplication sequence.
                try appendLine(&out, allocator, "    pop rcx"); // Pop right operand into RCX.
                try appendLine(&out, allocator, "    pop rax"); // Pop left operand into RAX.
                try appendLine(&out, allocator, "    imul rax, rcx"); // Signed multiply RAX by RCX.
                try appendLine(&out, allocator, "    push rax"); // Push multiplication result.
            }, // End mul emission.
            .div => { // Emit signed division sequence.
                try appendLine(&out, allocator, "    pop rcx"); // Pop divisor into RCX.
                try appendLine(&out, allocator, "    pop rax"); // Pop dividend into RAX.
                try appendLine(&out, allocator, "    cqo"); // Sign-extend RAX into RDX:RAX for idiv.
                try appendLine(&out, allocator, "    idiv rcx"); // Signed divide RDX:RAX by RCX, quotient in RAX.
                try appendLine(&out, allocator, "    push rax"); // Push quotient result.
            }, // End div emission.
            .cmp => try emitCompare(&out, allocator, inst.data.cmp), // Emit relational comparison and boolean push.
            .jump => try emitJumpToLabel(&out, allocator, "    jmp ", inst.data.jump), // Emit unconditional jump.
            .jump_if_false => { // Emit conditional jump on false (zero).
                try appendLine(&out, allocator, "    pop rax"); // Pop condition value from stack.
                try appendLine(&out, allocator, "    cmp rax, 0"); // Compare condition with zero.
                try emitJumpToLabel(&out, allocator, "    je ", inst.data.jump_if_false); // Jump when condition equals zero.
            }, // End jump_if_false emission.
            .jump_indirect => { // Emit runtime-computed jump placeholder behavior.
                try appendLine(&out, allocator, "    ; TODO: jump_indirect lowering"); // Record current limitation in emitted assembly.
                try appendLine(&out, allocator, "    mov ecx, 1"); // Set non-zero exit code to mark unsupported feature path.
                try appendLine(&out, allocator, "    call tb_exit"); // Terminate program through runtime helper.
            }, // End jump_indirect emission.
            .read_data => { // Emit READ operation into destination variable.
                const off = @as(usize, inst.data.read_data) * 4; // Compute destination variable byte offset.
                try appendLine(&out, allocator, "    call tb_read_data"); // Get next DATA value in EAX.
                try appendFmt(&out, allocator, "    mov dword [rel vars+{d}], eax\n", .{off}); // Store read DATA value into destination variable.
            }, // End read_data emission.
            .poke => { // Emit POKE runtime memory write.
                try appendLine(&out, allocator, "    pop rdx"); // Pop value expression into RDX (second argument).
                try appendLine(&out, allocator, "    pop rcx"); // Pop address expression into RCX (first argument).
                try appendLine(&out, allocator, "    call tb_poke"); // Dispatch runtime memory write.
            }, // End poke emission.
            .peek => { // Emit PEEK runtime memory read.
                try appendLine(&out, allocator, "    pop rcx"); // Pop address expression into RCX (first argument).
                try appendLine(&out, allocator, "    call tb_peek"); // Fetch runtime memory value, returned in EAX.
                try appendLine(&out, allocator, "    movsxd rax, eax"); // Sign-extend 32-bit return value to stack machine width.
                try appendLine(&out, allocator, "    push rax"); // Push PEEK result back onto virtual expression stack.
            }, // End peek emission.
            .call => { // Emit CALL runtime dispatch.
                try appendLine(&out, allocator, "    pop rcx"); // Pop CALL id expression into RCX (first argument).
                try appendLine(&out, allocator, "    call tb_call"); // Invoke runtime builtin dispatcher.
            }, // End call emission.
            .print_str => { // Emit string print call.
                const id = strings.get(inst.data.print_str).?; // Resolve previously assigned data-label id.
                try appendFmt(&out, allocator, "    lea rcx, [rel str_{d}]\n", .{id}); // Load pointer to string literal into RCX (first arg).
                try appendLine(&out, allocator, "    call tb_print_cstr"); // Call runtime string print function.
            }, // End print_str emission.
            .print_value => { // Emit numeric print call.
                try appendLine(&out, allocator, "    pop rcx"); // Pop value into RCX (first arg register).
                try appendLine(&out, allocator, "    call tb_print_i32"); // Call runtime integer print function.
            }, // End print_value emission.
            .print_newline => { // Emit newline print call.
                try appendLine(&out, allocator, "    call tb_print_newline"); // Call runtime newline helper.
            }, // End print_newline emission.
            .halt => { // Emit process termination path.
                try appendLine(&out, allocator, "    mov ecx, 0"); // Set exit code 0.
                try appendLine(&out, allocator, "    call tb_exit"); // Exit through runtime helper.
            }, // End halt emission.
        } // End IR opcode emission switch.
    } // End IR emission loop.

    try appendLine(&out, allocator, ""); // Emit blank separator before fallback epilogue.
    try appendLine(&out, allocator, "exit_fallback:"); // Emit fallback return path in case runtime exit call returns unexpectedly.
    try appendLine(&out, allocator, "    xor eax, eax"); // Return 0 in EAX by default.
    try appendLine(&out, allocator, "    add rsp, 32"); // Restore stack pointer by removing reserved shadow space.
    try appendLine(&out, allocator, "    pop rbp"); // Restore caller base pointer.
    try appendLine(&out, allocator, "    ret"); // Return to process startup/runtime.
    try appendLine(&out, allocator, ""); // Emit blank separator before data sections.

    try appendLine(&out, allocator, "section .bss"); // Begin uninitialized data section.
    try appendLine(&out, allocator, "vars: resd 26"); // Reserve 26 signed 32-bit variables for A..Z.
    try appendLine(&out, allocator, ""); // Emit blank separator before read-only strings.

    try appendLine(&out, allocator, "section .rdata"); // Begin read-only data section for string literals.
    try emitStringTable(&out, allocator, &strings); // Emit one zero-terminated data label for each collected literal.
    try emitDataTable(&out, allocator, program.data_values); // Emit flattened DATA table consumed by READ runtime.

    return out.toOwnedSlice(allocator); // Return generated NASM text as owned UTF-8 byte slice.
} // End emitProgram.

fn emitBinaryArith(out: *std.ArrayList(u8), allocator: std.mem.Allocator, op: []const u8) !void { // emitBinaryArith emits stack machine template for add/sub.
    try appendLine(out, allocator, "    pop rcx"); // Pop right operand into RCX.
    try appendLine(out, allocator, "    pop rax"); // Pop left operand into RAX.
    try appendFmt(out, allocator, "    {s} rax, rcx\n", .{op}); // Apply arithmetic operation in-place on RAX.
    try appendLine(out, allocator, "    push rax"); // Push arithmetic result back onto stack.
} // End emitBinaryArith.

fn emitCompare(out: *std.ArrayList(u8), allocator: std.mem.Allocator, op: ast.RelOp) !void { // emitCompare emits comparison and boolean materialization sequence.
    try appendLine(out, allocator, "    pop rcx"); // Pop right operand into RCX.
    try appendLine(out, allocator, "    pop rax"); // Pop left operand into RAX.
    try appendLine(out, allocator, "    cmp rax, rcx"); // Compare left and right operands.
    const setcc = switch (op) { // Map AST relational operator to x86 `setcc` mnemonic.
        .equal => "sete", // '=' maps to set-equal.
        .less => "setl", // '<' maps to set-less.
        .less_equal => "setle", // '<=' maps to set-less-or-equal.
        .greater => "setg", // '>' maps to set-greater.
        .greater_equal => "setge", // '>=' maps to set-greater-or-equal.
        .not_equal => "setne", // '<>' maps to set-not-equal.
    }; // End setcc mapping.
    try appendFmt(out, allocator, "    {s} al\n", .{setcc}); // Store boolean result byte in AL.
    try appendLine(out, allocator, "    movzx rax, al"); // Zero-extend AL into full RAX register.
    try appendLine(out, allocator, "    push rax"); // Push boolean result on expression stack.
} // End emitCompare.

fn emitLabel(out: *std.ArrayList(u8), allocator: std.mem.Allocator, label: ir.Label) !void { // emitLabel prints one NASM label definition line.
    switch (label) { // Dispatch by label kind.
        .line => |line_no| try appendFmt(out, allocator, "L{d}:\n", .{line_no}), // Source line label form.
        .temp => |tmp_id| try appendFmt(out, allocator, "T{d}:\n", .{tmp_id}), // Temporary compiler label form.
    } // End label-kind switch.
} // End emitLabel.

fn emitJumpToLabel(out: *std.ArrayList(u8), allocator: std.mem.Allocator, prefix: []const u8, label: ir.Label) !void { // emitJumpToLabel prints jump instruction to concrete label.
    switch (label) { // Dispatch by label kind.
        .line => |line_no| try appendFmt(out, allocator, "{s}L{d}\n", .{ prefix, line_no }), // Jump to source line label.
        .temp => |tmp_id| try appendFmt(out, allocator, "{s}T{d}\n", .{ prefix, tmp_id }), // Jump to temporary label.
    } // End jump-label switch.
} // End emitJumpToLabel.

fn emitStringTable(out: *std.ArrayList(u8), allocator: std.mem.Allocator, strings: *const std.StringHashMap(usize)) !void { // emitStringTable writes zero-terminated NASM byte arrays for literals.
    var reverse: std.ArrayList([]const u8) = .empty; // Reverse lookup array where index is string id and value is literal bytes.
    defer reverse.deinit(allocator); // Release reverse array buffer when function exits.

    try reverse.ensureTotalCapacity(allocator, strings.count()); // Reserve slots for all literals.
    reverse.appendNTimesAssumeCapacity("", strings.count()); // Fill placeholder entries to make indices writable by id.

    var it = strings.iterator(); // Create iterator over string->id mapping entries.
    while (it.next()) |entry| { // Populate reverse array according to stored ids.
        reverse.items[entry.value_ptr.*] = entry.key_ptr.*; // Place literal bytes at array index equal to assigned id.
    } // End reverse mapping fill loop.

    for (reverse.items, 0..) |s, id| { // Emit string labels in id order for deterministic assembly output.
        try appendFmt(out, allocator, "str_{d}: db ", .{id}); // Emit start of string data definition line.
        try appendDbEscapedString(out, allocator, s); // Emit comma-separated byte values for literal content.
        try appendLine(out, allocator, ", 0"); // Terminate each string with zero byte for C-style runtime print calls.
    } // End string emission loop.
} // End emitStringTable.

fn emitDataTable(out: *std.ArrayList(u8), allocator: std.mem.Allocator, values: []const i32) !void { // emitDataTable emits flattened BASIC DATA stream as `dd` table for runtime READ.
    if (values.len == 0) { // Ensure label exists even for programs with no DATA statements.
        try appendLine(out, allocator, "tb_data_values: dd 0"); // Emit one dummy dword to keep symbol defined.
        return; // Exit after emitting dummy table.
    } // End empty-data branch.

    try appendFmt(out, allocator, "tb_data_values: dd {d}", .{values[0]}); // Emit first DATA element on same line as label.
    var i: usize = 1; // Start at second element because first was emitted already.
    while (i < values.len) : (i += 1) { // Emit remaining DATA values as comma-separated dwords.
        try appendFmt(out, allocator, ", {d}", .{values[i]}); // Append one DATA value to current `dd` line.
    } // End DATA table emission loop.
    try appendLine(out, allocator, ""); // Terminate DATA table line.
} // End emitDataTable.

fn appendDbEscapedString(out: *std.ArrayList(u8), allocator: std.mem.Allocator, s: []const u8) !void { // appendDbEscapedString emits NASM `db` byte list from raw string bytes.
    if (s.len == 0) { // Empty string literal branch.
        try out.appendSlice(allocator, "0"); // Represent empty content as single numeric byte text.
        return; // Return after handling empty string.
    } // End empty literal branch.

    for (s, 0..) |b, i| { // Emit each byte value in decimal for robust assembler compatibility.
        if (i != 0) try out.appendSlice(allocator, ", "); // Separate byte values with comma and space.
        try appendFmt(out, allocator, "{d}", .{b}); // Emit current byte numeric literal.
    } // End byte emission loop.
} // End appendDbEscapedString.

fn appendLine(out: *std.ArrayList(u8), allocator: std.mem.Allocator, text: []const u8) !void { // appendLine appends text plus newline to output buffer.
    try out.appendSlice(allocator, text); // Append provided text bytes.
    try out.append(allocator, '\n'); // Append line terminator.
} // End appendLine.

fn appendFmt(out: *std.ArrayList(u8), allocator: std.mem.Allocator, comptime fmt: []const u8, args: anytype) !void { // appendFmt appends formatted text into output buffer.
    const s = try std.fmt.allocPrint(allocator, fmt, args); // Allocate temporary formatted string via allocator.
    defer allocator.free(s); // Free temporary formatted buffer after append.
    try out.appendSlice(allocator, s); // Append formatted bytes to output buffer.
} // End appendFmt.
