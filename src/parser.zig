const std = @import("std"); // Import std for allocator types and dynamic arrays.
const lexer = @import("lexer.zig"); // Import token types and lexer scanner.
const ast = @import("ast.zig"); // Import AST node definitions used as parser output.

pub const ParseError = error{ // ParseError is finite parser failure set for syntax and semantic shape checks.
    UnexpectedToken, // Encountered token that does not match grammar rule expectation.
    ExpectedLineNumber, // Each BASIC line in compiler mode must start with a numeric line number.
    ExpectedStatement, // A statement keyword was expected but not found.
    ExpectedExpression, // Expression was expected but current token cannot start one.
    ExpectedDataValue, // DATA statement expected numeric literal item.
    InvalidVariable, // Variable token is invalid for this grammar stage.
    InvalidRelOp, // Relational operator token is invalid in IF condition.
} || lexer.LexError || std.mem.Allocator.Error; // Compose parser errors with lexer errors and allocation errors.

pub const Parser = struct { // Parser keeps lexer state and builds AST nodes incrementally.
    allocator: std.mem.Allocator, // Allocator used for AST node creation and slices.
    lex: lexer.Lexer, // Owned lexer instance from which we read token stream.
    current: lexer.Token, // Current token under parser cursor.

    pub fn init(allocator: std.mem.Allocator, source: []const u8) ParseError!Parser { // Build parser and prime first token.
        var lex = lexer.Lexer.init(source); // Initialize lexer with full source input.
        const first = try lex.next(); // Read first token so parser invariant always has `current`.
        return .{ // Return initialized parser state.
            .allocator = allocator, // Save allocator handle.
            .lex = lex, // Save lexer state.
            .current = first, // Set first current token.
        }; // End parser initializer literal.
    } // End init.

    pub fn parseProgram(self: *Parser) ParseError!ast.Program { // Parse full source into AST program root.
        var lines: std.ArrayList(ast.Line) = .empty; // Temporary growable list of parsed AST lines.
        errdefer lines.deinit(self.allocator); // Ensure list buffer is released if parse fails mid-way.

        while (self.current.tag != .eof) { // Continue until explicit EOF token is reached.
            while (self.current.tag == .newline) { // Skip empty lines between numbered BASIC lines.
                try self.advance(); // Consume newline token.
            } // End newline skip loop.

            if (self.current.tag == .eof) break; // Allow trailing newlines before EOF.

            if (self.current.tag != .number) return ParseError.ExpectedLineNumber; // Enforce line-numbered BASIC format.
            const line_tok = self.current; // Capture source position and lexeme for this BASIC line.
            const line_number = self.current.int_value orelse return ParseError.ExpectedLineNumber; // Extract parsed integer value.
            try self.advance(); // Consume line-number token and move to statement start token.

            const stmt = try self.parseStatement(); // Parse one statement for this BASIC line.

            if (self.current.tag == .newline) { // Normal case: line ends with newline token.
                try self.advance(); // Consume newline to prepare next loop iteration.
            } else if (self.current.tag != .eof) { // If not newline and not EOF, line has extra garbage.
                return ParseError.UnexpectedToken; // Reject trailing tokens in same line.
            } // End line-terminator handling.

            try lines.append(self.allocator, .{ // Append parsed line AST record.
                .number = line_number, // Store BASIC line number.
                .stmt = stmt, // Store parsed statement pointer.
                .line = line_tok.line, // Store source line position for diagnostics.
                .column = line_tok.column, // Store source column position for diagnostics.
            }); // End append call.
        } // End parse loop.

        return .{ .lines = try lines.toOwnedSlice(self.allocator) }; // Freeze lines list into owned slice and return root AST.
    } // End parseProgram.

    fn parseStatement(self: *Parser) ParseError!ast.StmtRef { // Parse one statement node according to leading keyword.
        return switch (self.current.tag) { // Dispatch by current keyword token.
            .kw_let => try self.parseLet(), // LET statement parser.
            .kw_print => try self.parsePrint(), // PRINT statement parser.
            .kw_goto => try self.parseGoto(), // GOTO statement parser.
            .kw_if => try self.parseIfThen(), // IF THEN statement parser.
            .kw_data => try self.parseData(), // DATA statement parser.
            .kw_read => try self.parseRead(), // READ statement parser.
            .kw_poke => try self.parsePoke(), // POKE statement parser.
            .kw_call => try self.parseCall(), // CALL statement parser.
            .kw_end => try self.parseEnd(), // END statement parser.
            else => ParseError.ExpectedStatement, // Anything else cannot start valid statement in this stage.
        }; // End statement dispatch.
    } // End parseStatement.

    fn parseLet(self: *Parser) ParseError!ast.StmtRef { // Parse `LET <var> = <expr>`.
        try self.consume(.kw_let); // Consume LET keyword.
        const var_index = try self.parseVariableIndex(); // Parse variable name and map to 0..25 index.
        try self.consume(.equal); // Consume '=' assignment operator.
        const value = try self.parseExpression(); // Parse right-hand side expression.
        return try self.allocStmt(.let_stmt, .{ .let_stmt = .{ .var_index = var_index, .value = value } }); // Build LET node.
    } // End parseLet.

    fn parsePrint(self: *Parser) ParseError!ast.StmtRef { // Parse `PRINT item[, item ...]`.
        try self.consume(.kw_print); // Consume PRINT keyword.

        var items: std.ArrayList(ast.PrintItem) = .empty; // Temporary print-item collector.
        defer items.deinit(self.allocator); // Free temporary buffer on function exit.

        if (self.current.tag == .newline or self.current.tag == .eof) return ParseError.ExpectedExpression; // Require at least one item.

        while (true) { // Parse comma-separated print items.
            if (self.current.tag == .string) { // String-literal item branch.
                const s = self.current.lexeme; // Capture already-unquoted string body.
                try self.advance(); // Consume string token.
                try items.append(self.allocator, .{ .string = s }); // Add string print item.
            } else { // Non-string item is parsed as arithmetic expression.
                const expr = try self.parseExpression(); // Parse expression item.
                try items.append(self.allocator, .{ .expr = expr }); // Add expression print item.
            } // End item kind branch.

            if (self.current.tag != .comma) break; // Stop if next token is not comma separator.
            try self.advance(); // Consume comma and continue with next item.

            if (self.current.tag == .newline or self.current.tag == .eof) return ParseError.ExpectedExpression; // Reject trailing comma.
        } // End print-item loop.

        const owned = try items.toOwnedSlice(self.allocator); // Convert temporary list into owned AST slice.
        return try self.allocStmt(.print_stmt, .{ .print_stmt = .{ .items = owned } }); // Build PRINT statement node.
    } // End parsePrint.

    fn parseGoto(self: *Parser) ParseError!ast.StmtRef { // Parse `GOTO <expr>`.
        try self.consume(.kw_goto); // Consume GOTO keyword.
        const target = try self.parseExpression(); // Parse target expression.
        return try self.allocStmt(.goto_stmt, .{ .goto_stmt = target }); // Build GOTO node.
    } // End parseGoto.

    fn parseIfThen(self: *Parser) ParseError!ast.StmtRef { // Parse `IF <expr> <relop> <expr> THEN <stmt>`.
        try self.consume(.kw_if); // Consume IF keyword.
        const left = try self.parseExpression(); // Parse left condition expression.
        const op = try self.parseRelOp(); // Parse relational operator token.
        const right = try self.parseExpression(); // Parse right condition expression.
        try self.consume(.kw_then); // Consume THEN keyword.
        const then_stmt = try self.parseStatement(); // Parse nested single statement after THEN.
        return try self.allocStmt(.if_then_stmt, .{ .if_then_stmt = .{ .left = left, .op = op, .right = right, .then_stmt = then_stmt } }); // Build IF node.
    } // End parseIfThen.

    fn parseEnd(self: *Parser) ParseError!ast.StmtRef { // Parse `END` statement.
        try self.consume(.kw_end); // Consume END keyword.
        return try self.allocStmt(.end_stmt, .{ .end_stmt = {} }); // Build END node with void payload.
    } // End parseEnd.

    fn parseData(self: *Parser) ParseError!ast.StmtRef { // Parse `DATA n[, n ...]` where values are compile-time numeric literals.
        try self.consume(.kw_data); // Consume DATA keyword.

        var values: std.ArrayList(i32) = .empty; // Temporary collector for DATA list values.
        defer values.deinit(self.allocator); // Free temporary list buffer on parse exit.

        if (self.current.tag == .newline or self.current.tag == .eof) return ParseError.ExpectedDataValue; // Require at least one DATA item.

        while (true) { // Parse comma-separated numeric literal list.
            try values.append(self.allocator, try self.parseDataValue()); // Parse one signed integer and append.
            if (self.current.tag != .comma) break; // End list when comma is absent.
            try self.advance(); // Consume comma separator.
            if (self.current.tag == .newline or self.current.tag == .eof) return ParseError.ExpectedDataValue; // Reject trailing comma with missing value.
        } // End DATA list loop.

        return try self.allocStmt(.data_stmt, .{ .data_stmt = .{ .values = try values.toOwnedSlice(self.allocator) } }); // Build DATA node with owned values slice.
    } // End parseData.

    fn parseRead(self: *Parser) ParseError!ast.StmtRef { // Parse `READ <var>`.
        try self.consume(.kw_read); // Consume READ keyword.
        const var_index = try self.parseVariableIndex(); // Parse target variable to receive next DATA item.
        return try self.allocStmt(.read_stmt, .{ .read_stmt = var_index }); // Build READ node.
    } // End parseRead.

    fn parsePoke(self: *Parser) ParseError!ast.StmtRef { // Parse `POKE <addr_expr>, <value_expr>`.
        try self.consume(.kw_poke); // Consume POKE keyword.
        const addr = try self.parseExpression(); // Parse destination memory address expression.
        try self.consume(.comma); // Consume comma between address and value.
        const value = try self.parseExpression(); // Parse value expression.
        return try self.allocStmt(.poke_stmt, .{ .poke_stmt = .{ .addr = addr, .value = value } }); // Build POKE node.
    } // End parsePoke.

    fn parseCall(self: *Parser) ParseError!ast.StmtRef { // Parse `CALL <expr>`.
        try self.consume(.kw_call); // Consume CALL keyword.
        const callee = try self.parseExpression(); // Parse CALL id expression.
        return try self.allocStmt(.call_stmt, .{ .call_stmt = callee }); // Build CALL node.
    } // End parseCall.

    fn parseExpression(self: *Parser) ParseError!ast.ExprRef { // Parse additive-precedence expression.
        var expr = try self.parseTerm(); // Parse first multiplicative term.
        while (self.current.tag == .plus or self.current.tag == .minus) { // Fold left-associative + and - chain.
            const op_tag = self.current.tag; // Capture operator token kind.
            try self.advance(); // Consume operator token.
            const right = try self.parseTerm(); // Parse right-side term.
            const op: ast.BinaryOp = if (op_tag == .plus) .add else .sub; // Map token to AST binary operator.
            expr = try self.allocExpr(.binary, .{ .binary = .{ .op = op, .left = expr, .right = right } }); // Create new binary node and rebind expr.
        } // End additive loop.
        return expr; // Return completed expression tree.
    } // End parseExpression.

    fn parseTerm(self: *Parser) ParseError!ast.ExprRef { // Parse multiplicative-precedence term.
        var expr = try self.parseUnary(); // Parse first unary factor.
        while (self.current.tag == .star or self.current.tag == .slash) { // Fold left-associative * and / chain.
            const op_tag = self.current.tag; // Capture operator token kind.
            try self.advance(); // Consume operator token.
            const right = try self.parseUnary(); // Parse right unary factor.
            const op: ast.BinaryOp = if (op_tag == .star) .mul else .div; // Map token to AST operator.
            expr = try self.allocExpr(.binary, .{ .binary = .{ .op = op, .left = expr, .right = right } }); // Create binary node.
        } // End multiplicative loop.
        return expr; // Return term expression tree.
    } // End parseTerm.

    fn parseUnary(self: *Parser) ParseError!ast.ExprRef { // Parse unary prefix operators.
        if (self.current.tag == .plus or self.current.tag == .minus) { // Unary operator branch.
            const op_tag = self.current.tag; // Capture unary operator token.
            try self.advance(); // Consume unary operator token.
            const inner = try self.parseUnary(); // Parse operand recursively (right-associative unary chain).
            const op: ast.UnaryOp = if (op_tag == .plus) .plus else .minus; // Map token to AST unary op.
            return try self.allocExpr(.unary, .{ .unary = .{ .op = op, .expr = inner } }); // Build unary expression node.
        } // End unary branch.
        return self.parsePrimary(); // Fallback to primary expression parser.
    } // End parseUnary.

    fn parsePrimary(self: *Parser) ParseError!ast.ExprRef { // Parse expression atom.
        return switch (self.current.tag) { // Dispatch by current token kind.
            .number => blk: { // Number literal branch.
                const value = self.current.int_value orelse return ParseError.ExpectedExpression; // Extract numeric value.
                try self.advance(); // Consume number token.
                break :blk try self.allocExpr(.number, .{ .number = value }); // Build number node.
            }, // End number branch.
            .identifier => blk: { // Variable reference branch.
                if (std.ascii.eqlIgnoreCase(self.current.lexeme, "PEEK")) { // Recognize PEEK keyword-like intrinsic as primary expression.
                    try self.advance(); // Consume identifier token that spelled PEEK.
                    try self.consume(.lparen); // Consume opening parenthesis of PEEK call.
                    const arg = try self.parseExpression(); // Parse memory address expression argument.
                    try self.consume(.rparen); // Consume closing parenthesis.
                    break :blk try self.allocExpr(.peek, .{ .peek = arg }); // Build PEEK expression node.
                } // End PEEK intrinsic parse branch.
                const idx = try self.parseVariableIndex(); // Parse variable index from identifier token.
                break :blk try self.allocExpr(.variable, .{ .variable = idx }); // Build variable node.
            }, // End identifier branch.
            .lparen => blk: { // Parenthesized expression branch.
                try self.advance(); // Consume '(' token.
                const inner = try self.parseExpression(); // Parse inner expression.
                try self.consume(.rparen); // Consume ')' token.
                break :blk inner; // Return inner expression directly (no extra AST wrapper).
            }, // End parenthesized branch.
            else => ParseError.ExpectedExpression, // Any other token cannot start primary expression.
        }; // End primary dispatch.
    } // End parsePrimary.

    fn parseRelOp(self: *Parser) ParseError!ast.RelOp { // Parse relational operator token inside IF condition.
        const op = switch (self.current.tag) { // Map token to AST RelOp.
            .equal => ast.RelOp.equal, // '='
            .less => ast.RelOp.less, // '<'
            .less_equal => ast.RelOp.less_equal, // '<='
            .greater => ast.RelOp.greater, // '>'
            .greater_equal => ast.RelOp.greater_equal, // '>='
            .not_equal => ast.RelOp.not_equal, // '<>' or '><'
            else => return ParseError.InvalidRelOp, // Reject non-relational tokens.
        }; // End relational token mapping.
        try self.advance(); // Consume relational operator token after mapping.
        return op; // Return parsed relational operator.
    } // End parseRelOp.

    fn parseVariableIndex(self: *Parser) ParseError!u8 { // Parse single-letter variable token and map to 0..25.
        if (self.current.tag != .identifier) return ParseError.InvalidVariable; // Variable must be identifier token.
        const name = self.current.lexeme; // Capture identifier text bytes.
        if (name.len != 1) return ParseError.InvalidVariable; // Enforce one-letter variable rule in this stage.
        const c = name[0]; // Read single variable character.
        if (!isAlpha(c)) return ParseError.InvalidVariable; // Ensure identifier char is alphabetic.
        const idx: u8 = @intCast(std.ascii.toUpper(c) - 'A'); // Convert A..Z/a..z into 0..25.
        try self.advance(); // Consume variable identifier token.
        return idx; // Return variable index.
    } // End parseVariableIndex.

    fn parseDataValue(self: *Parser) ParseError!i32 { // Parse one signed DATA integer literal item.
        var negate = false; // Track leading unary sign for DATA number.
        if (self.current.tag == .minus) { // Leading '-' means negative DATA literal.
            negate = true; // Mark sign as negative.
            try self.advance(); // Consume unary minus token.
        } else if (self.current.tag == .plus) { // Leading '+' is accepted and ignored.
            try self.advance(); // Consume unary plus token.
        } // End optional sign handling.

        if (self.current.tag != .number) return ParseError.ExpectedDataValue; // DATA values must be numeric literals at this stage.
        const raw = self.current.int_value orelse return ParseError.ExpectedDataValue; // Extract parsed numeric token value.
        try self.advance(); // Consume numeric token after reading value.

        if (!negate) return raw; // Fast path for non-negative values.

        const wide = -@as(i64, raw); // Compute negative form in wider integer type to guard narrowing.
        if (wide < std.math.minInt(i32) or wide > std.math.maxInt(i32)) return ParseError.ExpectedDataValue; // Reject out-of-range value.
        return @intCast(wide); // Return safely narrowed signed i32 value.
    } // End parseDataValue.

    fn consume(self: *Parser, tag: lexer.TokenTag) ParseError!void { // Require exact current token tag and advance.
        if (self.current.tag != tag) return ParseError.UnexpectedToken; // Fail if expected tag does not match.
        try self.advance(); // Consume expected token and move forward.
    } // End consume.

    fn advance(self: *Parser) ParseError!void { // Move parser cursor to next token from lexer.
        self.current = try self.lex.next(); // Read and store next token.
    } // End advance.

    fn allocStmt(self: *Parser, tag: ast.StmtTag, data: ast.StmtData) ParseError!ast.StmtRef { // Allocate AST statement node.
        const node = try self.allocator.create(ast.Stmt); // Allocate statement object on heap.
        node.* = .{ .tag = tag, .data = data }; // Initialize statement object fields.
        return node; // Return statement pointer.
    } // End allocStmt.

    fn allocExpr(self: *Parser, tag: ast.ExprTag, data: ast.ExprData) ParseError!ast.ExprRef { // Allocate AST expression node.
        const node = try self.allocator.create(ast.Expr); // Allocate expression object on heap.
        node.* = .{ .tag = tag, .data = data }; // Initialize expression object fields.
        return node; // Return expression pointer.
    } // End allocExpr.
}; // End Parser struct.

fn isAlpha(c: u8) bool { // Local alphabetic helper matching lexer behavior.
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z'); // Return true for ASCII letters only.
} // End isAlpha helper.

pub fn parseProgram(allocator: std.mem.Allocator, source: []const u8) ParseError!ast.Program { // Convenience top-level parse function.
    var p = try Parser.init(allocator, source); // Initialize parser with source and first token.
    return p.parseProgram(); // Parse full program and return AST root.
} // End parseProgram convenience function.
