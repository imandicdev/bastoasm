// =============================================================================
// Project : bstoasm
// File    : bstoasm\src\lexer.zig
// Author  : Ilija Mandic
// Purpose : Lexer for line-numbered BASIC input source.
// =============================================================================
const std = @import("std"); // Import Zig standard library for strings, ascii helpers, maps, and errors.

pub const TokenTag = enum { // TokenTag defines all token kinds that parser can receive.
    eof, // End-of-file marker.
    newline, // Line break marker; BASIC is line-oriented so newline is meaningful.

    identifier, // Generic identifier (variable name or unknown word).
    number, // Decimal integer number.
    string, // Quoted string literal without surrounding quotes.

    kw_let, // LET keyword.
    kw_print, // PRINT keyword.
    kw_if, // IF keyword.
    kw_then, // THEN keyword.
    kw_goto, // GOTO keyword.
    kw_data, // DATA keyword.
    kw_read, // READ keyword.
    kw_poke, // POKE keyword.
    kw_call, // CALL keyword.
    kw_end, // END keyword.

    plus, // '+' operator.
    minus, // '-' operator.
    star, // '*' operator.
    slash, // '/' operator.
    equal, // '=' operator.
    less, // '<' operator.
    greater, // '>' operator.
    less_equal, // '<=' operator.
    greater_equal, // '>=' operator.
    not_equal, // '<>' or '><' operator.
    comma, // ',' separator.
    lparen, // '(' punctuation.
    rparen, // ')' punctuation.
}; // End TokenTag enum.

pub const Token = struct { // Token carries token type, exact text slice, and source position.
    tag: TokenTag, // Category of token.
    lexeme: []const u8, // Exact source slice for this token (for diagnostics and debug).
    line: u32, // 1-based source line where token starts.
    column: u32, // 1-based source column where token starts.
    int_value: ?i32 = null, // Parsed numeric value when tag is .number; null otherwise.
}; // End Token struct.

pub const LexError = error{ // LexError is the finite set of lexer failures.
    InvalidCharacter, // Source contains a character we do not support in this stage.
    UnterminatedString, // String literal started with '"' but not closed before line/end.
    NumberTooLarge, // Decimal number cannot fit in i32.
}; // End LexError error set.

pub const Lexer = struct { // Lexer is a stateful scanner over source bytes.
    source: []const u8, // Full source buffer being scanned.
    index: usize, // Current absolute byte index into source.
    line: u32, // Current 1-based line number.
    column: u32, // Current 1-based column number.

    pub fn init(source: []const u8) Lexer { // Construct a new lexer at start of source.
        return .{ // Return a fully initialized lexer value.
            .source = source, // Store source slice.
            .index = 0, // Start scanning at first byte.
            .line = 1, // First line is line 1.
            .column = 1, // First column is column 1.
        }; // End initializer literal.
    } // End init.

    pub fn next(self: *Lexer) LexError!Token { // Scan and return next token from current cursor.
        self.skipHorizontalWhitespace(); // Consume spaces and tabs, but keep newlines as tokens.

        const start_index = self.index; // Capture token start byte index for lexeme slicing.
        const start_line = self.line; // Capture token start line for diagnostics.
        const start_col = self.column; // Capture token start column for diagnostics.

        if (self.index >= self.source.len) { // If cursor is past source end, emit EOF token.
            return .{ .tag = .eof, .lexeme = "", .line = start_line, .column = start_col }; // EOF token.
        } // End EOF guard.

        const c = self.source[self.index]; // Read current byte without advancing yet.

        if (c == '\n' or c == '\r') { // Newline branch supports both Unix and Windows line endings.
            self.consumeNewline(); // Advance over newline sequence and update line/column.
            return .{ // Emit newline token with original slice bytes.
                .tag = .newline, // Token kind is newline.
                .lexeme = self.source[start_index..self.index], // Slice includes '\n' or '\r\n'.
                .line = start_line, // Start line.
                .column = start_col, // Start column.
            }; // End newline token.
        } // End newline handling.

        if (isAlpha(c)) { // Identifier/keyword starts with alphabetic character.
            _ = self.advance(); // Consume first identifier character.
            while (self.peek()) |p| { // Continue while we still have input.
                if (!isIdentChar(p)) break; // Stop on first non-identifier byte.
                _ = self.advance(); // Consume identifier byte.
            } // End identifier loop.
            const lexeme = self.source[start_index..self.index]; // Slice identifier text.
            const tag = keywordTag(lexeme) orelse .identifier; // Convert to keyword token if reserved word.
            return .{ .tag = tag, .lexeme = lexeme, .line = start_line, .column = start_col }; // Emit token.
        } // End identifier/keyword branch.

        if (std.ascii.isDigit(c)) { // Number branch for decimal integers.
            const value = try self.readNumber(); // Parse full decimal sequence with overflow check.
            const lexeme = self.source[start_index..self.index]; // Slice number text.
            return .{ // Emit number token with parsed value.
                .tag = .number, // Token kind is number.
                .lexeme = lexeme, // Original number bytes.
                .line = start_line, // Start line.
                .column = start_col, // Start column.
                .int_value = value, // Parsed i32 value.
            }; // End number token.
        } // End number branch.

        switch (c) { // Single/double-character symbol dispatch.
            '"' => { // Quoted string branch.
                const s = try self.readString(); // Parse string body until closing quote.
                return .{ .tag = .string, .lexeme = s, .line = start_line, .column = start_col }; // Emit string.
            }, // End string case.
            '+' => { // Plus operator case.
                _ = self.advance(); // Consume '+'.
                return .{ .tag = .plus, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit plus.
            }, // End plus case.
            '-' => { // Minus operator case.
                _ = self.advance(); // Consume '-'.
                return .{ .tag = .minus, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit minus.
            }, // End minus case.
            '*' => { // Multiply operator case.
                _ = self.advance(); // Consume '*'.
                return .{ .tag = .star, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit star.
            }, // End star case.
            '/' => { // Divide operator case.
                _ = self.advance(); // Consume '/'.
                return .{ .tag = .slash, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit slash.
            }, // End slash case.
            '=' => { // Equal operator case.
                _ = self.advance(); // Consume '='.
                return .{ .tag = .equal, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit equal.
            }, // End equal case.
            ',' => { // Comma case.
                _ = self.advance(); // Consume ','.
                return .{ .tag = .comma, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit comma.
            }, // End comma case.
            '(' => { // Left parenthesis case.
                _ = self.advance(); // Consume '('.
                return .{ .tag = .lparen, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit lparen.
            }, // End lparen case.
            ')' => { // Right parenthesis case.
                _ = self.advance(); // Consume ')'.
                return .{ .tag = .rparen, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit rparen.
            }, // End rparen case.
            '<' => { // Relational operator starting with '<'.
                _ = self.advance(); // Consume '<'.
                if (self.match('=')) { // Check "<=".
                    return .{ .tag = .less_equal, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit <=.
                } // End <= check.
                if (self.match('>')) { // Check "<>".
                    return .{ .tag = .not_equal, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit <>.
                } // End <> check.
                return .{ .tag = .less, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit <.
            }, // End '<' case.
            '>' => { // Relational operator starting with '>'.
                _ = self.advance(); // Consume '>'.
                if (self.match('=')) { // Check ">=".
                    return .{ .tag = .greater_equal, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit >=.
                } // End >= check.
                if (self.match('<')) { // Check "><" (accepted as not equal).
                    return .{ .tag = .not_equal, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit ><.
                } // End >< check.
                return .{ .tag = .greater, .lexeme = self.source[start_index..self.index], .line = start_line, .column = start_col }; // Emit >.
            }, // End '>' case.
            else => return LexError.InvalidCharacter, // Unknown byte cannot be tokenized.
        } // End switch.
    } // End next.

    fn skipHorizontalWhitespace(self: *Lexer) void { // Consume spaces and tabs but not newlines.
        while (self.index < self.source.len) { // Loop while bytes remain.
            const c = self.source[self.index]; // Read current byte.
            if (c == ' ' or c == '\t') { // Horizontal whitespace set.
                _ = self.advance(); // Consume whitespace.
                continue; // Continue scanning for more horizontal whitespace.
            } // End horizontal whitespace check.
            break; // Stop on any non-space/tab byte.
        } // End whitespace loop.
    } // End skipHorizontalWhitespace.

    fn consumeNewline(self: *Lexer) void { // Consume '\n' or '\r\n' as one logical newline.
        if (self.source[self.index] == '\r') { // Windows-style newline may start with '\r'.
            _ = self.advanceRaw(); // Consume '\r' without column accounting.
            if (self.index < self.source.len and self.source[self.index] == '\n') { // Optional following '\n'.
                _ = self.advanceRaw(); // Consume '\n'.
            } // End optional '\n'.
        } else { // Otherwise expected '\n'.
            _ = self.advanceRaw(); // Consume '\n'.
        } // End newline form branch.
        self.line += 1; // Move to next logical line.
        self.column = 1; // Reset column to first column of new line.
    } // End consumeNewline.

    fn readString(self: *Lexer) LexError![]const u8 { // Parse quoted string and return inner content.
        _ = self.advance(); // Consume opening quote '"'.
        const start = self.index; // Mark start of string body.
        while (self.index < self.source.len and self.source[self.index] != '"') { // Scan until closing quote.
            if (self.source[self.index] == '\n' or self.source[self.index] == '\r') { // Strings may not cross lines in v1.
                return LexError.UnterminatedString; // Error on newline before closing quote.
            } // End newline-in-string check.
            _ = self.advance(); // Consume non-quote character.
        } // End string scanning loop.
        if (self.index >= self.source.len) return LexError.UnterminatedString; // EOF before closing quote.
        const slice = self.source[start..self.index]; // Slice string body bytes.
        _ = self.advance(); // Consume closing quote '"'.
        return slice; // Return body (without quotes).
    } // End readString.

    fn readNumber(self: *Lexer) LexError!i32 { // Parse decimal integer with overflow check.
        var value: i32 = 0; // Running accumulator.
        while (self.index < self.source.len and std.ascii.isDigit(self.source[self.index])) { // Scan consecutive digits.
            const digit: i32 = @intCast(self.source[self.index] - '0'); // Convert ascii digit byte to numeric value.
            if (value > @divTrunc((@as(i32, std.math.maxInt(i32)) - digit), 10)) { // Guard before value*10+digit.
                return LexError.NumberTooLarge; // Number exceeds i32 range.
            } // End overflow guard.
            value = value * 10 + digit; // Update accumulator.
            _ = self.advance(); // Consume digit.
        } // End digit loop.
        return value; // Return parsed decimal value.
    } // End readNumber.

    fn advance(self: *Lexer) u8 { // Consume one byte and update column.
        const c = self.source[self.index]; // Read byte at current cursor.
        self.index += 1; // Move cursor forward.
        self.column += 1; // Advance column counter for regular characters.
        return c; // Return consumed byte.
    } // End advance.

    fn advanceRaw(self: *Lexer) u8 { // Consume one byte without touching column accounting.
        const c = self.source[self.index]; // Read byte at current cursor.
        self.index += 1; // Move cursor forward.
        return c; // Return consumed byte.
    } // End advanceRaw.

    fn peek(self: *Lexer) ?u8 { // Non-consuming lookahead of current byte.
        if (self.index >= self.source.len) return null; // Null when at or beyond EOF.
        return self.source[self.index]; // Return current byte when available.
    } // End peek.

    fn match(self: *Lexer, expected: u8) bool { // Consume byte only if it matches expected.
        if (self.index >= self.source.len) return false; // Cannot match beyond EOF.
        if (self.source[self.index] != expected) return false; // Mismatch case.
        _ = self.advance(); // Consume expected byte on match.
        return true; // Report success.
    } // End match.
}; // End Lexer struct.

const keyword_map = std.StaticStringMap(TokenTag).initComptime(.{ // Compile-time map from uppercase text to keyword tags.
    .{ "LET", .kw_let }, // LET keyword entry.
    .{ "PRINT", .kw_print }, // PRINT keyword entry.
    .{ "IF", .kw_if }, // IF keyword entry.
    .{ "THEN", .kw_then }, // THEN keyword entry.
    .{ "GOTO", .kw_goto }, // GOTO keyword entry.
    .{ "DATA", .kw_data }, // DATA keyword entry.
    .{ "READ", .kw_read }, // READ keyword entry.
    .{ "POKE", .kw_poke }, // POKE keyword entry.
    .{ "CALL", .kw_call }, // CALL keyword entry.
    .{ "END", .kw_end }, // END keyword entry.
}); // End keyword map literal.

fn keywordTag(lexeme: []const u8) ?TokenTag { // Try to classify identifier text as keyword.
    var buf: [16]u8 = undefined; // Temporary uppercase buffer.
    if (lexeme.len == 0 or lexeme.len > buf.len) return null; // Reject empty/too-long lexeme for keyword lookup.
    for (lexeme, 0..) |c, i| { // Copy and uppercase lexeme bytes.
        buf[i] = std.ascii.toUpper(c); // Case-insensitive BASIC keyword handling.
    } // End uppercase copy loop.
    return keyword_map.get(buf[0..lexeme.len]); // Return keyword tag or null if not reserved.
} // End keywordTag.

fn isAlpha(c: u8) bool { // ASCII alphabetic predicate.
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z'); // True for A-Z or a-z.
} // End isAlpha.

fn isIdentChar(c: u8) bool { // Identifier continuation predicate.
    return isAlpha(c) or std.ascii.isDigit(c); // Identifier chars are letters or digits in v1.
} // End isIdentChar.
