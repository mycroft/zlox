const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Obj = @import("./object.zig").Obj;
const ObjType = @import("./object.zig").ObjType;

const OpCode = @import("./opcode.zig").OpCode;
const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Chunk = @import("./chunk.zig").Chunk;
const Value = @import("./values.zig").Value;
const VM = @import("./vm.zig").VM;

const ParsingError = @import("./errors.zig").ParsingError;

const Precedence = enum {
    None,
    Assignement,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
};

const ParserRule = struct {
    prefix: ?*const fn (*Parser) ParsingError!void,
    infix: ?*const fn (*Parser) ParsingError!void,
    precedence: Precedence,
};

const Parser = struct {
    current: ?Token,
    previous: ?Token,
    scanner: *Scanner,
    had_error: bool,
    panic_mode: bool,
    chunk: *Chunk,
    vm: *VM,

    fn new(vm: *VM, scanner: *Scanner, chunk: *Chunk) Parser {
        return Parser{
            .current = null,
            .previous = null,
            .scanner = scanner,
            .had_error = false,
            .panic_mode = false,
            .chunk = chunk,
            .vm = vm,
        };
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scan_token();
            if (self.current.?.token_type != TokenType.ERROR) {
                break;
            }

            self.error_at_current(self.current.?.start);
        }
    }

    fn expression(self: *Parser) ParsingError!void {
        try self.parse_precedence(Precedence.Assignement);
    }

    fn consume(self: *Parser, token_type: TokenType, error_message: []const u8) void {
        if (self.current.?.token_type == token_type) {
            self.advance();
            return;
        }

        self.error_at_current(error_message);
    }

    fn error_at_current(self: *Parser, error_message: []const u8) void {
        self.error_at(self.current.?, error_message);
    }

    fn error_at(self: *Parser, token: Token, error_message: []const u8) void {
        if (self.panic_mode) {
            return;
        }

        self.panic_mode = true;

        debug.print("[line {d}] Error", .{token.line});
        if (token.token_type == TokenType.EOF) {
            debug.print(" at end", .{});
        } else if (token.token_type == TokenType.ERROR) {
            // Nothing
        } else {
            const expr = std.mem.trimRight(u8, token.start[0..token.length], "\n");
            debug.print(" at '{s}'", .{expr});
        }
        debug.print(": {s}\n", .{error_message});
        self.had_error = true;
    }

    fn error_msg(self: *Parser, error_message: []const u8) void {
        self.error_at(self.previous.?, error_message);
    }

    fn emit_byte(self: *Parser, byte: u8) ParsingError!void {
        self.chunk.write(byte, self.previous.?.line) catch |err| {
            switch (err) {
                error.OutOfMemory => return ParsingError.OutOfMemory,
            }
        };
    }

    fn emit_bytes(self: *Parser, byte0: u8, byte1: u8) ParsingError!void {
        try self.emit_byte(byte0);
        try self.emit_byte(byte1);
    }

    fn emit_return(self: *Parser) ParsingError!void {
        try self.emit_byte(@intFromEnum(OpCode.OP_RETURN));
    }

    fn end_parser(self: *Parser) !void {
        if (!self.had_error and self.vm.has_tracing()) {
            self.chunk.dissassemble("code");
        }
        try self.emit_return();
    }

    fn number(self: *Parser) ParsingError!void {
        const value = std.fmt.parseFloat(f64, self.previous.?.start[0..self.previous.?.length]) catch {
            self.error_msg("Failed converting float.");
            return ParsingError.FloatConv;
        };
        self.emit_constant(Value.number_val(value)) catch |err| {
            self.error_msg("Failed emiting constant.");
            return switch (err) {
                error.OutOfMemory => ParsingError.OutOfMemory,
                else => ParsingError.Unknown,
            };
        };
    }

    fn emit_constant(self: *Parser, value: Value) !void {
        const constant = try self.make_constant(value);
        try self.emit_bytes(@intFromEnum(OpCode.OP_CONSTANT), constant);
    }

    fn make_constant(self: *Parser, value: Value) !u8 {
        const constant = try self.chunk.add_constant(value);
        if (constant > 256) {
            self.error_msg("Too many constants in one chunk.");
            return 0;
        }
        return @intCast(constant);
    }

    fn grouping(self: *Parser) ParsingError!void {
        try self.expression();
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Parser) ParsingError!void {
        const operation_type = self.previous.?.token_type;

        // Compile the operand
        try self.parse_precedence(Precedence.Unary);

        // Emit the operator instruction
        switch (operation_type) {
            TokenType.MINUS => self.emit_byte(@intFromEnum(OpCode.OP_NEGATE)) catch |err| {
                self.error_msg("Failed emiting NEGATE opcode.");
                return switch (err) {
                    error.OutOfMemory => ParsingError.OutOfMemory,
                    else => ParsingError.Unknown,
                };
            },
            TokenType.BANG => self.emit_byte(@intFromEnum(OpCode.OP_NOT)) catch |err| {
                self.error_msg("Failed emiting NOT opcode.");
                return switch (err) {
                    error.OutOfMemory => ParsingError.OutOfMemory,
                    else => ParsingError.Unknown,
                };
            },
            else => {},
        }
    }

    fn binary(self: *Parser) ParsingError!void {
        const operator_type = self.previous.?.token_type;
        const parser_rule = Parser.get_rule(operator_type);

        try self.parse_precedence(@enumFromInt(1 + @intFromEnum(parser_rule.precedence)));

        return switch (operator_type) {
            TokenType.BANG_EQUAL => self.emit_bytes(@intFromEnum(OpCode.OP_EQUAL), @intFromEnum(OpCode.OP_NOT)),
            TokenType.EQUAL_EQUAL => self.emit_byte(@intFromEnum(OpCode.OP_EQUAL)),
            TokenType.GREATER => self.emit_byte(@intFromEnum(OpCode.OP_GREATER)),
            TokenType.GREATER_EQUAL => self.emit_bytes(@intFromEnum(OpCode.OP_LESS), @intFromEnum(OpCode.OP_NOT)),
            TokenType.LESS => self.emit_byte(@intFromEnum(OpCode.OP_LESS)),
            TokenType.LESS_EQUAL => self.emit_bytes(@intFromEnum(OpCode.OP_GREATER), @intFromEnum(OpCode.OP_NOT)),
            TokenType.PLUS => self.emit_byte(@intFromEnum(OpCode.OP_ADD)),
            TokenType.MINUS => self.emit_byte(@intFromEnum(OpCode.OP_SUBSTRACT)),
            TokenType.STAR => self.emit_byte(@intFromEnum(OpCode.OP_MULTIPLY)),
            TokenType.SLASH => self.emit_byte(@intFromEnum(OpCode.OP_DIVIDE)),
            else => return,
        };
    }

    fn get_rule(operator_type: TokenType) ParserRule {
        return switch (operator_type) {
            TokenType.LEFT_PAREN => ParserRule{ .prefix = grouping, .infix = null, .precedence = Precedence.None },
            TokenType.RIGHT_PAREN => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.LEFT_BRACE => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.RIGHT_BRACE => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.COMMA => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.DOT => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.MINUS => ParserRule{ .prefix = unary, .infix = binary, .precedence = Precedence.Term },
            TokenType.PLUS => ParserRule{ .prefix = null, .infix = binary, .precedence = Precedence.Term },
            TokenType.SEMICOLON => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.SLASH => ParserRule{ .prefix = null, .infix = binary, .precedence = Precedence.Factor },
            TokenType.STAR => ParserRule{ .prefix = null, .infix = binary, .precedence = Precedence.Factor },
            TokenType.BANG => ParserRule{ .prefix = unary, .infix = null, .precedence = Precedence.None },
            TokenType.BANG_EQUAL => ParserRule{ .prefix = null, .infix = binary, .precedence = Precedence.Equality },
            TokenType.EQUAL => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.EQUAL_EQUAL => ParserRule{ .prefix = null, .infix = binary, .precedence = Precedence.Equality },
            TokenType.GREATER => ParserRule{ .prefix = null, .infix = binary, .precedence = Precedence.Comparison },
            TokenType.GREATER_EQUAL => ParserRule{ .prefix = null, .infix = binary, .precedence = Precedence.Comparison },
            TokenType.LESS => ParserRule{ .prefix = null, .infix = binary, .precedence = Precedence.Comparison },
            TokenType.LESS_EQUAL => ParserRule{ .prefix = null, .infix = binary, .precedence = Precedence.Comparison },
            TokenType.IDENTIFIER => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.STRING => ParserRule{ .prefix = string, .infix = null, .precedence = Precedence.None },
            TokenType.NUMBER => ParserRule{ .prefix = number, .infix = null, .precedence = Precedence.None },
            TokenType.AND => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.CLASS => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.ELSE => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.FALSE => ParserRule{ .prefix = literal, .infix = null, .precedence = Precedence.None },
            TokenType.FOR => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.FUN => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.IF => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.NIL => ParserRule{ .prefix = literal, .infix = null, .precedence = Precedence.None },
            TokenType.OR => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.PRINT => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.RETURN => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.SUPER => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.THIS => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.TRUE => ParserRule{ .prefix = literal, .infix = null, .precedence = Precedence.None },
            TokenType.VAR => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.WHILE => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.ERROR => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.EOF => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
        };
    }

    fn parse_precedence(self: *Parser, precedence: Precedence) ParsingError!void {
        self.advance();

        const prefix_rule = Parser.get_rule(self.previous.?.token_type).prefix;
        if (prefix_rule == null) {
            self.error_msg("Expect expression.");
            return;
        }

        try prefix_rule.?(self);

        while (@intFromEnum(precedence) <= @intFromEnum(Parser.get_rule(self.current.?.token_type).precedence)) {
            self.advance();
            const infix_rule = Parser.get_rule(self.previous.?.token_type).infix;
            try infix_rule.?(self);
        }
    }

    fn literal(self: *Parser) ParsingError!void {
        try switch (self.previous.?.token_type) {
            TokenType.NIL => self.emit_byte(@intFromEnum(OpCode.OP_NIL)),
            TokenType.TRUE => self.emit_byte(@intFromEnum(OpCode.OP_TRUE)),
            TokenType.FALSE => self.emit_byte(@intFromEnum(OpCode.OP_FALSE)),
            else => unreachable,
        };
    }

    fn string(self: *Parser) ParsingError!void {
        const str = self.previous.?.start[1 .. self.previous.?.length - 1];

        var string_obj = self.vm.copy_string(str);

        self.vm.add_reference(&string_obj.obj);

        try self.emit_constant(Value.obj_val(&string_obj.obj));
    }
};

pub fn compile(allocator: Allocator, vm: *VM, contents: []const u8, chunk: *Chunk) !bool {
    _ = allocator;

    var scanner = Scanner.init(contents);
    var parser = Parser.new(vm, &scanner, chunk);

    parser.advance();
    try parser.expression();
    parser.consume(TokenType.EOF, "Expect end of expression.");
    try parser.end_parser();

    return !parser.had_error;
}
