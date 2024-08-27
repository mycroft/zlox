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

const identifiers_equals = @import("./utils.zig").identifiers_equals;

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
    prefix: ?*const fn (*Parser, bool) ParsingError!void,
    infix: ?*const fn (*Parser, bool) ParsingError!void,
    precedence: Precedence,
};

const Parser = struct {
    compiler: *Compiler,
    current: ?Token,
    previous: ?Token,
    scanner: *Scanner,
    had_error: bool,
    panic_mode: bool,
    chunk: *Chunk,
    vm: *VM,

    fn new(vm: *VM, compiler: *Compiler, scanner: *Scanner, chunk: *Chunk) Parser {
        return Parser{
            .compiler = compiler,
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

    fn number(self: *Parser, can_assign: bool) ParsingError!void {
        _ = can_assign;

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

    fn grouping(self: *Parser, can_assign: bool) ParsingError!void {
        _ = can_assign;

        try self.expression();
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Parser, can_assign: bool) ParsingError!void {
        _ = can_assign;

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

    fn binary(self: *Parser, can_assign: bool) ParsingError!void {
        _ = can_assign;

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
            TokenType.IDENTIFIER => ParserRule{ .prefix = variable, .infix = null, .precedence = Precedence.None },
            TokenType.STRING => ParserRule{ .prefix = string, .infix = null, .precedence = Precedence.None },
            TokenType.NUMBER => ParserRule{ .prefix = number, .infix = null, .precedence = Precedence.None },
            TokenType.AND => ParserRule{ .prefix = null, .infix = and_, .precedence = Precedence.And },
            TokenType.CLASS => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.ELSE => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.FALSE => ParserRule{ .prefix = literal, .infix = null, .precedence = Precedence.None },
            TokenType.FOR => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.FUN => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.IF => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.NIL => ParserRule{ .prefix = literal, .infix = null, .precedence = Precedence.None },
            TokenType.OR => ParserRule{ .prefix = null, .infix = or_, .precedence = Precedence.Or },
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

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignement);
        try prefix_rule.?(self, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(Parser.get_rule(self.current.?.token_type).precedence)) {
            self.advance();
            const infix_rule = Parser.get_rule(self.previous.?.token_type).infix;
            try infix_rule.?(self, can_assign);
        }

        if (can_assign and self.match(TokenType.EQUAL)) {
            self.error_msg("Invalid assignment target.");
        }
    }

    fn literal(self: *Parser, can_assign: bool) ParsingError!void {
        _ = can_assign;

        try switch (self.previous.?.token_type) {
            TokenType.NIL => self.emit_byte(@intFromEnum(OpCode.OP_NIL)),
            TokenType.TRUE => self.emit_byte(@intFromEnum(OpCode.OP_TRUE)),
            TokenType.FALSE => self.emit_byte(@intFromEnum(OpCode.OP_FALSE)),
            else => unreachable,
        };
    }

    fn string(self: *Parser, can_assign: bool) ParsingError!void {
        _ = can_assign;

        const str = self.previous.?.start[1 .. self.previous.?.length - 1];

        var string_obj = self.vm.copy_string(str);

        self.vm.add_reference(&string_obj.obj);

        try self.emit_constant(Value.obj_val(&string_obj.obj));
    }

    fn variable(self: *Parser, can_assign: bool) ParsingError!void {
        try self.named_variable(self.previous.?, can_assign);
    }

    fn named_variable(self: *Parser, token: Token, can_assign: bool) ParsingError!void {
        var get_op: OpCode = OpCode.OP_GET_LOCAL;
        var set_op: OpCode = OpCode.OP_SET_LOCAL;
        var has_local = true;

        var constant = self.resolve_local(token) catch blk: {
            has_local = false;
            break :blk 0;
        };

        if (!has_local) {
            constant = try self.identifier_constant(token);
            get_op = OpCode.OP_GET_GLOBAL;
            set_op = OpCode.OP_SET_GLOBAL;
        }

        if (can_assign and self.match(TokenType.EQUAL)) {
            try self.expression();
            try self.emit_bytes(@intFromEnum(set_op), constant);
        } else {
            try self.emit_bytes(@intFromEnum(get_op), constant);
        }
    }

    fn declaration(self: *Parser) ParsingError!void {
        if (self.match(TokenType.VAR)) {
            try self.var_declaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) {
            self.synchronize();
        }
    }

    fn statement(self: *Parser) ParsingError!void {
        if (self.match(TokenType.PRINT)) {
            try self.print_statement();
        } else if (self.match(TokenType.IF)) {
            try self.if_statement();
        } else if (self.match(TokenType.LEFT_BRACE)) {
            self.begin_scope();
            try self.block();
            try self.end_scope();
        } else {
            try self.expression_statement();
        }
    }

    fn match(self: *Parser, token_type: TokenType) bool {
        if (!self.check(token_type))
            return false;
        self.advance();
        return true;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        return self.current.?.token_type == token_type;
    }

    fn print_statement(self: *Parser) ParsingError!void {
        try self.expression();
        self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
        try self.emit_byte(@intFromEnum(OpCode.OP_PRINT));
    }

    fn expression_statement(self: *Parser) ParsingError!void {
        try self.expression();
        self.consume(TokenType.SEMICOLON, "Expect ';' after value.");
        try self.emit_byte(@intFromEnum(OpCode.OP_POP));
    }

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;

        while (self.current.?.token_type != TokenType.EOF) {
            if (self.previous.?.token_type == TokenType.SEMICOLON) {
                return;
            }

            switch (self.current.?.token_type) {
                TokenType.CLASS,
                TokenType.FUN,
                TokenType.VAR,
                TokenType.FOR,
                TokenType.IF,
                TokenType.WHILE,
                TokenType.PRINT,
                TokenType.RETURN,
                => return,
                else => {},
            }

            self.advance();
        }
    }

    fn var_declaration(self: *Parser) ParsingError!void {
        const global = try self.parse_variable("Expect variable name.");

        if (self.match(TokenType.EQUAL)) {
            try self.expression();
        } else {
            try self.emit_byte(@intFromEnum(OpCode.OP_NIL));
        }
        self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");

        try self.define_variable(global);
    }

    fn parse_variable(self: *Parser, err_msg: []const u8) ParsingError!u8 {
        self.consume(TokenType.IDENTIFIER, err_msg);

        self.declare_variable();
        if (self.compiler.scope_depth > 0) {
            return 0;
        }

        return self.identifier_constant(self.previous.?);
    }

    fn identifier_constant(self: *Parser, token: Token) ParsingError!u8 {
        const copy = &self.vm.copy_string(token.start[0..token.length]).obj;
        self.vm.add_reference(copy);
        return self.make_constant(Value.obj_val(copy));
    }

    fn define_variable(self: *Parser, global: u8) ParsingError!void {
        if (self.compiler.scope_depth > 0) {
            self.mark_initialized();
            return;
        }

        return self.emit_bytes(@intFromEnum(OpCode.OP_DEFINE_GLOBAL), global);
    }

    fn mark_initialized(self: *Parser) void {
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }

    fn declare_variable(self: *Parser) void {
        if (self.compiler.scope_depth == 0) {
            return;
        }
        const name = self.previous.?;

        if (self.compiler.local_count == 0) {
            self.add_local(name);
            return;
        }

        var idx = self.compiler.local_count - 1;

        while (idx >= 0) {
            const local = &self.compiler.locals[idx];

            if (local.depth != null and local.depth.? < self.compiler.scope_depth) {
                break;
            }

            if (identifiers_equals(name, local.name)) {
                self.error_msg("Already a variable with this name in this scope.");
            }

            if (idx == 0) {
                break;
            }
            idx -= 1;
        }

        self.add_local(name);
    }

    fn block(self: *Parser) ParsingError!void {
        while (!self.check(TokenType.RIGHT_BRACE) and !self.check(TokenType.EOF)) {
            try self.declaration();
        }

        self.consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
    }

    fn begin_scope(self: *Parser) void {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(self: *Parser) !void {
        self.compiler.scope_depth -= 1;

        while (self.compiler.local_count > 0 and self.compiler.locals[self.compiler.local_count - 1].depth.? > self.compiler.scope_depth) {
            try self.emit_byte(@intFromEnum(OpCode.OP_POP));
            self.compiler.local_count -= 1;
        }
    }

    fn add_local(self: *Parser, token: Token) void {
        if (self.compiler.local_count == 256) {
            self.error_msg("Too many local variables in function.");
            return;
        }

        var local = &self.compiler.locals[self.compiler.local_count];
        self.compiler.local_count += 1;

        local.name = token;
        local.depth = null;
    }

    fn resolve_local(self: *Parser, name: Token) !u8 {
        if (self.compiler.local_count == 0) {
            return ParsingError.NotFound;
        }

        var idx: u8 = @intCast(self.compiler.local_count - 1);

        while (idx >= 0) {
            const local = &self.compiler.locals[idx];

            if (identifiers_equals(local.name, name)) {
                if (local.depth == null) {
                    self.error_msg("Can't read local variable in its own initializer.");
                }
                return idx;
            }

            if (idx == 0) {
                break;
            }
            idx -= 1;
        }

        return ParsingError.NotFound;
    }

    fn if_statement(self: *Parser) !void {
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
        try self.expression();
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

        const then_jump = try self.emit_jump(@intFromEnum(OpCode.OP_JUMP_IF_FALSE));
        try self.emit_byte(@intFromEnum(OpCode.OP_POP));
        try self.statement();
        const else_jump = try self.emit_jump(@intFromEnum(OpCode.OP_JUMP));

        self.patch_jump(then_jump);
        try self.emit_byte(@intFromEnum(OpCode.OP_POP));

        if (self.match(TokenType.ELSE)) {
            try self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn emit_jump(self: *Parser, instruction: u8) ParsingError!usize {
        try self.emit_byte(instruction);
        try self.emit_byte(0xff);
        try self.emit_byte(0xff);

        return self.chunk.count - 2;
    }

    fn patch_jump(self: *Parser, offset: usize) void {
        const jump = self.chunk.count - offset - 2;

        if (jump > 65535) {
            self.error_msg("Too much code to jump over.");
        }

        const b1 = (jump >> 8) & 0xff;
        const b0 = jump & 0xff;

        self.chunk.code[offset] = @intCast(b1);
        self.chunk.code[offset + 1] = @intCast(b0);
    }

    fn and_(self: *Parser, can_assign: bool) ParsingError!void {
        _ = can_assign;
        const end_jump = try self.emit_jump(@intFromEnum(OpCode.OP_JUMP_IF_FALSE));
        try self.emit_byte(@intFromEnum(OpCode.OP_POP));

        try self.parse_precedence(Precedence.And);
        self.patch_jump(end_jump);
    }

    fn or_(self: *Parser, can_assign: bool) ParsingError!void {
        _ = can_assign;
        const else_jump = try self.emit_jump(@intFromEnum(OpCode.OP_JUMP_IF_FALSE));
        const end_jump = try self.emit_jump(@intFromEnum(OpCode.OP_JUMP));

        self.patch_jump(else_jump);
        try self.emit_byte(@intFromEnum(OpCode.OP_POP));

        try self.parse_precedence(Precedence.Or);
        self.patch_jump(end_jump);
    }
};

const Compiler = struct {
    locals: [256]Local,
    local_count: usize,
    scope_depth: usize,

    fn new() Compiler {
        return Compiler{
            .locals = undefined,
            .local_count = 0,
            .scope_depth = 0,
        };
    }
};

const Local = struct {
    name: Token,
    depth: ?usize,
};

pub fn compile(allocator: Allocator, vm: *VM, contents: []const u8, chunk: *Chunk) !bool {
    _ = allocator;

    var compiler = Compiler.new();

    var scanner = Scanner.init(contents);
    var parser = Parser.new(vm, &compiler, &scanner, chunk);

    parser.advance();

    while (!parser.match(TokenType.EOF)) {
        try parser.declaration();
    }

    try parser.end_parser();

    return !parser.had_error;
}
