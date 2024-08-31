const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const constants = @import("./constant.zig");

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

const ParserFn = *const fn (*Parser, bool) ParsingError!void;

const ParserRule = struct {
    prefix: ?ParserFn,
    infix: ?ParserFn,
    precedence: Precedence,
};

pub const Parser = struct {
    compiler: *Compiler,
    current: ?Token,
    previous: ?Token,
    scanner: *Scanner,
    had_error: bool,
    panic_mode: bool,
    vm: *VM,

    fn new(vm: *VM, compiler: *Compiler, scanner: *Scanner) Parser {
        return Parser{
            .compiler = compiler,
            .current = null,
            .previous = null,
            .scanner = scanner,
            .had_error = false,
            .panic_mode = false,
            .vm = vm,
        };
    }

    inline fn current_chunk(self: *Parser) *Chunk {
        return self.compiler.function.chunk;
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
        self.current_chunk().write(byte, self.previous.?.line) catch |err| {
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
        try self.emit_byte(@intFromEnum(OpCode.OP_NIL));
        try self.emit_byte(@intFromEnum(OpCode.OP_RETURN));
    }

    fn end_parser(self: *Parser) !*Obj.Function {
        try self.emit_return();

        if (!self.had_error and constants.DEBUG_PRINT_CODE) {
            self.current_chunk().dissassemble("code");
        }

        const function_obj = self.compiler.function;

        if (self.compiler.enclosing != null) {
            self.compiler = self.compiler.enclosing.?;
        }

        return function_obj;
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
        const constant = try self.current_chunk().add_constant(value);
        if (constant > constants.UINT8_MAX) {
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
            TokenType.LEFT_PAREN => ParserRule{ .prefix = grouping, .infix = call, .precedence = Precedence.Call },
            TokenType.RIGHT_PAREN => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.LEFT_BRACE => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.RIGHT_BRACE => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.COMMA => ParserRule{ .prefix = null, .infix = null, .precedence = Precedence.None },
            TokenType.DOT => ParserRule{ .prefix = null, .infix = dot, .precedence = Precedence.Call },
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

        const string_obj = self.vm.copy_string(str);

        try self.emit_constant(Value.obj_val(&string_obj.obj));
    }

    fn variable(self: *Parser, can_assign: bool) ParsingError!void {
        try self.named_variable(self.previous.?, can_assign);
    }

    fn named_variable(self: *Parser, token: Token, can_assign: bool) ParsingError!void {
        var get_op: OpCode = OpCode.OP_GET_LOCAL;
        var set_op: OpCode = OpCode.OP_SET_LOCAL;

        var arg = self.resolve_local(self.compiler, token);
        const upvalue_arg = self.resolve_upvalue(self.compiler, token);
        if (arg != -1) {
            get_op = OpCode.OP_GET_LOCAL;
            set_op = OpCode.OP_SET_LOCAL;
        } else if (upvalue_arg != -1) {
            get_op = OpCode.OP_GET_UPVALUE;
            set_op = OpCode.OP_SET_UPVALUE;
            arg = upvalue_arg;
        } else {
            arg = try self.identifier_constant(token);
            get_op = OpCode.OP_GET_GLOBAL;
            set_op = OpCode.OP_SET_GLOBAL;
        }

        if (can_assign and self.match(TokenType.EQUAL)) {
            try self.expression();
            try self.emit_bytes(@intFromEnum(set_op), @intCast(arg));
        } else {
            try self.emit_bytes(@intFromEnum(get_op), @intCast(arg));
        }
    }

    fn declaration(self: *Parser) ParsingError!void {
        if (self.match(TokenType.CLASS)) {
            try self.class_declaration();
        } else if (self.match(TokenType.FUN)) {
            try self.fun_declaration();
        } else if (self.match(TokenType.VAR)) {
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
        } else if (self.match(TokenType.FOR)) {
            try self.for_statement();
        } else if (self.match(TokenType.IF)) {
            try self.if_statement();
        } else if (self.match(TokenType.RETURN)) {
            try self.return_statement();
        } else if (self.match(TokenType.WHILE)) {
            try self.while_statement();
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
        if (self.compiler.scope_depth == 0) {
            return;
        }
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
            if (self.compiler.locals[self.compiler.local_count - 1].is_captured) {
                try self.emit_byte(@intFromEnum(OpCode.OP_CLOSE_UPVALUE));
            } else {
                try self.emit_byte(@intFromEnum(OpCode.OP_POP));
            }
            self.compiler.local_count -= 1;
        }
    }

    fn add_local(self: *Parser, token: Token) void {
        if (self.compiler.local_count == constants.UINT8_COUNT) {
            self.error_msg("Too many local variables in function.");
            return;
        }

        var local = &self.compiler.locals[self.compiler.local_count];
        self.compiler.local_count += 1;

        local.name = token;
        local.depth = null;
        local.is_captured = false;
    }

    fn resolve_local(self: *Parser, compiler: *Compiler, name: Token) isize {
        if (compiler.local_count == 0) {
            return -1;
        }

        var idx: u8 = @intCast(compiler.local_count - 1);

        while (idx >= 0) {
            const local = &compiler.locals[idx];

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

        return -1;
    }

    fn resolve_upvalue(self: *Parser, compiler: *Compiler, name: Token) isize {
        if (compiler.enclosing == null) {
            return -1;
        }

        const local = self.resolve_local(compiler.enclosing.?, name);
        if (local != -1) {
            compiler.enclosing.?.locals[@intCast(local)].is_captured = true;
            return @intCast(self.add_upvalue(compiler, @intCast(local), true));
        }

        const upvalue = self.resolve_upvalue(compiler.enclosing.?, name);
        if (upvalue != -1) {
            return @intCast(self.add_upvalue(compiler, @intCast(upvalue), false));
        }

        return -1;
    }

    fn add_upvalue(self: *Parser, compiler: *Compiler, index: u8, is_local: bool) usize {
        const upvalue_count = compiler.function.upvalue_count;

        for (0..upvalue_count) |i| {
            const upvalue: *Upvalue = &compiler.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (upvalue_count == constants.UINT8_COUNT) {
            self.error_msg("Too many closure variables in function.");
            return 0;
        }

        compiler.upvalues[upvalue_count].is_local = is_local;
        compiler.upvalues[upvalue_count].index = index;
        compiler.function.upvalue_count += 1;
        return compiler.function.upvalue_count - 1;
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

        return self.current_chunk().count - 2;
    }

    fn patch_jump(self: *Parser, offset: usize) void {
        const jump = self.current_chunk().count - offset - 2;

        if (jump > constants.UINT16_MAX) {
            self.error_msg("Too much code to jump over.");
        }

        const b1 = (jump >> 8) & 0xff;
        const b0 = jump & 0xff;

        self.current_chunk().code[offset] = @intCast(b1);
        self.current_chunk().code[offset + 1] = @intCast(b0);
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

    fn while_statement(self: *Parser) ParsingError!void {
        const loop_start = self.current_chunk().count;
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
        try self.expression();
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

        const exit_jump = try self.emit_jump(@intFromEnum(OpCode.OP_JUMP_IF_FALSE));
        try self.emit_byte(@intFromEnum(OpCode.OP_POP));
        try self.statement();
        try self.emit_loop(loop_start);
        self.patch_jump(exit_jump);
        try self.emit_byte(@intFromEnum(OpCode.OP_POP));
    }

    fn emit_loop(self: *Parser, loop_start: usize) ParsingError!void {
        try self.emit_byte(@intFromEnum(OpCode.OP_LOOP));

        const offset = self.current_chunk().count - loop_start + 2;
        if (offset > constants.UINT16_MAX) {
            self.error_msg("Loop body too large.");
        }

        try self.emit_byte(@intCast((offset >> 8) & 0xff));
        try self.emit_byte(@intCast(offset & 0xff));
    }

    fn for_statement(self: *Parser) ParsingError!void {
        self.begin_scope();

        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");
        if (self.match(TokenType.SEMICOLON)) {
            // No initializer
        } else if (self.match(TokenType.VAR)) {
            try self.var_declaration();
        } else {
            try self.expression_statement();
        }

        var loop_start = self.current_chunk().count;

        var exit_jump: ?usize = null;

        if (!self.match(TokenType.SEMICOLON)) {
            try self.expression();
            self.consume(TokenType.SEMICOLON, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false.
            exit_jump = try self.emit_jump(@intFromEnum(OpCode.OP_JUMP_IF_FALSE));
            _ = try self.emit_byte(@intFromEnum(OpCode.OP_POP)); // Condition
        }

        if (!self.match(TokenType.RIGHT_PAREN)) {
            const body_jump = try self.emit_jump(@intFromEnum(OpCode.OP_JUMP));
            const increment_start = self.current_chunk().count;
            try self.expression();
            try self.emit_byte(@intFromEnum(OpCode.OP_POP));
            self.consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.");

            try self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        try self.statement();
        try self.emit_loop(loop_start);

        if (exit_jump != null) {
            self.patch_jump(exit_jump.?);
            try self.emit_byte(@intFromEnum(OpCode.OP_POP));
        }

        try self.end_scope();
    }

    fn fun_declaration(self: *Parser) ParsingError!void {
        const global: u8 = try self.parse_variable("Expect function name.");
        self.mark_initialized();
        try self.function(FunctionType.Function);
        try self.define_variable(global);
    }

    fn function(self: *Parser, function_type: FunctionType) ParsingError!void {
        var compiler = Compiler.new(self.vm, self.compiler, function_type);

        self.compiler = &compiler;
        if (function_type != FunctionType.Script) {
            self.compiler.function.name = self.vm.copy_string(self.previous.?.start[0..self.previous.?.length]);
        }

        self.begin_scope();

        self.consume(TokenType.LEFT_PAREN, "Expect '(' after function name.");
        if (!self.check(TokenType.RIGHT_PAREN)) {
            while (true) {
                self.compiler.function.arity += 1;
                if (self.compiler.function.arity > 255) {
                    self.error_at_current("Can't have more than 255 parameters.");
                }

                const constant = try self.parse_variable("Expect parameter name.");
                try self.define_variable(constant);
                if (!self.match(TokenType.COMMA)) {
                    break;
                }
            }
        }

        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");
        self.consume(TokenType.LEFT_BRACE, "Expect '{' before function body.");

        try self.block();

        const obj_function = try self.end_parser();

        const constant = try self.make_constant(Value.obj_val(&obj_function.obj));
        try self.emit_bytes(@intFromEnum(OpCode.OP_CLOSURE), constant);

        for (0..obj_function.upvalue_count) |i| {
            if (compiler.upvalues[i].is_local) {
                try self.emit_byte(1);
            } else {
                try self.emit_byte(0);
            }
            try self.emit_byte(@intCast(compiler.upvalues[i].index));
        }
    }

    fn call(self: *Parser, can_assign: bool) ParsingError!void {
        _ = can_assign;

        const arg_count = try self.argument_list();
        try self.emit_bytes(@intFromEnum(OpCode.OP_CALL), @intCast(arg_count));
    }

    fn argument_list(self: *Parser) ParsingError!usize {
        var arg_count: usize = 0;

        if (!self.check(TokenType.RIGHT_PAREN)) {
            while (true) {
                try self.expression();
                if (arg_count == 16) {
                    self.error_msg("Can't have more than 16 arguments.");
                }
                arg_count += 1;

                if (!self.match(TokenType.COMMA)) {
                    break;
                }
            }
        }

        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");

        return arg_count;
    }

    fn return_statement(self: *Parser) ParsingError!void {
        if (self.compiler.function_type == FunctionType.Script) {
            self.error_msg("Can't return from top-level code.");
        }

        if (self.match(TokenType.SEMICOLON)) {
            try self.emit_return();
        } else {
            try self.expression();
            self.consume(TokenType.SEMICOLON, "Expect ';' after return value.");
            try self.emit_byte(@intFromEnum(OpCode.OP_RETURN));
        }
    }

    fn class_declaration(self: *Parser) ParsingError!void {
        self.consume(TokenType.IDENTIFIER, "Expect class name.");
        const class_name = self.previous.?;

        const name_constant = try self.identifier_constant(self.previous.?);
        self.declare_variable();

        try self.emit_bytes(@intFromEnum(OpCode.OP_CLASS), name_constant);
        try self.define_variable(name_constant);

        try self.named_variable(class_name, false);

        self.consume(TokenType.LEFT_BRACE, "Expect '{' before class body.");
        while (!self.check(TokenType.RIGHT_BRACE) and !self.check(TokenType.EOF)) {
            try self.method();
        }
        self.consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.");
        try self.emit_byte(@intFromEnum(OpCode.OP_POP));
    }

    fn dot(self: *Parser, can_assign: bool) ParsingError!void {
        self.consume(TokenType.IDENTIFIER, "Expect property name after '.'.");
        const name = try self.identifier_constant(self.previous.?);

        if (can_assign and self.match(TokenType.EQUAL)) {
            try self.expression();
            try self.emit_bytes(@intFromEnum(OpCode.OP_SET_PROPERTY), name);
        } else {
            try self.emit_bytes(@intFromEnum(OpCode.OP_GET_PROPERTY), name);
        }
    }

    fn method(self: *Parser) ParsingError!void {
        self.consume(TokenType.IDENTIFIER, "Expect method name.");
        const constant = try self.identifier_constant(self.previous.?);

        try self.function(FunctionType.Function);
        try self.emit_bytes(@intFromEnum(OpCode.OP_METHOD), constant);
    }
};

const FunctionType = enum {
    Function,
    Script,
};

pub const Compiler = struct {
    enclosing: ?*Compiler,

    function: *Obj.Function,
    function_type: FunctionType,

    locals: [constants.UINT8_COUNT]Local,
    local_count: usize,
    upvalues: [constants.UINT8_COUNT]Upvalue,
    scope_depth: usize,

    fn new(vm: *VM, enclosing: ?*Compiler, function_type: FunctionType) Compiler {
        const obj_function = Obj.Function.new(vm);

        var compiler = Compiler{
            .locals = undefined,
            .local_count = 0,
            .upvalues = undefined,
            .scope_depth = 0,
            .function = obj_function,
            .function_type = function_type,
            .enclosing = enclosing,
        };

        compiler.locals[0].depth = 0;
        compiler.locals[0].name = Token{
            .token_type = TokenType.EOF,
            .start = "",
            .length = 0,
            .line = 0,
        };
        compiler.locals[0].is_captured = false;

        compiler.local_count += 1;

        return compiler;
    }

    fn destroy(self: *Compiler) void {
        // do not destroy function here! it is used after compiler life.
        _ = self;
    }
};

const Local = struct {
    name: Token,
    depth: ?usize,
    is_captured: bool,
};

const Upvalue = struct {
    index: usize,
    is_local: bool,
};

pub fn compile(vm: *VM, contents: []const u8) !?*Obj.Function {
    var compiler = Compiler.new(vm, null, FunctionType.Script);
    var scanner = Scanner.init(contents);
    var parser = Parser.new(vm, &compiler, &scanner);

    vm.parser = &parser;

    parser.advance();

    while (!parser.match(TokenType.EOF)) {
        try parser.declaration();
    }

    const function = try parser.end_parser();

    vm.parser = null;
    if (!parser.had_error) {
        return function;
    } else {
        return null;
    }
}
