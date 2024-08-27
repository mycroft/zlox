const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const Value = @import("./values.zig").Value;
const Obj = @import("./object.zig").Obj;
const Table = @import("./table.zig").Table;

const compile = @import("./compile.zig").compile;
const compute_hash = @import("./utils.zig").compute_hash;

const DEBUG_TRACE_EXECUTION = @import("./main.zig").DEBUG_TRACE_EXECUTION;

const print_value = @import("./values.zig").print_value;

const STACK_MAX = 256;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    allocator: Allocator,
    chunk: ?*Chunk,
    ip: ?usize,
    stack: std.ArrayList(Value),
    // Keeping creating objects in references to destroy objects on cleaning.
    // In the book, a linked list between objects is used to handle this.
    references: std.ArrayList(*Obj),
    strings: Table,
    globals: Table,
    tracing: bool,

    pub fn new(allocator: Allocator) VM {
        return VM{
            .allocator = allocator,
            .chunk = null,
            .ip = null,
            .stack = std.ArrayList(Value).init(allocator),
            .references = std.ArrayList(*Obj).init(allocator),
            .strings = Table.new(allocator),
            .globals = Table.new(allocator),
            .tracing = false,
        };
    }

    pub fn free(self: *VM) void {
        self.stack.deinit();

        if (self.has_tracing()) {
            self.strings.dump();
        }

        self.strings.deinit();
        self.globals.deinit();
        self.clean_references();
        self.references.deinit();
    }

    pub fn set_trace(self: *VM, tracing: bool) void {
        self.tracing = tracing;
    }

    pub fn has_tracing(self: *VM) bool {
        return self.tracing;
    }

    pub fn interpret(self: *VM, allocator: Allocator, content: []const u8) !InterpretResult {
        var chunk = Chunk.new(allocator);
        defer chunk.deinit();

        const res = try compile(allocator, self, content, &chunk);
        if (!res) {
            return InterpretResult.COMPILE_ERROR;
        }

        self.chunk = &chunk;
        self.ip = 0;

        return try self.run();
    }

    pub fn run(self: *VM) !InterpretResult {
        while (true) {
            if (self.has_tracing()) {
                if (self.stack.items.len > 0) {
                    debug.print("{s:32}", .{""});
                    for (self.stack.items) |item| {
                        debug.print("[ ", .{});
                        print_value(item);
                        debug.print(" ]", .{});
                    }
                    debug.print("\n", .{});
                }
                _ = self.chunk.?.dissassemble_instruction(self.ip.?);
            }

            const instruction = self.read_byte();

            switch (instruction) {
                @intFromEnum(OpCode.OP_CONSTANT) => {
                    const constant = self.read_constant();
                    try self.push(constant);
                },
                @intFromEnum(OpCode.OP_NIL) => try self.push(Value.nil_val()),
                @intFromEnum(OpCode.OP_FALSE) => try self.push(Value.bool_val(false)),
                @intFromEnum(OpCode.OP_TRUE) => try self.push(Value.bool_val(true)),
                @intFromEnum(OpCode.OP_POP) => _ = self.pop(),
                @intFromEnum(OpCode.OP_ADD),
                @intFromEnum(OpCode.OP_SUBSTRACT),
                @intFromEnum(OpCode.OP_MULTIPLY),
                @intFromEnum(OpCode.OP_DIVIDE),
                @intFromEnum(OpCode.OP_LESS),
                @intFromEnum(OpCode.OP_GREATER),
                => {
                    const res = try self.binary_op(@enumFromInt(instruction));
                    if (res != InterpretResult.OK) {
                        return res;
                    }
                },
                @intFromEnum(OpCode.OP_NOT) => {
                    try self.push(Value.bool_val(self.pop().is_falsey()));
                },
                @intFromEnum(OpCode.OP_NEGATE) => {
                    if (!self.peek(0).is_number()) {
                        self.runtime_error("Operand must be a number.");
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    try self.push(Value.number_val(-self.pop().as_number()));
                },
                @intFromEnum(OpCode.OP_PRINT) => {
                    print_value(self.pop());
                    debug.print("\n", .{});
                },
                @intFromEnum(OpCode.OP_RETURN) => {
                    return InterpretResult.OK;
                },
                @intFromEnum(OpCode.OP_EQUAL) => {
                    try self.push(Value.bool_val(self.pop().equals(self.pop())));
                },
                @intFromEnum(OpCode.OP_DEFINE_GLOBAL) => {
                    const name = self.read_constant().as_string();

                    _ = self.globals.set(name, self.peek(0));
                    _ = self.pop();
                },
                @intFromEnum(OpCode.OP_GET_GLOBAL) => {
                    const name: *Obj.String = self.read_constant().as_string();
                    var value = Value.nil_val();

                    if (!self.globals.get(name, &value)) {
                        const err_msg = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'.", .{name.chars});
                        defer self.allocator.free(err_msg);
                        self.runtime_error(err_msg);
                        return InterpretResult.RUNTIME_ERROR;
                    }

                    try self.push(value);
                },
                @intFromEnum(OpCode.OP_SET_GLOBAL) => {
                    const name: *Obj.String = self.read_constant().as_string();

                    if (self.globals.set(name, self.peek(0))) {
                        _ = self.globals.del(name);

                        const err_msg = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'.", .{name.chars});
                        defer self.allocator.free(err_msg);

                        self.runtime_error(err_msg);
                        return InterpretResult.RUNTIME_ERROR;
                    }
                },
                @intFromEnum(OpCode.OP_GET_LOCAL) => {
                    const slot = self.read_byte();
                    try self.push(self.stack.items[slot]);
                },
                @intFromEnum(OpCode.OP_SET_LOCAL) => {
                    const slot = self.read_byte();
                    self.stack.items[slot] = self.peek(0);
                },
                @intFromEnum(OpCode.OP_JUMP) => {
                    const offset = self.read_short();
                    self.ip.? += offset;
                },
                @intFromEnum(OpCode.OP_JUMP_IF_FALSE) => {
                    const offset = self.read_short();
                    if (self.peek(0).is_falsey()) {
                        self.ip.? += offset;
                    }
                },
                @intFromEnum(OpCode.OP_LOOP) => {
                    const offset = self.read_short();
                    self.ip.? -= offset;
                },
                else => {
                    debug.print("Invalid instruction: {d}\n", .{instruction});
                    return InterpretResult.RUNTIME_ERROR;
                },
            }
        }

        return InterpretResult.OK;
    }

    // XXX In the book, we're using a ptr to data directly, to avoid dereferencing to a given offset
    // How to do that in Zig?
    pub fn read_byte(self: *VM) u8 {
        const ret = self.chunk.?.code[self.ip.?];
        self.ip.? += 1;

        return ret;
    }

    pub fn read_short(self: *VM) u16 {
        self.ip.? += 2;

        return (@as(u16, self.chunk.?.code[self.ip.? - 2]) << 8) | (@as(u16, self.chunk.?.code[self.ip.? - 1]));
    }

    pub fn read_constant(self: *VM) Value {
        return self.chunk.?.constants.values[read_byte(self)];
    }

    pub fn push(self: *VM, value: Value) !void {
        try self.stack.append(value);
    }

    pub fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    pub fn binary_op(self: *VM, op: OpCode) !InterpretResult {
        if (op == OpCode.OP_ADD and self.peek(0).is_string() and self.peek(1).is_string()) {
            try self.concatenate();
            return InterpretResult.OK;
        }

        if (!self.peek(0).is_number() or !self.peek(0).is_number()) {
            self.runtime_error("Operands must be two numbers or two strings");
            return InterpretResult.RUNTIME_ERROR;
        }

        const b = self.pop().as_number();
        const a = self.pop().as_number();

        const res: Value = switch (op) {
            OpCode.OP_ADD => Value.number_val(a + b),
            OpCode.OP_SUBSTRACT => Value.number_val(a - b),
            OpCode.OP_MULTIPLY => Value.number_val(a * b),
            OpCode.OP_DIVIDE => Value.number_val(a / b),
            OpCode.OP_LESS => Value.bool_val(a < b),
            OpCode.OP_GREATER => Value.bool_val(a > b),
            else => unreachable,
        };

        try self.push(res);

        return InterpretResult.OK;
    }

    pub fn concatenate(self: *VM) !void {
        const b = self.pop().as_cstring();
        const a = self.pop().as_cstring();

        const concat_str = try std.mem.concat(self.chunk.?.allocator, u8, &.{ a, b });

        var string_obj = self.take_string(concat_str);

        self.add_reference(&string_obj.obj);

        try self.push(Value.obj_val(&string_obj.obj));
    }

    pub fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    pub fn runtime_error(self: *VM, err_msg: []const u8) void {
        const instruction = self.ip.?;
        const line = self.chunk.?.lines[instruction];

        debug.print("err: {s}\n", .{err_msg});
        debug.print("[line {d}] in script\n", .{line});
    }

    pub fn add_reference(self: *VM, obj: *Obj) void {
        // do not add duplicate references
        for (self.references.items) |item| {
            if (item == obj) {
                return;
            }
        }
        // XXX TODO catch unreachable to prevents
        self.references.append(obj) catch unreachable;
    }

    pub fn clean_references(self: *VM) void {
        for (self.references.items) |item| {
            item.destroy();
        }
    }

    pub fn copy_string(self: *VM, source: []const u8) *Obj.String {
        const hash = compute_hash(source);
        const obj_string = self.strings.find_string(source, hash);

        if (obj_string != null) {
            return obj_string.?;
        }

        const copy: []const u8 = self.allocator.dupe(u8, source) catch unreachable;
        return self.allocate_string(copy);
    }

    pub fn take_string(self: *VM, source: []const u8) *Obj.String {
        const hash = compute_hash(source);
        const obj_string = self.strings.find_string(source, hash);

        if (obj_string != null) {
            // free given string
            self.allocator.free(source);
            return obj_string.?;
        }

        return self.allocate_string(source);
    }

    pub fn allocate_string(self: *VM, source: []const u8) *Obj.String {
        const obj_string = Obj.String.new(self.allocator, source);
        _ = self.strings.set(obj_string, Value.nil_val());

        return obj_string;
    }
};
