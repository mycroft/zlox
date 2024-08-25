const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const Value = @import("./values.zig").Value;

const compile = @import("./compile.zig").compile;

const DEBUG_TRACE_EXECUTION = @import("./main.zig").DEBUG_TRACE_EXECUTION;

const print_value = @import("./values.zig").print_value;

const STACK_MAX = 256;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    chunk: ?*Chunk,
    ip: ?usize,
    stack: std.ArrayList(Value),

    pub fn new(allocator: Allocator) VM {
        return VM{
            .chunk = null,
            .ip = null,
            .stack = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn free(self: *VM) void {
        self.stack.deinit();
    }

    pub fn interpret(self: *VM, allocator: Allocator, content: []const u8) !InterpretResult {
        var chunk = Chunk.new(allocator);
        defer chunk.deinit();

        const res = try compile(allocator, content, &chunk);
        if (!res) {
            return InterpretResult.COMPILE_ERROR;
        }

        self.chunk = &chunk;
        self.ip = 0;

        return try self.run();
    }

    pub fn run(self: *VM) !InterpretResult {
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
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
                @intFromEnum(OpCode.OP_ADD), @intFromEnum(OpCode.OP_SUBSTRACT), @intFromEnum(OpCode.OP_MULTIPLY), @intFromEnum(OpCode.OP_DIVIDE), @intFromEnum(OpCode.OP_LESS), @intFromEnum(OpCode.OP_GREATER) => {
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
                @intFromEnum(OpCode.OP_RETURN) => {
                    print_value(self.pop());
                    debug.print("\n", .{});
                    return InterpretResult.OK;
                },
                @intFromEnum(OpCode.OP_EQUAL) => {
                    try self.push(Value.bool_val(self.pop().equals(self.pop())));
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
        if (!self.peek(0).is_number() or !self.peek(0).is_number()) {
            self.runtime_error("Operands must be numbers");
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

    pub fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    pub fn runtime_error(self: *VM, err_msg: []const u8) void {
        const instruction = self.ip.?;
        const line = self.chunk.?.lines[instruction];

        debug.print("err: {s}\n", .{err_msg});
        debug.print("[line {d}] in script\n", .{line});
    }
};
