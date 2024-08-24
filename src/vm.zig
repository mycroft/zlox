const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const Value = @import("./values.zig").Value;

const DEBUG_TRACE_EXECUTION = @import("./main.zig").DEBUG_TRACE_EXECUTION;

const print_value = @import("./values.zig").print_value;

const STACK_MAX = 256;

const InterpretResult = enum {
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

    pub fn interpret(self: *VM, chunk: *Chunk) !InterpretResult {
        self.chunk = chunk;
        self.ip = 0;

        return self.run();
    }

    pub fn run(self: *VM) !InterpretResult {
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                if (self.stack.items.len > 0) {
                    debug.print("S:        ", .{});
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
                @intFromEnum(OpCode.OP_RETURN) => {
                    print_value(self.pop());
                    return InterpretResult.OK;
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
};
