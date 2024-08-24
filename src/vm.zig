const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const Value = @import("./values.zig").Value;

const DEBUG_TRACE_EXECUTION = @import("./main.zig").DEBUG_TRACE_EXECUTION;

const print_value = @import("./values.zig").print_value;

const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    chunk: ?*Chunk,
    ip: ?usize,

    pub fn new() VM {
        return VM{
            .chunk = null,
            .ip = null,
        };
    }

    pub fn free(self: *VM) void {
        _ = self;
    }

    pub fn interpret(self: *VM, chunk: *Chunk) InterpretResult {
        self.chunk = chunk;
        self.ip = 0;

        return self.run();
    }

    pub fn run(self: *VM) InterpretResult {
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                _ = self.chunk.?.dissassemble_instruction(self.ip.?);
            }

            const instruction = self.read_byte();

            switch (instruction) {
                @intFromEnum(OpCode.OP_CONSTANT) => {
                    const constant = self.read_constant();

                    // XXX Those should not be std.debug, but stdout.
                    print_value(constant);
                    std.debug.print("\n", .{});
                },
                @intFromEnum(OpCode.OP_RETURN) => return InterpretResult.OK,
                else => {
                    std.debug.print("Invalid instruction: {d}\n", .{instruction});
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
};
