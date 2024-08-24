const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const Value = @import("./values.zig").Value;

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
            const instruction = self.read_byte();

            switch (instruction) {
                @intFromEnum(OpCode.OP_CONSTANT) => {
                    const constant = self.read_constant();
                    print_value(constant);
                },
                @intFromEnum(OpCode.OP_RETURN) => return InterpretResult.OK,
                else => return InterpretResult.RUNTIME_ERROR,
            }
        }

        return InterpretResult.OK;
    }

    // XXX In the book, we're using a ptr to data directly, to avoid dereferencing to a given offset
    // How to do that in Zig?
    pub fn read_byte(self: *VM) u8 {
        self.ip.? += 1;

        return self.chunk.?.code[self.ip.?];
    }

    pub fn read_constant(self: *VM) Value {
        return self.chunk.?.constants.values[read_byte(self)];
    }
};
