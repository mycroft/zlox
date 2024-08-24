const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const utils = @import("./utils.zig");
const values = @import("./values.zig");
const Value = values.Value;
const ValueArray = values.ValueArray;

const OpCode = enum(u8) { OP_CONSTANT, OP_RETURN };

const Chunk = struct {
    count: usize,
    capacity: usize,
    code: []u8,
    constants: ValueArray,

    pub fn new() Chunk {
        return Chunk{
            .count = 0,
            .capacity = 0,
            .code = &.{},
            .constants = ValueArray.new(),
        };
    }

    pub fn init(self: *Chunk, allocator: Allocator) !void {
        self.deinit(allocator);

        self.count = 0;
        self.capacity = 0;
        self.code = &.{};
        self.constants = ValueArray.new();
    }

    pub fn write(self: *Chunk, allocator: Allocator, byte: u8) !void {
        if (self.capacity < self.count + 1) {
            const old_capacity = self.capacity;
            self.capacity = utils.grow_capacity(old_capacity);
            self.code = try allocator.realloc(self.code, self.capacity);
        }

        self.code[self.count] = byte;
        self.count += 1;
    }

    pub fn dump(self: Chunk) void {
        debug.print("{any}\n", .{self});
    }

    pub fn dissassemble(self: Chunk, name: []const u8) void {
        debug.print("== {s} ==\n", .{name});

        var offset: usize = 0;

        while (offset < self.count) {
            offset += self.dissassemble_instruction(offset);
        }
    }

    pub fn dissassemble_instruction(self: Chunk, offset: usize) usize {
        debug.print("{x:0>4} ", .{offset});

        const instruction = self.code[offset];

        switch (instruction) {
            @intFromEnum(OpCode.OP_RETURN) => return simple_instruction("OP_RETURN", offset),
            @intFromEnum(OpCode.OP_CONSTANT) => return constant_instruction("OP_CONSTANT", self, offset),
            else => {
                debug.print("unknown opcode {d}\n", .{instruction});
                return offset + 1;
            },
        }
    }

    pub fn deinit(self: *Chunk, allocator: Allocator) void {
        self.constants.free(allocator);

        if (self.capacity > 0) {
            allocator.free(self.code);
        }
    }

    pub fn add_constant(self: *Chunk, allocator: Allocator, value: Value) !usize {
        try self.constants.write(allocator, value);
        return self.constants.count - 1;
    }
};

pub fn simple_instruction(opcode_name: []const u8, offset: usize) usize {
    debug.print("{s:16}\n", .{opcode_name});

    return offset + 1;
}

pub fn constant_instruction(opcode_name: []const u8, chunk: Chunk, offset: usize) usize {
    const constant = chunk.code[offset + 1];
    debug.print("{s:16} {d:4} ", .{ opcode_name, constant });
    values.print_value(chunk.constants.values[constant]);
    debug.print("\n", .{});
    return offset + 2;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var chunk = Chunk.new();
    try chunk.init(allocator);

    try chunk.write(allocator, @intFromEnum(OpCode.OP_RETURN));
    const constant = try chunk.add_constant(allocator, 1.2);
    try chunk.write(allocator, @intFromEnum(OpCode.OP_CONSTANT));
    try chunk.write(allocator, @intCast(constant));

    chunk.dissassemble("test chunk");

    chunk.deinit(allocator);
}
