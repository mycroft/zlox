const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const OpCode = enum(u8) { OP_RETURN };

const Chunk = struct {
    count: usize,
    capacity: usize,
    code: []u8,

    pub fn new() Chunk {
        return Chunk{
            .count = 0,
            .capacity = 0,
            .code = &.{},
        };
    }

    pub fn init(self: *Chunk, allocator: Allocator) !void {
        self.deinit(allocator);

        self.count = 0;
        self.capacity = 0;
        self.code = &.{};
    }

    pub fn write(self: *Chunk, allocator: Allocator, byte: u8) !void {
        if (self.capacity < self.count + 1) {
            const old_capacity = self.capacity;
            self.capacity = grow_capacity(old_capacity);
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

        for (0..self.count) |idx| {
            const offset = self.dissassemble_instruction(idx);
            _ = offset;
        }
    }

    pub fn dissassemble_instruction(self: Chunk, offset: usize) usize {
        debug.print("{x:0>4} ", .{offset});

        const instruction = self.code[offset];

        switch (instruction) {
            @intFromEnum(OpCode.OP_RETURN) => return simple_instruction("OP_RETURN", offset),
            else => {
                debug.print("unknown opcode {d}\n", .{instruction});
                return offset + 1;
            },
        }
    }

    pub fn deinit(self: *Chunk, allocator: Allocator) void {
        if (self.capacity > 0) {
            allocator.free(self.code);
        }
    }
};

pub fn simple_instruction(opcode_name: []const u8, offset: usize) usize {
    debug.print("{s}\n", .{opcode_name});

    return offset + 1;
}

pub fn grow_capacity(capacity: usize) usize {
    if (capacity < 8) {
        return 8;
    }
    return capacity * 2;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var chunk = Chunk.new();
    try chunk.init(allocator);

    try chunk.write(allocator, @intFromEnum(OpCode.OP_RETURN));
    chunk.dissassemble("test chunk");

    chunk.deinit(allocator);
}
