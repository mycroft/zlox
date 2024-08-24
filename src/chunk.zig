const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Value = @import("./values.zig").Value;
const ValueArray = @import("./values.zig").ValueArray;
const OpCode = @import("./opcode.zig").OpCode;

const grow_capacity = @import("./utils.zig").grow_capacity;
const utils = @import("./utils.zig");

pub const Chunk = struct {
    count: usize,
    capacity: usize,
    code: []u8,
    lines: []usize,
    constants: ValueArray,

    pub fn new() Chunk {
        return Chunk{
            .count = 0,
            .capacity = 0,
            .code = &.{},
            .lines = &.{},
            .constants = ValueArray.new(),
        };
    }

    pub fn init(self: *Chunk, allocator: Allocator) !void {
        self.deinit(allocator);

        self.count = 0;
        self.capacity = 0;
        self.code = &.{};
        self.lines = &.{};
        self.constants = ValueArray.new();
    }

    pub fn write(self: *Chunk, allocator: Allocator, byte: u8, line: usize) !void {
        if (self.capacity < self.count + 1) {
            const old_capacity = self.capacity;
            self.capacity = grow_capacity(old_capacity);
            self.code = try allocator.realloc(self.code, self.capacity);
            self.lines = try allocator.realloc(self.lines, self.capacity);
        }

        self.code[self.count] = byte;
        self.lines[self.count] = line;
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

        if (offset > 0 and self.lines[offset] == self.lines[offset - 1]) {
            debug.print("   | ", .{});
        } else {
            debug.print("{d:4} ", .{self.lines[offset]});
        }

        const instruction = self.code[offset];

        switch (instruction) {
            @intFromEnum(OpCode.OP_RETURN) => return utils.simple_instruction("OP_RETURN", offset),
            @intFromEnum(OpCode.OP_CONSTANT) => return utils.constant_instruction("OP_CONSTANT", self, offset),
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
            allocator.free(self.lines);
        }
    }

    pub fn add_constant(self: *Chunk, allocator: Allocator, value: Value) !usize {
        try self.constants.write(allocator, value);
        return self.constants.count - 1;
    }
};
