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
    allocator: Allocator,

    pub fn new(allocator: Allocator) Chunk {
        return Chunk{
            .count = 0,
            .capacity = 0,
            .code = &.{},
            .lines = &.{},
            .constants = ValueArray.new(),
            .allocator = allocator,
        };
    }

    pub fn init(self: *Chunk) !void {
        self.deinit(self.allocator);

        self.count = 0;
        self.capacity = 0;
        self.code = &.{};
        self.lines = &.{};
        self.constants = ValueArray.new();
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        if (self.capacity < self.count + 1) {
            const old_capacity = self.capacity;
            self.capacity = grow_capacity(old_capacity);
            self.code = try self.allocator.realloc(self.code, self.capacity);
            self.lines = try self.allocator.realloc(self.lines, self.capacity);
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
            offset = self.dissassemble_instruction(offset);
        }
        debug.print("== end of {s} ==\n\n", .{name});
    }

    pub fn dissassemble_instruction(self: Chunk, offset: usize) usize {
        debug.print("{d:0>4} ", .{offset});

        if (offset > 0 and self.lines[offset] == self.lines[offset - 1]) {
            debug.print("   | ", .{});
        } else {
            debug.print("{d:4} ", .{self.lines[offset]});
        }

        const instruction = self.code[offset];

        switch (instruction) {
            @intFromEnum(OpCode.OP_RETURN) => return utils.simple_instruction("OP_RETURN", offset),
            @intFromEnum(OpCode.OP_ADD) => return utils.simple_instruction("OP_ADD", offset),
            @intFromEnum(OpCode.OP_SUBSTRACT) => return utils.simple_instruction("OP_SUBSTRACT", offset),
            @intFromEnum(OpCode.OP_MULTIPLY) => return utils.simple_instruction("OP_MULTIPLY", offset),
            @intFromEnum(OpCode.OP_DIVIDE) => return utils.simple_instruction("OP_DIVIDE", offset),
            @intFromEnum(OpCode.OP_NEGATE) => return utils.simple_instruction("OP_NEGATE", offset),
            @intFromEnum(OpCode.OP_CONSTANT) => return utils.constant_instruction("OP_CONSTANT", self, offset),
            @intFromEnum(OpCode.OP_NIL) => return utils.simple_instruction("OP_NIL", offset),
            @intFromEnum(OpCode.OP_TRUE) => return utils.simple_instruction("OP_TRUE", offset),
            @intFromEnum(OpCode.OP_FALSE) => return utils.simple_instruction("OP_FALSE", offset),
            @intFromEnum(OpCode.OP_NOT) => return utils.simple_instruction("OP_NOT", offset),
            @intFromEnum(OpCode.OP_EQUAL) => return utils.simple_instruction("OP_EQUAL", offset),
            @intFromEnum(OpCode.OP_GREATER) => return utils.simple_instruction("OP_GREATER", offset),
            @intFromEnum(OpCode.OP_LESS) => return utils.simple_instruction("OP_LESS", offset),
            @intFromEnum(OpCode.OP_PRINT) => return utils.simple_instruction("OP_PRINT", offset),
            @intFromEnum(OpCode.OP_POP) => return utils.simple_instruction("OP_POP", offset),
            @intFromEnum(OpCode.OP_DEFINE_GLOBAL) => return utils.constant_instruction("OP_DEFINE_GLOBAL", self, offset),
            @intFromEnum(OpCode.OP_GET_GLOBAL) => return utils.constant_instruction("OP_GET_GLOBAL", self, offset),
            @intFromEnum(OpCode.OP_SET_GLOBAL) => return utils.constant_instruction("OP_SET_GLOBAL", self, offset),
            @intFromEnum(OpCode.OP_GET_LOCAL) => return utils.byte_instruction("OP_GET_LOCAL", self, offset),
            @intFromEnum(OpCode.OP_SET_LOCAL) => return utils.byte_instruction("OP_SET_LOCAL", self, offset),
            @intFromEnum(OpCode.OP_JUMP) => return utils.jump_instruction("OP_JUMP", 1, self, offset),
            @intFromEnum(OpCode.OP_JUMP_IF_FALSE) => return utils.jump_instruction("OP_JUMP_IF_FALSE", 1, self, offset),
            @intFromEnum(OpCode.OP_LOOP) => return utils.jump_instruction("OP_LOOP", -1, self, offset),
            else => {
                debug.print("unknown opcode {d}\n", .{instruction});
                return offset + 1;
            },
        }
    }

    pub fn deinit(self: *Chunk) void {
        self.constants.free(self.allocator);

        if (self.capacity > 0) {
            self.allocator.free(self.code);
            self.allocator.free(self.lines);
        }
    }

    pub fn add_constant(self: *Chunk, value: Value) !usize {
        try self.constants.write(self.allocator, value);
        return self.constants.count - 1;
    }
};
