const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Value = @import("./values.zig").Value;
const ValueArray = @import("./values.zig").ValueArray;
const OpCode = @import("./opcode.zig").OpCode;
const VM = @import("./vm.zig").VM;
const ZloxAllocator = @import("./memory.zig").ZloxAllocator;

const constants = @import("./constant.zig");
const utils = @import("./utils.zig");

pub const Chunk = struct {
    allocator: Allocator,
    vm: *VM,

    count: usize,
    capacity: usize,
    code: []u8,
    lines: []usize,
    constants: ValueArray,

    pub fn new(allocator: Allocator, vm: *VM) *Chunk {
        var chunk: *Chunk = allocator.create(Chunk) catch unreachable;

        chunk.allocator = allocator;
        chunk.vm = vm;
        chunk.count = 0;
        chunk.capacity = 0;
        chunk.code = &.{};
        chunk.lines = &.{};
        chunk.constants = ValueArray.new(allocator);

        return chunk;
    }

    pub fn destroy(self: *Chunk) void {
        self.constants.destroy();

        if (self.capacity > 0) {
            self.allocator.free(self.code);
            self.allocator.free(self.lines);
        }

        self.allocator.destroy(self);
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        if (self.capacity < self.count + 1) {
            const old_capacity = self.capacity;
            self.capacity = ZloxAllocator.grow_capacity(old_capacity);

            self.code = try self.allocator.realloc(self.code, self.capacity);
            self.lines = try self.allocator.realloc(self.lines, self.capacity);
        }

        self.code[self.count] = byte;
        self.lines[self.count] = line;
        self.count += 1;
    }

    pub fn dump(self: *Chunk) void {
        debug.print("== chunk dump of {*} ==\n", .{self});
        debug.print("{any}\n", .{self});

        for (0..self.constants.count) |idx| {
            debug.print("constant {d}: {*} ", .{ idx, &self.constants.values[idx] });
            self.constants.values[idx].print();
            debug.print("\n", .{});
        }

        debug.print("== end of chunk dump \n\n", .{});
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
        var current_offset = offset;
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
            @intFromEnum(OpCode.OP_CALL) => return utils.byte_instruction("OP_CALL", self, offset),
            @intFromEnum(OpCode.OP_CLOSURE) => {
                current_offset += 1;
                const constant = self.code[current_offset];
                current_offset += 1;
                debug.print("{s:<16} {d:0>4} ", .{ "OP_CLOSURE", constant });
                self.constants.values[constant].print();
                debug.print("\n", .{});

                const function = self.constants.values[constant].as_obj().as_function();
                for (0..function.upvalue_count) |j| {
                    _ = j;
                    const is_local_str = switch (self.code[current_offset]) {
                        1 => "local",
                        else => "upvalue",
                    };
                    current_offset += 1;
                    const index = self.code[current_offset];
                    current_offset += 1;

                    debug.print("{d:0>4}      | {s:<19} {s} {d}\n", .{ current_offset - 2, "", is_local_str, index });
                }
                return current_offset;
            },
            @intFromEnum(OpCode.OP_GET_UPVALUE) => return utils.byte_instruction("OP_GET_UPVALUE", self, offset),
            @intFromEnum(OpCode.OP_SET_UPVALUE) => return utils.byte_instruction("OP_SET_UPVALUE", self, offset),
            @intFromEnum(OpCode.OP_CLOSE_UPVALUE) => return utils.simple_instruction("OP_CLOSE_UPVALUE", offset),
            else => {
                debug.print("unknown opcode {d}\n", .{instruction});
                return offset + 1;
            },
        }
    }

    pub fn add_constant(self: *Chunk, value: Value) !usize {
        _ = try self.vm.push(value);
        try self.constants.write(value);
        _ = self.vm.pop();

        return self.constants.count - 1;
    }
};
