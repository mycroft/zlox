const std = @import("std");
const debug = std.debug;

const Chunk = @import("./chunk.zig").Chunk;
const Token = @import("./scanner.zig").Token;

const print_value = @import("./values.zig").print_value;

pub fn simple_instruction(opcode_name: []const u8, offset: usize) usize {
    debug.print("{s:<16}\n", .{opcode_name});

    return offset + 1;
}

pub fn constant_instruction(opcode_name: []const u8, chunk: Chunk, offset: usize) usize {
    const constant = chunk.code[offset + 1];
    debug.print("{s:<16} {d:4} '", .{ opcode_name, constant });
    print_value(chunk.constants.values[constant]);
    debug.print("'\n", .{});
    return offset + 2;
}

pub fn byte_instruction(opcode_name: []const u8, chunk: Chunk, offset: usize) usize {
    const slot = chunk.code[offset + 1];
    debug.print("{s:<16} {d:4}\n", .{ opcode_name, slot });

    return offset + 2;
}

pub fn jump_instruction(opcode_name: []const u8, sign: i32, chunk: Chunk, offset: usize) usize {
    var jump: u16 = @as(u16, chunk.code[offset + 1]) << 8;
    jump |= chunk.code[offset + 2];

    const address: i32 = @as(i32, @intCast(offset)) + 3 + sign * jump;

    debug.print("{s:<16} {d:4} -> {d}\n", .{ opcode_name, offset, address });
    return offset + 3;
}

pub fn compute_hash(str: []const u8) u32 {
    var res_hash: u32 = 2166136261;

    for (str) |c| {
        res_hash ^= c;
        res_hash *%= 16777619;
    }

    return res_hash;
}

pub fn identifiers_equals(a: Token, b: Token) bool {
    if (a.length != b.length) {
        return false;
    }

    return std.mem.eql(u8, a.start[0..a.length], b.start[0..b.length]);
}

pub fn invoke_instruction(opcode_name: []const u8, chunk: Chunk, offset: usize) usize {
    const constant = chunk.code[offset + 1];
    const arg_count = chunk.code[offset + 2];

    std.debug.print("{s:<16} ({d} args) {d:4} '", .{ opcode_name, arg_count, constant });
    chunk.constants.values[constant].print();
    std.debug.print("'\n", .{});

    return offset + 3;
}
