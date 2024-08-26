const std = @import("std");
const debug = std.debug;

const Chunk = @import("./chunk.zig").Chunk;
const print_value = @import("./values.zig").print_value;

pub fn grow_capacity(capacity: usize) usize {
    if (capacity < 8) {
        return 8;
    }
    return capacity * 2;
}

pub fn simple_instruction(opcode_name: []const u8, offset: usize) usize {
    debug.print("{s:16}\n", .{opcode_name});

    return offset + 1;
}

pub fn constant_instruction(opcode_name: []const u8, chunk: Chunk, offset: usize) usize {
    const constant = chunk.code[offset + 1];
    debug.print("{s:16} {d:4} '", .{ opcode_name, constant });
    print_value(chunk.constants.values[constant]);
    debug.print("'\n", .{});
    return offset + 2;
}

pub fn compute_hash(str: []const u8) u32 {
    var res_hash: u32 = 2166136261;

    for (str) |c| {
        res_hash ^= c;
        res_hash *%= 16777619;
    }

    return res_hash;
}
