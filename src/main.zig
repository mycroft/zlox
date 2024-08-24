const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const VM = @import("./vm.zig").VM;

pub const DEBUG_TRACE_EXECUTION = true;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var vm = VM.new(allocator);

    var chunk = Chunk.new();
    try chunk.init(allocator);

    const constant = try chunk.add_constant(allocator, 1.2);
    try chunk.write(allocator, @intFromEnum(OpCode.OP_CONSTANT), 123);
    try chunk.write(allocator, @intCast(constant), 123);
    try chunk.write(allocator, @intFromEnum(OpCode.OP_RETURN), 123);

    chunk.dissassemble("test chunk");

    _ = try vm.interpret(&chunk);
    vm.free();

    chunk.deinit(allocator);
}
