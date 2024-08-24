const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const VM = @import("./vm.zig").VM;
const InterpretResult = @import("./vm.zig").InterpretResult;

const compile = @import("./compile.zig").compile;

pub const DEBUG_TRACE_EXECUTION = true;

pub fn repl(allocator: Allocator) !void {
    var line: [1024]u8 = undefined;

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        try stdout.print("> ", .{});

        @memset(&line, 0);

        const bytes_read = try stdin.read(&line);

        if (bytes_read == 0) {
            try stdout.print("\n", .{});
            break;
        }

        _ = try interpret(allocator, &line);
    }
}

pub fn run_file(allocator: Allocator, filepath: []const u8) !void {
    const file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();

    const file_content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(file_content);

    const result = try interpret(allocator, file_content);

    switch (result) {
        InterpretResult.COMPILE_ERROR => std.process.exit(65),
        InterpretResult.RUNTIME_ERROR => std.process.exit(70),
        else => {},
    }
}

pub fn interpret(allocator: Allocator, content: []const u8) !InterpretResult {
    // XXX catch and return InterpretResult.COMPILE_ERROR ?
    try compile(allocator, content);
    return InterpretResult.OK;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        try repl(allocator);
    } else if (args.len == 2) {
        try run_file(allocator, args[1]);
    } else {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("Usage: clox [path]\n", .{});
        std.process.exit(64);
    }

    // var vm = VM.new(allocator);

    // var chunk = Chunk.new();
    // try chunk.init(allocator);

    // var constant = try chunk.add_constant(allocator, 1.2);
    // try chunk.write(allocator, @intFromEnum(OpCode.OP_CONSTANT), 123);
    // try chunk.write(allocator, @intCast(constant), 123);

    // constant = try chunk.add_constant(allocator, 3.4);
    // try chunk.write(allocator, @intFromEnum(OpCode.OP_CONSTANT), 123);
    // try chunk.write(allocator, @intCast(constant), 123);

    // try chunk.write(allocator, @intFromEnum(OpCode.OP_ADD), 123);

    // constant = try chunk.add_constant(allocator, 5.6);
    // try chunk.write(allocator, @intFromEnum(OpCode.OP_CONSTANT), 123);
    // try chunk.write(allocator, @intCast(constant), 123);

    // try chunk.write(allocator, @intFromEnum(OpCode.OP_DIVIDE), 123);

    // try chunk.write(allocator, @intFromEnum(OpCode.OP_NEGATE), 123);
    // try chunk.write(allocator, @intFromEnum(OpCode.OP_RETURN), 123);

    // chunk.dissassemble("test chunk");

    // _ = try vm.interpret(&chunk);
    // vm.free();

    // chunk.deinit(allocator);
}
