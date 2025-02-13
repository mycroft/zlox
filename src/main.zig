const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const constants = @import("./constant.zig");

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const VM = @import("./vm.zig").VM;
const InterpretResult = @import("./vm.zig").InterpretResult;

const ZloxAllocator = @import("./memory.zig").ZloxAllocator;

// XXX imported to run tests.
const Table = @import("./table.zig");

pub fn repl(vm: *VM) !void {
    var line: [1024]u8 = undefined;

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        if (constants.DEBUG_PRINT_GLOBALS) {
            vm.globals.dump();
        }

        try stdout.print("zlox> ", .{});

        @memset(&line, 0);

        const bytes_read = try stdin.read(&line);

        if (bytes_read == 0) {
            try stdout.print("\n", .{});
            break;
        }

        _ = try vm.interpret(&line);
    }
}

pub fn run_file(allocator: Allocator, vm: *VM, filepath: []const u8) !void {
    const file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();

    const file_content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(file_content);

    const result = try vm.interpret(file_content);

    switch (result) {
        InterpretResult.COMPILE_ERROR => std.process.exit(65),
        InterpretResult.RUNTIME_ERROR => std.process.exit(70),
        else => {},
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var vm = VM.new();
    if (constants.USE_CUSTON_ALLOCATOR) {
        var zlox_allocator_generator = ZloxAllocator.init(allocator, &vm);
        const z_allocator = zlox_allocator_generator.allocator();
        vm.init_vm(z_allocator);
    } else {
        vm.init_vm(allocator);
    }
    defer vm.destroy();

    if (args.len == 1) {
        try repl(&vm);
    } else if (args.len == 2) {
        try run_file(allocator, &vm, args[1]);
    } else {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("Usage: clox [path]\n", .{});
        std.process.exit(64);
    }
}
