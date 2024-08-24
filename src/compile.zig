const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;

pub fn compile(allocator: Allocator, contents: []const u8) !void {
    var line: ?usize = null;
    _ = allocator;

    var scanner = Scanner.init(contents);

    while (true) {
        const token = scanner.scan_token();
        if (line == null or token.line != line.?) {
            debug.print("{d:4} ", .{token.line});
            line = token.line;
        } else {
            debug.print("   | ", .{});
        }
        debug.print("{s:12} len:{d:2} '{s}'\n", .{ token.token_type.string(), token.length, token.start[0..token.length] });

        if (token.token_type == TokenType.EOF) {
            break;
        }
    }
}
