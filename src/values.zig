const std = @import("std");
const debug = std.debug;

const Allocator = std.mem.Allocator;

const utils = @import("./utils.zig");

pub const Value = f64;

pub const ValueArray = struct {
    capacity: usize,
    count: usize,
    values: []Value,

    pub fn new() ValueArray {
        return ValueArray{
            .capacity = 0,
            .count = 0,
            .values = &.{},
        };
    }

    pub fn write(self: *ValueArray, allocator: Allocator, value: Value) !void {
        if (self.capacity < self.count + 1) {
            const old_capacity = self.capacity;
            self.capacity = utils.grow_capacity(old_capacity);
            self.values = try allocator.realloc(self.values, self.capacity);
        }

        self.values[self.count] = value;
        self.count += 1;
    }

    pub fn free(self: *ValueArray, allocator: Allocator) void {
        if (self.capacity > 0) {
            allocator.free(self.values);
        }
    }
};

pub fn print_value(value: Value) void {
    debug.print("{d}", .{value});
}
