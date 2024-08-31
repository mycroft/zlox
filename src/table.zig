const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const constants = @import("./constant.zig");

const Obj = @import("./object.zig").Obj;
const Value = @import("./values.zig").Value;
const ZloxAllocator = @import("./memory.zig").ZloxAllocator;

const compute_hash = @import("./utils.zig").compute_hash;

pub const Entry = struct {
    key: ?*Obj.String = null,
    value: Value = Value.nil_val(),
};

pub const Table = struct {
    allocator: Allocator,
    count: usize,
    capacity: usize,
    entries: []Entry,

    pub fn new(allocator: Allocator) Table {
        return Table{
            .allocator = allocator,
            .count = 0,
            .capacity = 0,
            .entries = &.{},
        };
    }

    pub fn destroy(self: *Table) void {
        if (self.capacity == 0) {
            return;
        }

        self.allocator.free(self.entries);
    }

    pub fn set(self: *Table, key: *Obj.String, value: Value) bool {
        const current_count: f32 = @floatFromInt(self.count + 1);
        const current_capacity: f32 = @floatFromInt(self.capacity);

        if (current_count > current_capacity * constants.TABLE_MAX_LOAD) {
            const capacity = ZloxAllocator.grow_capacity(self.capacity);
            self.adjust_capacity(capacity);
        }

        const entry = Table.find_entry(self.entries, key);
        const is_new = entry.?.key == null;
        if (is_new and entry.?.value.is_nil()) {
            self.count += 1;
        }

        entry.?.key = key;
        entry.?.value = value;

        return is_new;
    }

    pub fn find_entry(entries: []Entry, key: *Obj.String) ?*Entry {
        var tombstone: ?*Entry = null;
        var index = key.hash % entries.len;

        while (true) {
            const entry = &entries[index];
            if (entry.key == null) {
                if (entry.value.is_nil()) {
                    // Empty entry.
                    if (tombstone != null) {
                        return tombstone;
                    } else {
                        return entry;
                    }
                } else {
                    // We found a tombestone
                    if (tombstone == null) {
                        tombstone = entry;
                    }
                }
            } else if (entry.key == key) {
                // We found the key
                return entry;
            }

            index = (index + 1) % entries.len;
        }
    }

    pub fn adjust_capacity(self: *Table, capacity: usize) void {
        const entries = self.allocator.alloc(Entry, capacity) catch unreachable;

        for (entries) |*e| {
            e.* = Entry{};
        }

        self.count = 0;
        for (0..self.capacity) |idx| {
            const entry = self.entries[idx];
            if (entry.key == null) {
                continue;
            }

            const dest_entry = Table.find_entry(entries, entry.key.?);
            dest_entry.?.key = entry.key;
            dest_entry.?.value = entry.value;

            self.count += 1;
        }

        self.capacity = capacity;
        if (entries.len > 0) {
            self.allocator.free(self.entries);
        }
        self.entries = entries;
    }

    pub fn dump(self: Table) void {
        std.debug.print("== Hash table count:{} capacity:{} ==\n", .{ self.count, self.capacity });
        for (self.entries, 0..) |entry, idx| {
            if (entry.key != null) {
                std.debug.print("{d} {*} (size: {d} hash:{d}) - {s}: ", .{ idx, entry.key, entry.key.?.chars.len, entry.key.?.hash, entry.key.?.chars });
                entry.value.type_print();
                std.debug.print("\n", .{});
            }

            if (entry.key == null and entry.value.as_bool()) {
                std.debug.print("{d} - tombstone\n", .{idx});
            }
        }
        std.debug.print("== End of hash table ==\n\n", .{});
    }

    pub fn add_all(self: *Table, from: Table) void {
        for (from.entries) |entry| {
            if (entry.key == null) {
                continue;
            }
            _ = self.set(entry.key.?, entry.value);
        }
    }

    pub fn get(self: Table, key: *Obj.String, value: *Value) bool {
        if (self.count == 0) {
            return false;
        }

        const entry = Table.find_entry(self.entries, key);
        if (entry.?.key == null) {
            return false;
        }

        value.* = entry.?.value;

        return true;
    }

    pub fn del(self: *Table, key: *Obj.String) bool {
        if (self.count == 0) {
            return false;
        }

        // Find the entry
        const entry = Table.find_entry(self.entries, key);
        if (entry.?.key == null) {
            return false;
        }

        // Place a tombstone in the entry
        entry.?.key = null;
        entry.?.value = Value.bool_val(true);

        return true;
    }

    pub fn find_string(self: *Table, chars: []const u8, hash: u32) ?*Obj.String {
        if (self.count == 0) {
            return null;
        }

        var index = hash % self.capacity;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key == null) {
                // Stop if we find an empty non-tombstone entry.
                if (entry.value.is_nil()) {
                    return null;
                }
            } else if (entry.key.?.chars.len == chars.len and entry.key.?.hash == hash and std.mem.eql(u8, chars, entry.key.?.chars)) {
                return entry.key;
            }

            index = (index + 1) % self.capacity;
        }
    }
};

test "initialize an hash table" {
    const allocator = std.testing.allocator;

    var table = Table.new(allocator);
    defer table.destroy();
    try std.testing.expectEqual(0, table.count);
    try std.testing.expectEqual(0, table.capacity);
}

test "adding values" {
    const allocator = std.testing.allocator;

    const key = Obj.String.new(allocator, "hello world");
    defer key.destroy();

    var table = Table.new(allocator);
    defer table.destroy();

    var res = table.set(key, Value.nil_val());
    try std.testing.expectEqual(true, res);
    try std.testing.expectEqual(1, table.count);
    try std.testing.expectEqual(8, table.capacity);

    res = table.set(key, Value.nil_val());
    try std.testing.expectEqual(false, res);
    try std.testing.expectEqual(1, table.count);
    try std.testing.expectEqual(8, table.capacity);
}

test "adding tables" {
    const allocator = std.testing.allocator;

    const key = Obj.String.new(allocator, "hello world");
    defer key.destroy();

    var table = Table.new(allocator);
    defer table.destroy();

    const res = table.set(key, Value.nil_val());
    try std.testing.expectEqual(true, res);
    try std.testing.expectEqual(8, table.capacity);
    try std.testing.expectEqual(1, table.count);

    var table2 = Table.new(allocator);
    defer table2.destroy();

    try std.testing.expectEqual(0, table2.capacity);
    try std.testing.expectEqual(0, table2.count);

    table2.add_all(table);
    try std.testing.expectEqual(8, table2.capacity);
    try std.testing.expectEqual(1, table2.count);
}

test "deleting from table" {
    const allocator = std.testing.allocator;

    const key = Obj.String.new(allocator, "hello world");
    defer key.destroy();

    var table = Table.new(allocator);
    defer table.destroy();

    var res = table.set(key, Value.nil_val());
    try std.testing.expectEqual(true, res);

    // table.dump();

    res = table.del(key);
    try std.testing.expectEqual(true, res);

    // table.dump();
}

test "find" {
    const allocator = std.testing.allocator;

    const key = Obj.String.new(allocator, "hello world");
    defer key.destroy();

    var table = Table.new(allocator);
    defer table.destroy();

    const value = Value.number_val(42.0);

    var res = table.set(key, value);
    try std.testing.expectEqual(true, res);

    var entry = table.find_string("bye world", compute_hash("bye world"));
    try std.testing.expectEqual(entry, null);

    entry = table.find_string("hello world", compute_hash("hello world"));
    // std.debug.print("{any}\n", .{entry});
    try std.testing.expect(entry != null);

    var value_obj = Value.nil_val();
    res = table.get(entry.?, &value_obj);
    try std.testing.expect(res);
    // std.debug.print("{any}\n", .{value_obj});

    try std.testing.expectEqual(value_obj.as_number(), value.as_number());
}
