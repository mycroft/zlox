const std = @import("std");
const debug = std.debug;

const Allocator = std.mem.Allocator;

const Obj = @import("./object.zig").Obj;
const utils = @import("./utils.zig");

pub const ValueType = enum {
    Bool,
    Nil,
    Number,
    Obj,
};

pub const Value = struct {
    value_type: ValueType,
    as: union {
        boolean: bool,
        number: f64,
        obj: *Obj,
    },

    pub fn bool_val(value: bool) Value {
        return Value{
            .value_type = ValueType.Bool,
            .as = .{
                .boolean = value,
            },
        };
    }

    pub fn nil_val() Value {
        return Value{
            .value_type = ValueType.Nil,
            .as = .{
                .boolean = false,
            },
        };
    }

    pub fn number_val(value: f64) Value {
        return Value{
            .value_type = ValueType.Number,
            .as = .{
                .number = value,
            },
        };
    }

    pub fn obj_val(obj: *Obj) Value {
        return Value{
            .value_type = ValueType.Obj,
            .as = .{
                .obj = obj,
            },
        };
    }

    pub fn as_bool(self: Value) bool {
        return self.as.boolean;
    }

    pub fn as_number(self: Value) f64 {
        return self.as.number;
    }

    pub fn as_obj(self: Value) *Obj {
        return self.as.obj;
    }

    pub fn as_string(self: Value) *Obj.String {
        const obj: *Obj.String = self.as_obj();

        return obj;
    }

    pub fn as_cstring(self: Value) []const u8 {
        const obj: *Obj.String = self.as_obj().as_string();

        return obj.chars;
    }

    pub fn is_bool(self: Value) bool {
        return self.value_type == ValueType.Bool;
    }

    pub fn is_number(self: Value) bool {
        return self.value_type == ValueType.Number;
    }

    pub fn is_nil(self: Value) bool {
        return self.value_type == ValueType.Nil;
    }

    pub fn is_obj(self: Value) bool {
        return self.value_type == ValueType.Obj;
    }

    pub fn is_string(self: Value) bool {
        return self.is_obj() and self.as_obj().is_string();
    }

    pub fn is_falsey(self: Value) bool {
        return self.is_nil() or (self.is_bool() and !self.as_bool());
    }

    pub fn equals(self: Value, other: Value) bool {
        if (self.value_type != other.value_type) {
            return false;
        }

        return switch (self.value_type) {
            ValueType.Nil => true,
            ValueType.Bool => self.as_bool() == other.as_bool(),
            ValueType.Number => self.as_number() == other.as_number(),
            ValueType.Obj => self.as_obj() == other.as_obj(),
        };
    }

    pub fn print(self: Value) void {
        switch (self.value_type) {
            ValueType.Nil => debug.print("nil", .{}),
            ValueType.Bool => debug.print("{any}", .{self.as_bool()}),
            ValueType.Number => debug.print("{d}", .{self.as_number()}),
            ValueType.Obj => self.as_obj().print(),
        }
    }
};

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
    value.print();
}
