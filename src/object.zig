const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

pub const ObjType = enum {
    String,
};

pub const Obj = struct {
    kind: ObjType,
    allocator: std.mem.Allocator,

    pub const String = struct {
        chars: []const u8,
        obj: Obj,

        pub fn new(allocator: std.mem.Allocator, str: []const u8) *String {
            const obj = Obj{
                .kind = ObjType.String,
                .allocator = allocator,
            };

            const str_obj = allocator.create(String) catch unreachable;
            str_obj.obj = obj;
            str_obj.chars = allocator.dupe(u8, str) catch unreachable;

            return str_obj;
        }

        pub fn destroy(self: *String) void {
            const allocator = self.obj.allocator;
            allocator.free(self.chars);
            allocator.destroy(self);
        }
    };

    pub fn is_type(self: *Obj, kind: ObjType) bool {
        return self.kind == kind;
    }

    pub fn is_string(self: *Obj) bool {
        return self.is_type(ObjType.String);
    }

    pub fn print(self: *Obj) void {
        switch (self.kind) {
            ObjType.String => {
                const obj = self.as_string();
                debug.print("{s}", .{obj.chars});
            },
        }
    }

    pub fn destroy(self: *Obj) void {
        switch (self.kind) {
            ObjType.String => {
                const obj: *String = @fieldParentPtr("obj", self);
                obj.destroy();
            },
        }
    }

    pub fn as_string(self: *Obj) *String {
        std.debug.assert(self.kind == ObjType.String);
        return @fieldParentPtr("obj", self);
    }
};
