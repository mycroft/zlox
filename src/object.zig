const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;

const compute_hash = @import("./utils.zig").compute_hash;

pub const ObjType = enum {
    String,
    Function,
};

pub const Obj = struct {
    kind: ObjType,
    allocator: std.mem.Allocator,

    pub const String = struct {
        obj: Obj,
        chars: []const u8,
        hash: u32,

        pub fn new(allocator: std.mem.Allocator, chars: []const u8) *String {
            const obj = Obj{
                .kind = ObjType.String,
                .allocator = allocator,
            };

            const str_obj = allocator.create(String) catch unreachable;
            str_obj.obj = obj;

            str_obj.chars = chars;
            str_obj.hash = compute_hash(str_obj.chars);

            return str_obj;
        }

        pub fn destroy(self: *String) void {
            const allocator = self.obj.allocator;
            allocator.free(self.chars);
            allocator.destroy(self);
        }
    };

    pub const Function = struct {
        obj: Obj,
        arity: usize,
        chunk: Chunk,
        name: ?*Obj.String,

        pub fn new(allocator: std.mem.Allocator) *Function {
            const obj = Obj{
                .kind = ObjType.Function,
                .allocator = allocator,
            };

            const function_obj = allocator.create(Function) catch unreachable;
            function_obj.obj = obj;
            function_obj.arity = 0;
            function_obj.chunk = Chunk.new(allocator);
            function_obj.name = null;

            return function_obj;
        }

        pub fn destroy(self: *Function) void {
            const allocator = self.obj.allocator;
            self.chunk.deinit();
            allocator.destroy(self);
        }
    };

    pub fn is_type(self: *Obj, kind: ObjType) bool {
        return self.kind == kind;
    }

    pub fn is_string(self: *Obj) bool {
        return self.is_type(ObjType.String);
    }

    pub fn is_function(self: *Obj) bool {
        return self.is_function(ObjType.Function);
    }

    pub fn print(self: *Obj) void {
        switch (self.kind) {
            ObjType.String => {
                const obj = self.as_string();
                debug.print("{s}", .{obj.chars});
            },
            ObjType.Function => {
                const obj = self.as_function();
                if (obj.name == null) {
                    debug.print("<script>", .{});
                } else {
                    debug.print("<fn {s}>", .{obj.name.?.chars});
                }
            },
        }
    }

    pub fn destroy(self: *Obj) void {
        switch (self.kind) {
            ObjType.String => {
                const obj: *String = @fieldParentPtr("obj", self);
                obj.destroy();
            },
            ObjType.Function => {
                const obj: *Function = @fieldParentPtr("obj", self);
                obj.destroy();
            },
        }
    }

    pub fn as_string(self: *Obj) *String {
        std.debug.assert(self.kind == ObjType.String);
        return @fieldParentPtr("obj", self);
    }

    pub fn as_function(self: *Obj) *Function {
        std.debug.assert(self.kind == ObjType.Function);
        return @fieldParentPtr("obj", self);
    }
};
