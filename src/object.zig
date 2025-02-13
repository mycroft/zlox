const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const Table = @import("./table.zig").Table;
const Value = @import("./values.zig").Value;
const VM = @import("./vm.zig").VM;

const compute_hash = @import("./utils.zig").compute_hash;

pub const ObjType = enum {
    String,
    Function,
    Native,
    Closure,
    Upvalue,
    Class,
    Instance,
    BoundMethod,
};

pub const NativeFn = *const fn (vm: *VM, arg_count: usize, args: []Value) Value;

pub const Obj = struct {
    kind: ObjType,
    allocator: Allocator,
    next: ?*Obj,
    is_marked: bool,

    fn new(comptime T: type, vm: *VM, kind: ObjType) *T {
        const created_obj = vm.allocator.create(T) catch unreachable;

        created_obj.obj = Obj{
            .kind = kind,
            .allocator = vm.allocator,
            .next = vm.objects,
            .is_marked = false,
        };

        vm.objects = &created_obj.obj;

        return created_obj;
    }

    pub fn destroy(self: *Obj) void {
        switch (self.kind) {
            ObjType.String => self.as_string().destroy(),
            ObjType.Function => self.as_function().destroy(),
            ObjType.Native => self.as_native().destroy(),
            ObjType.Closure => self.as_closure().destroy(),
            ObjType.Upvalue => self.as_upvalue().destroy(),
            ObjType.Class => self.as_class().destroy(),
            ObjType.Instance => self.as_instance().destroy(),
            ObjType.BoundMethod => self.as_bound_method().destroy(),
        }
    }

    pub const String = struct {
        obj: Obj,
        chars: []const u8,
        hash: u32,

        pub fn new(vm: *VM, chars: []const u8) *String {
            const str_obj = Obj.new(String, vm, ObjType.String);

            str_obj.chars = chars;
            str_obj.hash = compute_hash(str_obj.chars);

            return str_obj;
        }

        pub fn destroy(self: *String) void {
            self.obj.allocator.free(self.chars);
            self.obj.allocator.destroy(self);
        }
    };

    pub const Function = struct {
        obj: Obj,
        arity: usize,
        upvalue_count: usize,
        chunk: *Chunk,
        name: ?*Obj.String,

        pub fn new(vm: *VM) *Function {
            const function_obj = Obj.new(Function, vm, ObjType.Function);

            function_obj.arity = 0;
            function_obj.upvalue_count = 0;
            function_obj.chunk = Chunk.new(vm.allocator, vm);
            function_obj.name = null;

            return function_obj;
        }

        pub fn destroy(self: *Function) void {
            self.chunk.destroy();
            self.obj.allocator.destroy(self);
        }
    };

    pub const Native = struct {
        obj: Obj,
        native: NativeFn,

        pub fn new(vm: *VM, native: NativeFn) *Native {
            const native_obj = Obj.new(Native, vm, ObjType.Native);

            native_obj.native = native;

            return native_obj;
        }

        pub fn destroy(self: *Native) void {
            self.obj.allocator.destroy(self);
        }
    };

    pub const Closure = struct {
        obj: Obj,
        function: *Obj.Function,
        upvalues: []?*Obj.Upvalue,
        upvalue_count: usize,

        pub fn new(vm: *VM, function: *Obj.Function) *Closure {
            const closure_obj = Obj.new(Closure, vm, ObjType.Closure);

            closure_obj.function = function;
            closure_obj.upvalue_count = function.upvalue_count;

            closure_obj.upvalues = vm.allocator.alloc(?*Obj.Upvalue, function.upvalue_count) catch unreachable;

            for (0..function.upvalue_count) |i| {
                closure_obj.upvalues[i] = null;
            }

            return closure_obj;
        }

        pub fn destroy(self: *Closure) void {
            self.obj.allocator.free(self.upvalues);
            self.obj.allocator.destroy(self);
        }
    };

    pub const Upvalue = struct {
        obj: Obj,
        location: *Value,
        next: ?*Obj.Upvalue,
        closed: Value,

        pub fn new(vm: *VM, slot: *Value) *Upvalue {
            const upvalue_obj = Obj.new(Upvalue, vm, ObjType.Upvalue);

            upvalue_obj.location = slot;
            upvalue_obj.next = null;
            upvalue_obj.closed = Value.nil_val();

            return upvalue_obj;
        }

        pub fn destroy(self: *Upvalue) void {
            self.obj.allocator.destroy(self);
        }
    };

    pub const Class = struct {
        obj: Obj,
        name: *Obj.String,
        methods: Table,

        pub fn new(vm: *VM, name: *Obj.String) *Class {
            const class_obj = Obj.new(Class, vm, ObjType.Class);

            class_obj.name = name;
            class_obj.methods = Table.new(vm.allocator);

            return class_obj;
        }

        pub fn destroy(self: *Class) void {
            self.methods.destroy();
            self.obj.allocator.destroy(self);
        }
    };

    pub const Instance = struct {
        obj: Obj,
        class: *Obj.Class,
        fields: Table,

        pub fn new(vm: *VM, class: *Obj.Class) *Instance {
            const instance_obj = Obj.new(Instance, vm, ObjType.Instance);

            instance_obj.class = class;
            instance_obj.fields = Table.new(vm.allocator);

            return instance_obj;
        }

        pub fn destroy(self: *Instance) void {
            self.fields.destroy();
            self.obj.allocator.destroy(self);
        }
    };

    pub const BoundMethod = struct {
        obj: Obj,
        receiver: Value,
        method: *Obj.Closure,

        pub fn new(vm: *VM, receiver: Value, method: *Obj.Closure) *BoundMethod {
            const bound_method = Obj.new(BoundMethod, vm, ObjType.BoundMethod);

            bound_method.receiver = receiver;
            bound_method.method = method;

            return bound_method;
        }

        pub fn destroy(self: *BoundMethod) void {
            self.obj.allocator.destroy(self);
        }
    };

    pub fn is_type(self: *Obj, kind: ObjType) bool {
        return self.kind == kind;
    }

    pub fn is_string(self: *Obj) bool {
        return self.is_type(ObjType.String);
    }

    pub fn is_function(self: *Obj) bool {
        return self.is_type(ObjType.Function);
    }

    pub fn is_native(self: *Obj) bool {
        return self.is_type(ObjType.Native);
    }

    pub fn is_closure(self: *Obj) bool {
        return self.is_type(ObjType.Closure);
    }

    pub fn is_upvalue(self: *Obj) bool {
        return self.is_type(ObjType.Upvalue);
    }

    pub fn is_class(self: *Obj) bool {
        return self.is_type(ObjType.Class);
    }

    pub fn is_instance(self: *Obj) bool {
        return self.is_type(ObjType.Instance);
    }

    pub fn is_bound_method(self: *Obj) bool {
        return self.is_type(ObjType.BoundMethod);
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
            ObjType.Native => {
                debug.print("<native fn>", .{});
            },
            ObjType.Closure => {
                const obj = self.as_closure();
                obj.function.obj.print();
            },
            ObjType.Upvalue => {
                debug.print("upvalue", .{});
            },
            ObjType.Class => {
                const obj = self.as_class();
                debug.print("{s}", .{obj.name.chars});
            },
            ObjType.Instance => {
                const obj = self.as_instance();
                debug.print("{s} instance", .{obj.class.name.chars});
            },
            ObjType.BoundMethod => {
                const obj = self.as_bound_method();
                obj.method.function.obj.print();
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

    pub fn as_native(self: *Obj) *Native {
        std.debug.assert(self.kind == ObjType.Native);
        return @fieldParentPtr("obj", self);
    }

    pub fn as_closure(self: *Obj) *Closure {
        std.debug.assert(self.kind == ObjType.Closure);
        return @fieldParentPtr("obj", self);
    }

    pub fn as_upvalue(self: *Obj) *Upvalue {
        std.debug.assert(self.kind == ObjType.Upvalue);
        return @fieldParentPtr("obj", self);
    }

    pub fn as_class(self: *Obj) *Class {
        std.debug.assert(self.kind == ObjType.Class);
        return @fieldParentPtr("obj", self);
    }

    pub fn as_instance(self: *Obj) *Instance {
        std.debug.assert(self.kind == ObjType.Instance);
        return @fieldParentPtr("obj", self);
    }

    pub fn as_bound_method(self: *Obj) *BoundMethod {
        std.debug.assert(self.kind == ObjType.BoundMethod);
        return @fieldParentPtr("obj", self);
    }
};
