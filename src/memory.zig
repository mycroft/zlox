const std = @import("std");

const VM = @import("./vm.zig").VM;
const Obj = @import("./object.zig").Obj;
const ObjType = @import("./object.zig").ObjType;
const Table = @import("./table.zig").Table;
const Entry = @import("./table.zig").Entry;
const Value = @import("./values.zig").Value;
const Compiler = @import("./compile.zig").Compiler;
const ValueArray = @import("./values.zig").ValueArray;

const constants = @import("./constant.zig");

pub const ZloxAllocator = struct {
    parent_allocator: std.mem.Allocator,
    vm: *VM,
    bytes_allocated: usize,
    next_gc: usize,
    current_gc: bool,

    const Self = @This();

    pub fn init(parent_allocator: std.mem.Allocator, vm: *VM) Self {
        return .{
            .parent_allocator = parent_allocator,
            .vm = vm,
            .bytes_allocated = 0,
            .next_gc = 4096,
            .current_gc = false,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));

        const res = self.parent_allocator.rawAlloc(len, ptr_align, ret_addr);

        if (self.bytes_allocated > self.next_gc) {
            self.collect_garbage();
        }

        self.bytes_allocated += len;

        if (constants.DEBUG_LOG_GC) {
            if (res == null) {
                std.debug.print("GC: failed allocing buffer of size {d}\n", .{len});
            } else {
                std.debug.print("GC: allocing buffer {*} of size {d}\n", .{ res.?, len });
            }
        }

        return res;
    }

    fn resize(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));

        if (constants.DEBUG_LOG_GC) {
            std.debug.print("GC: resizing buffer {*} from size {d} to size {d}\n", .{ buf, buf.len, new_len });
        }

        self.bytes_allocated += new_len - buf.len;

        if (self.bytes_allocated > self.next_gc or constants.DEBUG_STRESS_GC) {
            self.collect_garbage();
        }

        return self.parent_allocator.rawResize(buf, log2_buf_align, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, ret_addr: usize) void {
        const self: *Self = @ptrCast(@alignCast(ctx));

        if (constants.DEBUG_LOG_GC) {
            std.debug.print("GC: freeing buffer {*} of size {d} ({d}/{d})\n", .{ &buf, buf.len, self.bytes_allocated, self.next_gc });
        }

        return self.parent_allocator.rawFree(buf, log2_buf_align, ret_addr);
    }

    pub fn set_vm(self: *Self, vm: *VM) void {
        self.vm = vm;
    }

    pub fn grow_capacity(capacity: usize) usize {
        if (capacity < 8) {
            return 8;
        }
        return capacity * 2;
    }

    pub fn collect_garbage(self: *Self) void {
        if (self.current_gc) {
            return;
        }
        if (comptime constants.DEBUG_LOG_GC == true) {
            std.debug.print("\nGC: collect_garbage(): begin\n", .{});
        }
        self.current_gc = true;

        self.mark_roots();
        self.trace_references();
        self.table_remove_white(&self.vm.strings);

        self.sweep();

        self.next_gc = self.bytes_allocated * constants.GC_HEAP_GROW_FACTOR;

        if (comptime constants.DEBUG_LOG_GC == true) {
            std.debug.print("GC: collect_garbage(): end\n\n", .{});
        }
        self.current_gc = false;
    }

    pub fn mark_roots(self: *Self) void {
        for (0..self.vm.stack_top) |stack_idx| {
            self.mark_value(&self.vm.stack[stack_idx]);
        }

        for (0..self.vm.frame_count) |frame_idx| {
            self.mark_object(&self.vm.frames[frame_idx].closure.obj);
        }

        var upvalue = self.vm.open_upvalues;
        while (upvalue != null) {
            self.mark_object(&upvalue.?.obj);
            upvalue = upvalue.?.next;
        }

        self.mark_table(&self.vm.globals);

        self.mark_compiler_roots();

        self.mark_object(&self.vm.init_string.?.obj);
    }

    pub fn mark_value(self: *Self, value: *Value) void {
        if (value.is_obj()) {
            self.mark_object(value.as_obj());
        }
    }

    pub fn mark_object(self: *Self, obj: *Obj) void {
        if (obj.is_marked) {
            return;
        }

        if (constants.DEBUG_LOG_GC) {
            std.debug.print("GC: mark {*} ", .{obj});
            obj.print();
            std.debug.print("\n", .{});
        }
        obj.is_marked = true;

        if (self.vm.gray_capacity < self.vm.gray_count + 1) {
            self.vm.gray_capacity = grow_capacity(self.vm.gray_capacity);
            self.vm.gray_stack = self.allocator().realloc(self.vm.gray_stack.?, self.vm.gray_capacity) catch {
                @panic("failed to realloc gray stack");
            };
        }

        // doing a realloc here will likely recall mark_roots and so on.
        self.vm.gray_stack.?[self.vm.gray_count] = obj;
        self.vm.gray_count += 1;
    }

    pub fn mark_table(self: *Self, table: *Table) void {
        for (0..table.capacity) |idx| {
            const entry = &table.entries[idx];
            if (entry.key != null) {
                self.mark_object(&entry.key.?.obj);
            }

            self.mark_value(&entry.value);
        }
    }

    pub fn mark_compiler_roots(self: *Self) void {
        if (self.vm.parser == null) {
            return;
        }
        var compiler: ?*Compiler = self.vm.parser.?.compiler;

        while (compiler != null) {
            self.mark_object(&compiler.?.function.obj);
            compiler = compiler.?.enclosing;
        }
    }

    pub fn trace_references(self: *Self) void {
        while (self.vm.gray_count > 0) {
            self.vm.gray_count -= 1;
            const obj: *Obj = self.vm.gray_stack.?[self.vm.gray_count];
            self.blacken_object(obj);
        }
    }

    pub fn blacken_object(self: *Self, obj: *Obj) void {
        if (constants.DEBUG_LOG_GC) {
            std.debug.print("GC: {*} blacken ", .{obj});
            obj.print();
            std.debug.print("\n", .{});
        }
        switch (obj.kind) {
            ObjType.Native, ObjType.String => {},
            ObjType.Upvalue => self.mark_value(&obj.as_upvalue().closed),
            ObjType.Function => {
                const function: *Obj.Function = obj.as_function();
                if (function.name != null) {
                    self.mark_object(&function.name.?.obj);
                }

                self.mark_array(&function.chunk.constants);
            },
            ObjType.Closure => {
                const closure: *Obj.Closure = obj.as_closure();
                self.mark_object(&closure.function.obj);
                for (0..closure.upvalue_count) |i| {
                    if (closure.upvalues[i] != null) {
                        self.mark_object(&closure.upvalues[i].?.obj);
                    }
                }
            },
            ObjType.Class => {
                const class: *Obj.Class = obj.as_class();
                self.mark_object(&class.name.obj);
                self.mark_table(&class.methods);
            },
            ObjType.Instance => {
                const instance: *Obj.Instance = obj.as_instance();
                self.mark_object(&instance.class.obj);
                self.mark_table(&instance.fields);
            },
            ObjType.BoundMethod => {
                const bound_method: *Obj.BoundMethod = obj.as_bound_method();
                self.mark_value(&bound_method.receiver);
                self.mark_object(&bound_method.method.obj);
            },
        }
    }

    pub fn mark_array(self: *Self, value_array: *ValueArray) void {
        for (0..value_array.count) |i| {
            self.mark_value(&value_array.values[i]);
        }
    }

    pub fn table_remove_white(self: *Self, table: *Table) void {
        _ = self;

        for (0..table.capacity) |idx| {
            const entry: *Entry = &table.entries[idx];
            if (entry.key != null and !entry.key.?.obj.is_marked) {
                if (comptime constants.DEBUG_LOG_GC) {
                    std.debug.print("GC: table_remove_white: deleting {s}\n", .{entry.key.?.chars});
                }
                _ = table.del(entry.key.?);
            }
        }
    }

    pub fn sweep(self: *Self) void {
        var previous: ?*Obj = null;
        var object: ?*Obj = self.vm.objects;

        while (object != null) {
            if (object.?.is_marked) {
                object.?.is_marked = false;
                previous = object;
                object = object.?.next;
            } else {
                const unreached: *Obj = object.?;
                object = object.?.next;

                if (previous != null) {
                    previous.?.next = object;
                } else {
                    self.vm.objects = object;
                }

                if (comptime constants.DEBUG_LOG_GC) {
                    std.debug.print("GC: sweeping {*}: ", .{unreached});
                    unreached.print();
                    std.debug.print("\n", .{});
                }

                unreached.destroy();
            }
        }
    }
};
