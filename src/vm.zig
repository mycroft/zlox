const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const constants = @import("./constant.zig");

const ZloxAllocator = @import("./memory.zig").ZloxAllocator;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const Value = @import("./values.zig").Value;
const Obj = @import("./object.zig").Obj;
const ObjType = @import("./object.zig").ObjType;
const Parser = @import("./compile.zig").Parser;
const NativeFn = @import("./object.zig").NativeFn;
const Table = @import("./table.zig").Table;

const natives = @import("./native.zig");

const compile = @import("./compile.zig").compile;
const compute_hash = @import("./utils.zig").compute_hash;

const print_value = @import("./values.zig").print_value;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const CallFrame = struct {
    closure: *Obj.Closure,
    ip: usize,
    // pointer to stack index provided to this frame
    slots_idx: usize,
};

pub const VM = struct {
    allocator: Allocator,
    stack: [constants.STACK_MAX]Value,
    stack_top: usize,
    strings: Table,
    globals: Table,
    frames: [constants.FRAMES_MAX]CallFrame,
    frame_count: usize,
    open_upvalues: ?*Obj.Upvalue,
    objects: ?*Obj,
    parser: ?*Parser,
    gray_count: usize,
    gray_capacity: usize,
    gray_stack: ?[]*Obj,
    init_string: ?*Obj.String,

    pub fn new() VM {
        const vm = VM{
            .allocator = undefined,
            .stack = undefined,
            .stack_top = 0,
            .strings = undefined,
            .globals = undefined,
            .frames = undefined,
            .frame_count = 0,
            .open_upvalues = null,
            .objects = null,
            .parser = null,
            .gray_capacity = 0,
            .gray_count = 0,
            .gray_stack = &.{},
            .init_string = null,
        };

        return vm;
    }

    pub fn init_vm(self: *VM, allocator: Allocator) void {
        self.allocator = allocator;
        self.globals = Table.new(self.allocator);
        self.strings = Table.new(self.allocator);

        _ = try self.push(Value.obj_val(&self.copy_string("init").obj));
        self.init_string = self.pop().as_string();

        self.define_native("clock", natives.clock);
        self.define_native("power", natives.power);
        self.define_native("str2num", natives.str2num);
        self.define_native("num2str", natives.num2str);
    }

    pub fn destroy(self: *VM) void {
        if (constants.DEBUG_PRINT_INTERNAL_STRINGS) {
            self.strings.dump();
        }

        self.strings.destroy();
        self.globals.destroy();
        self.init_string = null;
        self.destroy_objects();

        if (self.gray_stack != null) {
            self.allocator.free(self.gray_stack.?);
        }
    }

    pub fn destroy_objects(self: *VM) void {
        var obj = self.objects;
        while (obj != null) {
            const obj_next = obj.?.next;
            obj.?.destroy();
            obj = obj_next;
        }
    }

    pub fn dump_objects(self: *VM) void {
        var obj = self.objects;
        while (obj != null) {
            const obj_next = obj.?.next;
            std.debug.print("OBJ: {*} {any}", .{ obj.?, obj.?.kind });
            // obj.?.print();
            std.debug.print("\n", .{});
            obj = obj_next;
        }
    }

    inline fn current_chunk(self: *VM) *Chunk {
        return self.frames[self.frame_count - 1].closure.function.chunk;
    }

    inline fn current_frame(self: *VM) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }

    pub fn interpret(self: *VM, content: []const u8) !InterpretResult {
        var function = try compile(self, content);
        if (function == null) {
            return InterpretResult.COMPILE_ERROR;
        }

        _ = try self.push(Value.obj_val(&function.?.obj));
        const closure: *Obj.Closure = Obj.Closure.new(self, function.?);
        _ = self.pop();
        _ = try self.push(Value.obj_val(&closure.obj));
        _ = self.call(closure, 0);

        return try self.run();
    }

    pub fn run(self: *VM) !InterpretResult {
        while (true) {
            if (constants.DEBUG_TRACE_EXECUTION) {
                if (self.stack_top > 0) {
                    debug.print("{s:10}", .{""});
                    for (0..self.stack_top) |item_idx| {
                        debug.print("[ ", .{});
                        print_value(self.stack[item_idx]);
                        debug.print(" ]", .{});
                    }
                    debug.print("\n", .{});
                }
                _ = self.current_chunk().disassemble_instruction(self.current_frame().ip);
            }

            const instruction = self.read_byte();

            switch (instruction) {
                @intFromEnum(OpCode.OP_CONSTANT) => {
                    const constant = self.read_constant();
                    try self.push(constant);
                },
                @intFromEnum(OpCode.OP_NIL) => try self.push(Value.nil_val()),
                @intFromEnum(OpCode.OP_FALSE) => try self.push(Value.bool_val(false)),
                @intFromEnum(OpCode.OP_TRUE) => try self.push(Value.bool_val(true)),
                @intFromEnum(OpCode.OP_POP) => _ = self.pop(),
                @intFromEnum(OpCode.OP_ADD),
                @intFromEnum(OpCode.OP_SUBSTRACT),
                @intFromEnum(OpCode.OP_MULTIPLY),
                @intFromEnum(OpCode.OP_DIVIDE),
                @intFromEnum(OpCode.OP_LESS),
                @intFromEnum(OpCode.OP_GREATER),
                => {
                    const res = try self.binary_op(@enumFromInt(instruction));
                    if (res != InterpretResult.OK) {
                        return res;
                    }
                },
                @intFromEnum(OpCode.OP_NOT) => {
                    try self.push(Value.bool_val(self.pop().is_falsey()));
                },
                @intFromEnum(OpCode.OP_NEGATE) => {
                    if (!self.peek(0).is_number()) {
                        self.runtime_error("Operand must be a number.");
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    try self.push(Value.number_val(-self.pop().as_number()));
                },
                @intFromEnum(OpCode.OP_PRINT) => {
                    print_value(self.pop());
                    debug.print("\n", .{});
                },
                @intFromEnum(OpCode.OP_RETURN) => {
                    const result = self.pop();
                    self.close_upvalues(&self.stack[self.current_frame().slots_idx]);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return InterpretResult.OK;
                    }

                    self.stack_top = self.frames[self.frame_count].slots_idx;
                    try self.push(result);
                },
                @intFromEnum(OpCode.OP_EQUAL) => {
                    try self.push(Value.bool_val(self.pop().equals(self.pop())));
                },
                @intFromEnum(OpCode.OP_DEFINE_GLOBAL) => {
                    const name = self.read_constant().as_string();

                    _ = self.globals.set(name, self.peek(0));
                    _ = self.pop();
                },
                @intFromEnum(OpCode.OP_GET_GLOBAL) => {
                    const name: *Obj.String = self.read_constant().as_string();
                    var value = Value.nil_val();

                    if (!self.globals.get(name, &value)) {
                        const err_msg = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'.", .{name.chars});
                        defer self.allocator.free(err_msg);
                        self.runtime_error(err_msg);
                        return InterpretResult.RUNTIME_ERROR;
                    }

                    try self.push(value);
                },
                @intFromEnum(OpCode.OP_SET_GLOBAL) => {
                    const name: *Obj.String = self.read_constant().as_string();

                    if (self.globals.set(name, self.peek(0))) {
                        _ = self.globals.del(name);

                        const err_msg = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'.", .{name.chars});
                        defer self.allocator.free(err_msg);

                        self.runtime_error(err_msg);
                        return InterpretResult.RUNTIME_ERROR;
                    }
                },
                @intFromEnum(OpCode.OP_GET_LOCAL) => {
                    const slot = self.read_byte();
                    try self.push(self.stack[self.current_frame().slots_idx + slot]);
                },
                @intFromEnum(OpCode.OP_SET_LOCAL) => {
                    const slot = self.read_byte();
                    self.stack[self.current_frame().slots_idx + slot] = self.peek(0);
                },
                @intFromEnum(OpCode.OP_JUMP) => {
                    const offset = self.read_short();
                    self.current_frame().ip += offset;
                },
                @intFromEnum(OpCode.OP_JUMP_IF_FALSE) => {
                    const offset = self.read_short();
                    if (self.peek(0).is_falsey()) {
                        self.current_frame().ip += offset;
                    }
                },
                @intFromEnum(OpCode.OP_LOOP) => {
                    const offset = self.read_short();
                    self.current_frame().ip -= offset;
                },
                @intFromEnum(OpCode.OP_CALL) => {
                    const arg_count = self.read_byte();
                    if (!self.call_value(self.peek(arg_count), arg_count)) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                },
                @intFromEnum(OpCode.OP_CLOSURE) => {
                    const function = self.read_constant().as_obj().as_function();
                    const closure = Obj.Closure.new(self, function);
                    _ = try self.push(Value.obj_val(&closure.obj));
                    for (0..closure.upvalue_count) |i| {
                        const is_local = self.read_byte();
                        const index = self.read_byte();
                        if (is_local == 1) {
                            const value_idx = self.current_frame().slots_idx + index;
                            closure.upvalues[i] = self.capture_upvalue(&self.stack[value_idx]);
                        } else {
                            closure.upvalues[i] = self.current_frame().closure.upvalues[index];
                        }
                    }
                },
                @intFromEnum(OpCode.OP_GET_UPVALUE) => {
                    const slot = self.read_byte();
                    try self.push(self.current_frame().closure.upvalues[slot].?.location.*);
                },
                @intFromEnum(OpCode.OP_SET_UPVALUE) => {
                    const slot = self.read_byte();
                    self.current_frame().closure.upvalues[slot].?.location = @constCast(&self.peek(0));
                },
                @intFromEnum(OpCode.OP_CLOSE_UPVALUE) => {
                    self.close_upvalues(&self.stack[self.stack_top - 1]);
                    _ = self.pop();
                },
                @intFromEnum(OpCode.OP_CLASS) => {
                    const name: *Obj.String = self.read_constant().as_string();
                    _ = try self.push(Value.obj_val(&Obj.Class.new(self, name).obj));
                },
                @intFromEnum(OpCode.OP_GET_PROPERTY) => {
                    if (!self.peek(0).is_obj() or !self.peek(0).as_obj().is_instance()) {
                        self.runtime_error("Only instances have properties.");
                        return InterpretResult.RUNTIME_ERROR;
                    }

                    const instance = self.peek(0).as_obj().as_instance();
                    const name = self.read_constant().as_string();

                    var value = Value.nil_val();

                    if (instance.fields.get(name, &value)) {
                        _ = self.pop();
                        _ = try self.push(value);
                        continue;
                    }

                    if (!self.bind_method(instance.class, name)) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                },
                @intFromEnum(OpCode.OP_SET_PROPERTY) => {
                    if (!self.peek(1).is_obj() or !self.peek(1).as_obj().is_instance()) {
                        self.runtime_error("Only instances have fields.");
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    const instance = self.peek(1).as_obj().as_instance();
                    _ = instance.fields.set(self.read_constant().as_string(), self.peek(0));

                    const value = self.pop();
                    _ = self.pop();
                    _ = try self.push(value);
                },
                @intFromEnum(OpCode.OP_METHOD) => {
                    self.define_method(self.read_constant().as_string());
                },
                @intFromEnum(OpCode.OP_INDEX_GET) => {
                    if (!self.peek(0).is_number() or !self.peek(1).is_string()) {
                        self.runtime_error("A number and a string are required for indexes.");
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    const index_val = self.pop();
                    const value = self.pop();

                    const index: usize = @as(usize, @intFromFloat(index_val.as_number()));
                    if (index >= value.as_cstring().len) {
                        self.runtime_error("The index must be set between 0 and string len.");
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    const c = value.as_cstring()[index .. index + 1];

                    _ = try self.push(Value.obj_val(&self.copy_string(c).obj));
                },
                @intFromEnum(OpCode.OP_INDEX_SET) => {
                    const value = self.pop();
                    const index_val = self.pop();
                    const origin = self.pop();

                    if (!value.is_string() or value.as_cstring().len != 1) {
                        self.runtime_error("Value to assign must be one byte.");
                        return InterpretResult.RUNTIME_ERROR;
                    }

                    const index: usize = @as(usize, @intFromFloat(index_val.as_number()));

                    var str = self.allocator.dupe(u8, origin.as_cstring()) catch unreachable;
                    str[index] = value.as_cstring()[0];

                    _ = try self.push(Value.obj_val(&self.take_string(str).obj));
                },
                @intFromEnum(OpCode.OP_INVOKE) => {
                    const method = self.read_constant().as_string();
                    const arg_count = self.read_byte();
                    if (!self.invoke(method, arg_count)) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                },
                @intFromEnum(OpCode.OP_INHERIT) => {
                    const superclass = self.peek(1);
                    const subclass = self.peek(0).as_obj().as_class();

                    if (!superclass.is_obj() or !superclass.as_obj().is_class()) {
                        self.runtime_error("Superclass must be a class.");
                        return InterpretResult.RUNTIME_ERROR;
                    }

                    superclass.as_obj().as_class().methods.add_all(&subclass.methods);
                    _ = self.pop(); // subclass
                },
                @intFromEnum(OpCode.OP_GET_SUPER) => {
                    const name: *Obj.String = self.read_constant().as_string();
                    const superclass = self.pop().as_obj().as_class();

                    if (!self.bind_method(superclass, name)) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                },
                @intFromEnum(OpCode.OP_SUPER_INVOKE) => {
                    const method = self.read_constant().as_string();
                    const arg_count = self.read_byte();
                    const superclass = self.pop().as_obj().as_class();

                    if (!self.invoke_from_class(superclass, method, arg_count)) {
                        return InterpretResult.RUNTIME_ERROR;
                    }
                },
                else => {
                    debug.print("Invalid instruction: {d}\n", .{instruction});
                    return InterpretResult.RUNTIME_ERROR;
                },
            }
        }

        return InterpretResult.OK;
    }

    // XXX In the book, we're using a ptr to data directly, to avoid dereferencing to a given offset
    // How to do that in Zig?
    pub fn read_byte(self: *VM) u8 {
        const ret = self.current_chunk().code[self.current_frame().ip];
        self.current_frame().ip += 1;

        return ret;
    }

    pub fn read_short(self: *VM) u16 {
        self.current_frame().ip += 2;

        return (@as(u16, self.current_chunk().code[self.current_frame().ip - 2]) << 8) | (@as(u16, self.current_chunk().code[self.current_frame().ip - 1]));
    }

    pub fn read_constant(self: *VM) Value {
        return self.current_chunk().constants.values[read_byte(self)];
    }

    pub fn push(self: *VM, value: Value) !void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *VM) Value {
        const value = self.stack[self.stack_top - 1];
        self.stack_top -= 1;
        return value;
    }

    pub fn binary_op(self: *VM, op: OpCode) !InterpretResult {
        if (op == OpCode.OP_ADD and self.peek(0).is_string() and self.peek(1).is_string()) {
            try self.concatenate();
            return InterpretResult.OK;
        }

        if (!self.peek(0).is_number() or !self.peek(0).is_number()) {
            self.runtime_error("Operands must be two numbers or two strings");
            return InterpretResult.RUNTIME_ERROR;
        }

        const b = self.pop().as_number();
        const a = self.pop().as_number();

        const res: Value = switch (op) {
            OpCode.OP_ADD => Value.number_val(a + b),
            OpCode.OP_SUBSTRACT => Value.number_val(a - b),
            OpCode.OP_MULTIPLY => Value.number_val(a * b),
            OpCode.OP_DIVIDE => Value.number_val(a / b),
            OpCode.OP_LESS => Value.bool_val(a < b),
            OpCode.OP_GREATER => Value.bool_val(a > b),
            else => unreachable,
        };

        try self.push(res);

        return InterpretResult.OK;
    }

    pub fn concatenate(self: *VM) !void {
        const b = self.peek(0).as_cstring();
        const a = self.peek(1).as_cstring();

        const concat_str = try std.mem.concat(self.current_chunk().allocator, u8, &.{ a, b });
        const string_obj = self.take_string(concat_str);

        _ = self.pop();
        _ = self.pop();

        try self.push(Value.obj_val(&string_obj.obj));
    }

    pub fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stack_top - 1 - distance];
    }

    pub fn runtime_error(self: *VM, err_msg: []const u8) void {
        debug.print("err: {s}\n", .{err_msg});

        var frame_idx = self.frame_count - 1;

        while (true) {
            const frame = self.frames[frame_idx];
            const closure = frame.closure;
            const instruction = frame.ip;

            debug.print("[line {d}] in ", .{closure.function.chunk.lines[instruction]});

            if (closure.function.name == null) {
                debug.print("script\n", .{});
            } else {
                debug.print("{s}()\n", .{closure.function.name.?.chars});
            }

            if (frame_idx == 0) {
                break;
            }
            frame_idx -= 1;
        }
    }

    pub fn copy_string(self: *VM, source: []const u8) *Obj.String {
        const hash = compute_hash(source);
        const obj_string = self.strings.find_string(source, hash);

        if (obj_string != null) {
            return obj_string.?;
        }

        const copy: []const u8 = self.allocator.dupe(u8, source) catch unreachable;
        return self.allocate_string(copy);
    }

    pub fn take_string(self: *VM, source: []const u8) *Obj.String {
        const hash = compute_hash(source);
        const obj_string = self.strings.find_string(source, hash);

        if (obj_string != null) {
            // free given string
            self.allocator.free(source);
            return obj_string.?;
        }

        return self.allocate_string(source);
    }

    pub fn allocate_string(self: *VM, source: []const u8) *Obj.String {
        const obj_string = Obj.String.new(self, source);

        _ = try self.push(Value.obj_val(&obj_string.obj));
        _ = self.strings.set(obj_string, Value.nil_val());
        _ = self.pop();

        return obj_string;
    }

    pub fn call_value(self: *VM, callee: Value, arg_count: usize) bool {
        if (callee.is_obj()) {
            switch (callee.as_obj().kind) {
                ObjType.Function => {
                    return self.call(callee.as_obj().as_closure(), arg_count);
                },
                ObjType.Native => {
                    const native_obj: *Obj.Native = callee.as_obj().as_native();
                    const value = native_obj.native(
                        self,
                        arg_count,
                        self.stack[self.stack_top - arg_count .. self.stack_top],
                    );
                    self.stack_top -= arg_count + 1;
                    _ = try self.push(value);
                    return true;
                },
                ObjType.Closure => {
                    return self.call(callee.as_obj().as_closure(), arg_count);
                },
                ObjType.Class => {
                    const class = callee.as_obj().as_class();
                    self.stack[self.stack_top - arg_count - 1] = Value.obj_val(&Obj.Instance.new(self, class).obj);

                    var initializer = Value.nil_val();

                    if (class.methods.get(self.init_string.?, &initializer) == true) {
                        return self.call(initializer.as_obj().as_closure(), arg_count);
                    } else if (arg_count != 0) {
                        self.runtime_error("Expected 0 arguments."); // XXX show number of arguments.
                    }

                    return true;
                },
                ObjType.BoundMethod => {
                    const bound_method = callee.as_obj().as_bound_method();
                    self.stack[self.stack_top - arg_count - 1] = bound_method.receiver;
                    return self.call(bound_method.method, arg_count);
                },
                else => {},
            }
        }
        self.runtime_error("Can only call functions and classes.");
        return false;
    }

    pub fn bind_method(self: *VM, class: *Obj.Class, name: *Obj.String) bool {
        var method: Value = Value.nil_val();

        if (!class.methods.get(name, &method)) {
            const err_msg = std.fmt.allocPrint(self.allocator, "Undefined property '{s}'.", .{name.chars}) catch unreachable;
            defer self.allocator.free(err_msg);
            self.runtime_error(err_msg);

            return false;
        }

        const bound: *Obj.BoundMethod = Obj.BoundMethod.new(self, self.peek(0), method.as_obj().as_closure());
        _ = self.pop();
        try self.push(Value.obj_val(&bound.obj));

        return true;
    }

    pub fn call(self: *VM, closure: *Obj.Closure, arg_count: usize) bool {
        if (arg_count != closure.function.arity) {
            const err_msg = std.fmt.allocPrint(self.allocator, "Expected {d} arguments but got {d}.", .{ closure.function.arity, arg_count }) catch unreachable;
            defer self.allocator.free(err_msg);
            self.runtime_error(err_msg);
            return false;
        }

        if (self.frame_count == constants.FRAMES_MAX) {
            self.runtime_error("Stack overflow.");
            return false;
        }

        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;

        frame.closure = closure;
        frame.ip = 0;
        frame.slots_idx = self.stack_top - arg_count - 1;

        return true;
    }

    pub fn define_native(self: *VM, name: []const u8, native_fn: NativeFn) void {
        _ = try self.push(Value.obj_val(&self.copy_string(name).obj));
        _ = try self.push(Value.obj_val(&Obj.Native.new(self, native_fn).obj));

        _ = self.globals.set(self.stack[0].as_string(), self.stack[1]);

        _ = self.pop();
        _ = self.pop();
    }

    fn capture_upvalue(self: *VM, local: *Value) *Obj.Upvalue {
        var prev_upvalue: ?*Obj.Upvalue = null;
        var upvalue: ?*Obj.Upvalue = self.open_upvalues;

        while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local) {
            return upvalue.?;
        }

        const created_upvalue = Obj.Upvalue.new(self, local);
        created_upvalue.next = upvalue;

        if (prev_upvalue == null) {
            self.open_upvalues = created_upvalue;
        } else {
            prev_upvalue.?.next = created_upvalue;
        }

        return created_upvalue;
    }

    fn close_upvalues(self: *VM, last: *Value) void {
        while (self.open_upvalues != null and @intFromPtr(self.open_upvalues.?.location) >= @intFromPtr(last)) {
            const upvalue = self.open_upvalues.?;

            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
    }

    fn define_method(self: *VM, name: *Obj.String) void {
        const method = self.peek(0);
        const class = self.peek(1).as_obj().as_class();

        _ = class.methods.set(name, method);
        _ = self.pop();
    }

    fn invoke(self: *VM, name: *Obj.String, arg_count: usize) bool {
        const receiver = self.peek(arg_count);
        const instance = receiver.as_obj().as_instance();

        var value = Value.nil_val();
        if (instance.fields.get(name, &value)) {
            self.stack[self.stack_top - arg_count - 1] = value;

            return self.call_value(value, arg_count);
        }

        return self.invoke_from_class(instance.class, name, arg_count);
    }

    fn invoke_from_class(self: *VM, class: *Obj.Class, name: *Obj.String, arg_count: usize) bool {
        var method = Value.nil_val();
        if (!class.methods.get(name, &method)) {
            const err_msg = std.fmt.allocPrint(self.allocator, "Undefined property '{s}'.", .{name.chars}) catch unreachable;
            defer self.allocator.free(err_msg);
            self.runtime_error(err_msg);
            return false;
        }

        return self.call(method.as_obj().as_closure(), arg_count);
    }
};
