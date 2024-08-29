const std = @import("std");
const debug = std.debug;
const Allocator = std.mem.Allocator;

const constants = @import("./constant.zig");

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./opcode.zig").OpCode;
const Value = @import("./values.zig").Value;
const Obj = @import("./object.zig").Obj;
const ObjType = @import("./object.zig").ObjType;
const NativeFn = @import("./object.zig").NativeFn;
const Table = @import("./table.zig").Table;

const compile = @import("./compile.zig").compile;
const compute_hash = @import("./utils.zig").compute_hash;

const print_value = @import("./values.zig").print_value;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const CallFrame = struct {
    function: *Obj.Function,
    ip: usize,
    // pointer to stack index provided to this frame
    slots_idx: usize,
};

pub const VM = struct {
    allocator: Allocator,
    stack: [constants.STACK_MAX]Value,
    stack_top: usize,
    // Keeping creating objects in references to destroy objects on cleaning.
    // In the book, a linked list between objects is used to handle this.
    references: std.ArrayList(*Obj),
    strings: Table,
    globals: Table,
    frames: [constants.FRAMES_MAX]CallFrame,
    frame_count: usize,

    pub fn new(allocator: Allocator) VM {
        return VM{
            .allocator = allocator,
            .stack = undefined,
            .stack_top = 0,
            .references = std.ArrayList(*Obj).init(allocator),
            .strings = Table.new(allocator),
            .globals = Table.new(allocator),
            .frames = undefined,
            .frame_count = 0,
        };
    }

    pub fn init_vm(self: *VM) void {
        self.define_native("clock", clock_native);
    }

    pub fn destroy(self: *VM) void {
        if (constants.DEBUG_PRINT_INTERNAL_STRINGS) {
            self.strings.dump();
        }

        self.strings.destroy();
        self.globals.destroy();
        self.clean_references();
        self.references.deinit();
    }

    inline fn current_chunk(self: *VM) *Chunk {
        return self.frames[self.frame_count - 1].function.chunk;
    }

    inline fn current_frame(self: *VM) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }

    pub fn interpret(self: *VM, content: []const u8) !InterpretResult {
        var function = try compile(self, content);
        if (function == null) {
            return InterpretResult.COMPILE_ERROR;
        }
        defer function.?.destroy();

        _ = try self.push(Value.obj_val(&function.?.obj));
        _ = self.call(function.?, 0);

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
                _ = self.current_chunk().dissassemble_instruction(self.current_frame().ip);
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
        const b = self.pop().as_cstring();
        const a = self.pop().as_cstring();

        const concat_str = try std.mem.concat(self.current_chunk().allocator, u8, &.{ a, b });

        var string_obj = self.take_string(concat_str);

        self.add_reference(&string_obj.obj);

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
            const function = frame.function;
            const instruction = frame.ip;

            debug.print("[line {d}] in ", .{function.chunk.lines[instruction]});

            if (function.name == null) {
                debug.print("script\n", .{});
            } else {
                debug.print("{s}()\n", .{function.name.?.chars});
            }

            if (frame_idx == 0) {
                break;
            }
            frame_idx -= 1;
        }
    }

    pub fn add_reference(self: *VM, obj: *Obj) void {
        // do not add duplicate references
        for (self.references.items) |item| {
            if (item == obj) {
                return;
            }
        }
        // XXX TODO catch unreachable to prevents
        self.references.append(obj) catch unreachable;
    }

    pub fn clean_references(self: *VM) void {
        for (self.references.items) |item| {
            item.destroy();
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
        const obj_string = Obj.String.new(self.allocator, source);
        _ = self.strings.set(obj_string, Value.nil_val());

        return obj_string;
    }

    pub fn call_value(self: *VM, callee: Value, arg_count: usize) bool {
        if (callee.is_obj()) {
            switch (callee.as_obj().kind) {
                ObjType.Function => {
                    return self.call(callee.as_obj().as_function(), arg_count);
                },
                ObjType.Native => {
                    const native_obj: *Obj.Native = callee.as_obj().as_native();
                    const value = native_obj.native(
                        arg_count,
                        self.stack[self.current_frame().slots_idx - arg_count .. self.current_frame().slots_idx],
                    );
                    self.stack_top -= arg_count + 1;
                    _ = try self.push(value);
                    return true;
                },
                else => {},
            }
        }
        self.runtime_error("Can only call functions and classes.");
        return false;
    }

    pub fn call(self: *VM, function: *Obj.Function, arg_count: usize) bool {
        if (arg_count != function.arity) {
            self.runtime_error("Invalid argument count.");
            // runtimeError("Expected %d arguments but got %d.", function->arity, argCount);
            return false;
        }

        if (self.frame_count == constants.FRAMES_MAX) {
            self.runtime_error("Stack overflow.");
            return false;
        }

        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;

        frame.function = function;
        frame.ip = 0;
        frame.slots_idx = self.stack_top - arg_count - 1;

        return true;
    }

    pub fn define_native(self: *VM, name: []const u8, native_fn: NativeFn) void {
        _ = try self.push(Value.obj_val(&self.copy_string(name).obj));
        _ = try self.push(Value.obj_val(&Obj.Native.new(self.allocator, native_fn).obj));

        _ = self.globals.set(self.stack[0].as_string(), self.stack[1]);

        _ = self.pop();
        _ = self.pop();
    }

    pub fn clock_native(arg_count: usize, args: []Value) Value {
        const ts = std.time.milliTimestamp();
        _ = arg_count;
        _ = args;
        return Value.number_val(@floatFromInt(ts));
    }
};
