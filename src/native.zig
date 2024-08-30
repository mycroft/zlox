const std = @import("std");

const Obj = @import("./object.zig").Obj;
const Value = @import("./values.zig").Value;
const VM = @import("./vm.zig").VM;

pub fn clock(vm: *VM, arg_count: usize, args: []Value) Value {
    _ = vm;
    _ = arg_count;
    _ = args;

    const ts = std.time.milliTimestamp();
    return Value.number_val(@floatFromInt(ts));
}

pub fn power(vm: *VM, arg_count: usize, args: []Value) Value {
    _ = vm;

    if (arg_count != 2) {
        std.debug.print("power() is expecting 2 arguments.\n", .{});
        return Value.nil_val();
    }

    if (!args[0].is_number() or !args[0].is_number()) {
        std.debug.print("args must be numbers.\n", .{});
        return Value.nil_val();
    }

    const result_f64: f64 = std.math.pow(f64, args[0].as_number(), args[1].as_number());

    return Value.number_val(result_f64);
}

pub fn str2num(vm: *VM, arg_count: usize, args: []Value) Value {
    _ = vm;

    if (arg_count != 1 or !args[0].is_string()) {
        std.debug.print("str2num() is expecting 1 string argument.\n", .{});
        return Value.nil_val();
    }

    const result = std.fmt.parseFloat(f64, args[0].as_cstring()) catch {
        std.debug.print("invalid string for number.\n", .{});
        return Value.nil_val();
    };

    return Value.number_val(result);
}

pub fn num2str(vm: *VM, arg_count: usize, args: []Value) Value {
    if (arg_count != 1 or !args[0].is_number()) {
        std.debug.print("num2str() is expecting 1 number argument.\n", .{});
        return Value.nil_val();
    }

    const str = std.fmt.allocPrint(vm.allocator, "{d}", .{args[0].as_number()}) catch {
        std.debug.print("unable to convert number to string.\n", .{});
        return Value.nil_val();
    };

    const result = Obj.String.new(vm, str);

    return Value.obj_val(&result.obj);
}
