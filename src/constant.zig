const std = @import("std");

pub const TABLE_MAX_LOAD = 0.75;

pub const UINT8_MAX = std.math.maxInt(u8);
pub const UINT16_MAX = std.math.maxInt(u16);

pub const UINT8_COUNT = UINT8_MAX + 1;

pub const FRAMES_MAX = 64;
pub const STACK_MAX = (FRAMES_MAX * UINT8_MAX);

pub const DEBUG_PRINT_CODE = false;
pub const DEBUG_TRACE_EXECUTION = false;
pub const DEBUG_PRINT_INTERNAL_STRINGS = false;
pub const DEBUG_PRINT_GLOBALS = false;

pub const DEBUG_STRESS_GC = true;
pub const DEBUG_LOG_GC = false;

pub const USE_CUSTON_ALLOCATOR = true;

pub const GC_HEAP_GROW_FACTOR = 2;
