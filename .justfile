build:
    zig build

run *ARGS:
    zig build run -- {{ARGS}}

run-fast *ARGS:
    zig build run -Doptimize=ReleaseFast -- {{ARGS}}

test:
    zig build test
