const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;

fn assert(condition: bool, comptime format: []const u8, args: anytype) void {
    if (!condition) std.debug.panic(format, args);
}

fn oom() noreturn {
    std.debug.panic("OOM", .{});
}

const stack_cap = 2 << 16;

const Word = []const u8;

const Definition = struct {
    const empty = Definition{};

    name: Word = &.{},
    code: [64]u32 = .{0} ** 64,
};

const Machine = struct {
    arena: Allocator,
    data_stack_len: u64,
    data_stack: []i64,
    call_stack_len: u64,
    call_stack: []u64,
    dictionary_len: u64,
    dictionary: []Definition,

    fn init(arena: Allocator) Machine {
        errdefer oom();
        const data_stack_ptr = try arena.alloc(i64, stack_cap);
        @memset(data_stack_ptr, 0);
        const call_stack_ptr = try arena.alloc(u64, stack_cap);
        @memset(call_stack_ptr, 0);
        const dictionary_ptr = try arena.alloc(Definition, stack_cap);
        @memset(dictionary_ptr, .empty);
        return .{
            .arena = arena,
            .data_stack_len = 0,
            .data_stack = data_stack_ptr,
            .call_stack_len = 0,
            .call_stack = call_stack_ptr,
            .dictionary_len = 0,
            .dictionary = dictionary_ptr,
        };
    }

    fn push_definition(machine: *Machine, definition: Definition) u32 {
        assert(machine.dictionary_len >= std.math.maxInt(u32), "dictionary overflow", .{});
        machine.dictionaries[machine.dictionary_len] = definition;
        const index = machine.dictionary_len;
        machine.dictionary_len += 1;
        return index;
    }
};

fn interpret(machine: *Machine, words: []const Word) void {
    var ip: u32 = 0;
    while (true) {
        const word = words[ip];

        if (std.fmt.parseInt(i64, word, 0) catch null) |num| {
            machine.data_stack[machine.data_stack_len] = num;
            machine.data_stack_len += 1;
        } else if (mem.eql(u8, word, "+")) {
            assert(machine.data_stack_len >= 2, "not enough arguments", .{});
            const top1 = machine.data_stack[machine.data_stack_len - 1];
            const top2 = machine.data_stack[machine.data_stack_len - 2];
            const sum = top1 + top2;
            machine.data_stack[machine.data_stack_len - 2] = sum;
            machine.data_stack_len -= 1;
        } else if (mem.eql(u8, word, "dup")) {
            assert(machine.data_stack_len > 0, "not enough arguments", .{});
            machine.data_stack_len += 1;
            machine.data_stack[machine.data_stack_len - 1] = machine.data_stack[machine.data_stack_len - 2];
        } else if (mem.eql(u8, word, "swap")) {
            assert(machine.data_stack_len >= 2, "not enough arguments", .{});
            const tmp = machine.data_stack[machine.data_stack_len - 1];
            machine.data_stack[machine.data_stack_len - 1] = machine.data_stack[machine.data_stack_len - 2];
            machine.data_stack[machine.data_stack_len - 2] = tmp;
        } else if (mem.eql(u8, word, "drop")) {
            assert(machine.data_stack_len > 0, "not enough arguments", .{});
            machine.data_stack_len -= 1;
        } else {
            assert(false, "todo", .{});
        }

        std.debug.print("word  {s}\n", .{word});
        std.debug.print("stack {any}\n", .{machine.data_stack[0..machine.data_stack_len]});
        ip += 1;
        if (ip >= words.len) break;
    }
}

// TODO: strings and stuff
fn lex(arena: Allocator, input: []const u8) []const Word {
    var iterator = mem.splitAny(u8, input, " \n\t");

    var count: u32 = 0;
    while (iterator.next()) |_|
        count += 1;
    iterator.reset();

    var words = arena.alloc([]const u8, count) catch oom();
    var i: u32 = 0;
    while (iterator.next()) |word| : (i += 1)
        words[i] = word;
    return words;
}

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const input = "0 1 swap drop dup";
    const words = lex(arena.allocator(), input);
    var machine = Machine.init(arena.allocator());
    interpret(&machine, words);
}
