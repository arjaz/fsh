const std = @import("std");
const process = std.process;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const ArrayList = std.ArrayList;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const StaticStringMap = std.StaticStringMap;

fn assert(condition: bool, comptime format: []const u8, args: anytype) void {
    if (!condition) std.debug.panic(format ++ "\n", args);
}

fn oom() noreturn {
    std.debug.panic("OOM\n", .{});
}

fn exit(comptime format: []const u8, args: anytype) noreturn {
    std.debug.print(format ++ "\n", args);
    process.exit(1);
}

const stack_cap = 2 << 16;

const Value = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    array: []Value,

    fn print(value: Value) void {
        switch (value) {
            .int => std.debug.print("{d}", .{value.int}),
            .float => std.debug.print("{d}", .{value.float}),
            .string => std.debug.print("{s}", .{value.string}),
            .array => {
                std.debug.print("[", .{});
                for (value.array, 0..) |v, i| {
                    v.print();
                    if (i + 1 != value.array.len)
                        std.debug.print(" ", .{});
                }
                std.debug.print("]", .{});
            },
        }
    }
};

const Word = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    identifier: []const u8,

    fn print(word: Word) void {
        switch (word) {
            .int => std.debug.print("{d} ", .{word.int}),
            .float => std.debug.print("{d} ", .{word.float}),
            .string => std.debug.print("{s} ", .{word.string}),
            .identifier => std.debug.print("'{s} ", .{word.identifier}),
        }
    }
};

const Definition = struct {
    const empty = Definition{};

    name: []const u8 = &.{},
    code_len: u8 = 0,
    code: [64]?Word = .{null} ** 64,
};

const Machine = struct {
    arena: Allocator,
    data_stack_len: u64,
    data_stack: []Value,
    call_stack_len: u64,
    call_stack: []u64,
    dictionary_len: u64,
    dictionary: []Definition,

    fn init(arena: Allocator) Machine {
        errdefer oom();
        const data_stack_ptr = try arena.alloc(Value, stack_cap);
        @memset(data_stack_ptr, Value{ .int = 0 });
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

    fn dictionary_lookup(machine: Machine, name: []const u8) ?Definition {
        for (machine.dictionary) |def|
            if (mem.eql(u8, name, def.name))
                return def;
        return null;
    }

    fn push_definition(machine: *Machine, definition: Definition) u32 {
        assert(machine.dictionary_len >= std.math.maxInt(u32), "dictionary overflow", .{});
        machine.dictionaries[machine.dictionary_len] = definition;
        const index = machine.dictionary_len;
        machine.dictionary_len += 1;
        return index;
    }
};

// TODO: memory management, some other time
fn interpret(arena: Allocator, machine: *Machine, words: []const Word) void {
    var word_index: u32 = 0;
    while (true) {
        const word = words[word_index];

        switch (word) {
            .int => |num| {
                machine.data_stack[machine.data_stack_len] = Value{ .int = num };
                machine.data_stack_len += 1;
            },

            .float => |num| {
                machine.data_stack[machine.data_stack_len] = Value{ .float = num };
                machine.data_stack_len += 1;
            },

            .string => |str| {
                machine.data_stack[machine.data_stack_len] = Value{ .string = str };
                machine.data_stack_len += 1;
            },

            .identifier => |id| {
                // First check for builtins
                if (mem.eql(u8, id, "+")) {
                    const top1 = machine.data_stack[machine.data_stack_len - 1];
                    const top2 = machine.data_stack[machine.data_stack_len - 2];
                    const sum = top1.int + top2.int;
                    machine.data_stack[machine.data_stack_len - 2] = Value{ .int = sum };
                    machine.data_stack_len -= 1;
                } else if (mem.eql(u8, id, "dup")) {
                    machine.data_stack_len += 1;
                    machine.data_stack[machine.data_stack_len - 1] = machine.data_stack[machine.data_stack_len - 2];
                } else if (mem.eql(u8, id, "swap")) {
                    const tmp = machine.data_stack[machine.data_stack_len - 1];
                    machine.data_stack[machine.data_stack_len - 1] = machine.data_stack[machine.data_stack_len - 2];
                    machine.data_stack[machine.data_stack_len - 2] = tmp;
                } else if (mem.eql(u8, id, "drop")) {
                    machine.data_stack_len -= 1;
                } else if (mem.eql(u8, id, ".")) {
                    machine.data_stack[machine.data_stack_len - 1].print();
                    std.debug.print(" ", .{});
                    machine.data_stack_len -= 1;
                } else if (mem.eql(u8, id, "ls")) {
                    var array = ArrayList(Value).init(arena);
                    const dir = std.fs.cwd().openDir(".", .{ .iterate = true }) catch unreachable;
                    var iterator = dir.iterateAssumeFirstIteration();
                    while (iterator.next() catch null) |entry|
                        array.append(.{ .string = entry.name }) catch oom();
                    machine.data_stack_len += 1;
                    machine.data_stack[machine.data_stack_len - 1] = .{ .array = array.items };
                }
                // if not a builtin try to lookup in dictionary
                else if (machine.dictionary_lookup(id)) |def| {
                    assert(false, "todo", .{});
                    _ = def;
                }
                // if not in dictionary, try to execute as a command
                else {
                    process.execv(arena, &.{id}) catch {};
                }
            },
        }

        word_index += 1;
        if (word_index >= words.len) break;
    }
}

fn lex(arena: Allocator, input: []const u8) []const Word {
    var words = ArrayListUnmanaged(Word).empty;
    var index: usize = 0;

    while (index < input.len) {
        // Skip whitespace
        while (index < input.len and
            (input[index] == ' ' or input[index] == '\n' or input[index] == '\t'))
            index += 1;

        if (index >= input.len) break;

        if (input[index] == '"') {
            const start = index;
            // Skip opening quote
            index += 1;

            while (index < input.len and input[index] != '"') : (index += 1) {
                if (input[index] == '\\' and index + 1 < input.len)
                    index += 1;
            }

            const end = index + 1;
            if (index < input.len) {
                // Skip closing quote
                index += 1;
            } else {
                assert(false, "unterminated string literal", .{});
            }

            words.append(arena, .{ .string = input[start..end] }) catch oom();
        } else {
            // Not a string, find the end of the word
            const word_start = index;
            while (index < input.len and
                input[index] != ' ' and input[index] != '\n' and
                input[index] != '\t' and input[index] != '"')
                index += 1;

            const word = input[word_start..index];

            // Try to parse as int, float, or identifier
            if (std.fmt.parseInt(i64, word, 0) catch null) |num| {
                words.append(arena, .{ .int = num }) catch oom();
            } else if (std.fmt.parseFloat(f64, word) catch null) |num| {
                words.append(arena, .{ .float = num }) catch oom();
            } else {
                // Everything else is an identifier (including operators)
                words.append(arena, .{ .identifier = word }) catch oom();
            }
        }
    }

    return words.toOwnedSlice(arena) catch oom();
}

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var args_iterator = process.argsWithAllocator(gpa.allocator()) catch oom();
    // The first one is the binary name
    _ = args_iterator.next();
    const filename = args_iterator.next() orelse exit("provide the input file", .{});
    const input = std.fs.cwd().readFileAlloc(gpa.allocator(), filename, 4194304) catch |err| switch (err) {
        error.OutOfMemory => oom(),
        else => exit("Something is wrong with your file", .{}),
    };
    defer gpa.allocator().free(input);
    args_iterator.deinit();

    const words = lex(arena.allocator(), input);
    var machine = Machine.init(arena.allocator());
    interpret(arena.allocator(), &machine, words);
}

test "lex integers" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic integers
    {
        const words = lex(allocator, "0 42 -17 +99");
        try std.testing.expectEqual(@as(usize, 4), words.len);
        try std.testing.expectEqual(@as(i64, 0), words[0].int);
        try std.testing.expectEqual(@as(i64, 42), words[1].int);
        try std.testing.expectEqual(@as(i64, -17), words[2].int);
        try std.testing.expectEqual(@as(i64, 99), words[3].int);
    }

    // Different bases
    {
        const words = lex(allocator, "0x1F 0xFF 0b1010 0o77");
        try std.testing.expectEqual(@as(usize, 4), words.len);
        try std.testing.expectEqual(@as(i64, 31), words[0].int);
        try std.testing.expectEqual(@as(i64, 255), words[1].int);
        try std.testing.expectEqual(@as(i64, 10), words[2].int);
        try std.testing.expectEqual(@as(i64, 63), words[3].int);
    }

    // Large numbers
    {
        const words = lex(allocator, "9223372036854775807 -9223372036854775808");
        try std.testing.expectEqual(@as(usize, 2), words.len);
        try std.testing.expectEqual(@as(i64, std.math.maxInt(i64)), words[0].int);
        try std.testing.expectEqual(@as(i64, std.math.minInt(i64)), words[1].int);
    }
}

test "lex floats" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic floats
    {
        const words = lex(allocator, "3.14 -2.5 0.0 +1.23");
        try std.testing.expectEqual(@as(usize, 4), words.len);
        try std.testing.expectApproxEqAbs(@as(f64, 3.14), words[0].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, -2.5), words[1].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 0.0), words[2].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 1.23), words[3].float, 0.001);
    }

    // Scientific notation
    {
        const words = lex(allocator, "1e10 1.5e-5 -3.14e+2");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectApproxEqAbs(@as(f64, 1e10), words[0].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 1.5e-5), words[1].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, -3.14e+2), words[2].float, 0.001);
    }

    // Edge cases
    {
        const words = lex(allocator, ".5 0. 1.");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectApproxEqAbs(@as(f64, 0.5), words[0].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 0.0), words[1].float, 0.001);
        try std.testing.expectApproxEqAbs(@as(f64, 1.0), words[2].float, 0.001);
    }
}

test "lex strings" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic strings
    {
        const words = lex(allocator, "\"hello\" \"world\" \"\"");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectEqualStrings("\"hello\"", words[0].string);
        try std.testing.expectEqualStrings("\"world\"", words[1].string);
        try std.testing.expectEqualStrings("\"\"", words[2].string);
    }

    // Escape sequences
    {
        const words = lex(allocator, "\"\\n\\t\\r\" \"\\\"quoted\\\"\" \"back\\\\slash\" \"null\\0char\"");
        try std.testing.expectEqual(@as(usize, 4), words.len);
        try std.testing.expectEqualStrings("\"\\n\\t\\r\"", words[0].string);
        try std.testing.expectEqualStrings("\"\\\"quoted\\\"\"", words[1].string);
        try std.testing.expectEqualStrings("\"back\\\\slash\"", words[2].string);
        try std.testing.expectEqualStrings("\"null\\0char\"", words[3].string);
    }

    // Strings with spaces
    {
        const words = lex(allocator, "\"hello world\" \"multiple   spaces\" \"tabs\\there\"");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectEqualStrings("\"hello world\"", words[0].string);
        try std.testing.expectEqualStrings("\"multiple   spaces\"", words[1].string);
        try std.testing.expectEqualStrings("\"tabs\\there\"", words[2].string);
    }

    // Invalid escape sequences
    {
        const words = lex(allocator, "\"\\x\" \"\\q\" \"\\1\"");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectEqualStrings("\"\\x\"", words[0].string);
        try std.testing.expectEqualStrings("\"\\q\"", words[1].string);
        try std.testing.expectEqualStrings("\"\\1\"", words[2].string);
    }
}

test "lex identifiers" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic identifiers and operators
    {
        const words = lex(allocator, "+ - * / dup drop swap over");
        try std.testing.expectEqual(@as(usize, 8), words.len);
        try std.testing.expectEqualStrings("+", words[0].identifier);
        try std.testing.expectEqualStrings("-", words[1].identifier);
        try std.testing.expectEqualStrings("*", words[2].identifier);
        try std.testing.expectEqualStrings("/", words[3].identifier);
        try std.testing.expectEqualStrings("dup", words[4].identifier);
        try std.testing.expectEqualStrings("drop", words[5].identifier);
        try std.testing.expectEqualStrings("swap", words[6].identifier);
        try std.testing.expectEqualStrings("over", words[7].identifier);
    }

    // Complex identifiers
    {
        const words = lex(allocator, "foo123 _bar baz_ under_score CamelCase");
        try std.testing.expectEqual(@as(usize, 5), words.len);
        try std.testing.expectEqualStrings("foo123", words[0].identifier);
        try std.testing.expectEqualStrings("_bar", words[1].identifier);
        try std.testing.expectEqualStrings("baz_", words[2].identifier);
        try std.testing.expectEqualStrings("under_score", words[3].identifier);
        try std.testing.expectEqualStrings("CamelCase", words[4].identifier);
    }

    // Funny identifiers
    {
        const words = lex(allocator, "1+ 2/ works?");
        try std.testing.expectEqual(@as(usize, 3), words.len);
        try std.testing.expectEqualStrings("1+", words[0].identifier);
        try std.testing.expectEqualStrings("2/", words[1].identifier);
        try std.testing.expectEqualStrings("works?", words[2].identifier);
    }

    // Special characters
    {
        const words = lex(allocator, "! @ # $ % ^ & = < > ? :");
        try std.testing.expectEqual(@as(usize, 12), words.len);
        for (words, 0..) |word, i| {
            _ = i;
            try std.testing.expect(word == .identifier);
        }
    }
}

test "lex mixed input" {
    var arena = ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Mixed types
    {
        const words = lex(allocator, "42 3.14 \"hello\" + -17 \"world\" swap");
        try std.testing.expectEqual(@as(usize, 7), words.len);
        try std.testing.expectEqual(@as(i64, 42), words[0].int);
        try std.testing.expectApproxEqAbs(@as(f64, 3.14), words[1].float, 0.001);
        try std.testing.expectEqualStrings("\"hello\"", words[2].string);
        try std.testing.expectEqualStrings("+", words[3].identifier);
        try std.testing.expectEqual(@as(i64, -17), words[4].int);
        try std.testing.expectEqualStrings("\"world\"", words[5].string);
        try std.testing.expectEqualStrings("swap", words[6].identifier);
    }

    // Different whitespace
    {
        const words = lex(allocator, "1\n2\t3  4\n\t  5");
        try std.testing.expectEqual(@as(usize, 5), words.len);
        for (words, 1..) |word, i| {
            try std.testing.expectEqual(@as(i64, @intCast(i)), word.int);
        }
    }

    // Adjacent strings and numbers
    {
        const words = lex(allocator, "\"no\"\"space\"42\"between\"3.14");
        try std.testing.expectEqual(@as(usize, 5), words.len);
        try std.testing.expectEqualStrings("\"no\"", words[0].string);
        try std.testing.expectEqualStrings("\"space\"", words[1].string);
        try std.testing.expectEqual(@as(i64, 42), words[2].int);
        try std.testing.expectEqualStrings("\"between\"", words[3].string);
        try std.testing.expectApproxEqAbs(@as(f64, 3.14), words[4].float, 0.001);
    }

    // Empty input
    {
        const words = lex(allocator, "");
        try std.testing.expectEqual(@as(usize, 0), words.len);
    }

    // Only whitespace
    {
        const words = lex(allocator, "   \n\t  \n  ");
        try std.testing.expectEqual(@as(usize, 0), words.len);
    }
}
