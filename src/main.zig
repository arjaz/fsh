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

const Word = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    identifier: []const u8,
};

const Definition = struct {
    const empty = Definition{};

    name: []const u8 = &.{},
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

        switch (word) {
            .int => |num| {
                machine.data_stack[machine.data_stack_len] = num;
                machine.data_stack_len += 1;
            },
            .float => |num| {
                machine.data_stack[machine.data_stack_len] = @intFromFloat(num);
                machine.data_stack_len += 1;
            },
            .string => |str| {
                std.debug.print("(string: \"{s}\")\n", .{str});
            },
            .identifier => |id| {
                if (mem.eql(u8, id, "+")) {
                    assert(machine.data_stack_len >= 2, "not enough arguments", .{});
                    const top1 = machine.data_stack[machine.data_stack_len - 1];
                    const top2 = machine.data_stack[machine.data_stack_len - 2];
                    const sum = top1 + top2;
                    machine.data_stack[machine.data_stack_len - 2] = sum;
                    machine.data_stack_len -= 1;
                } else if (mem.eql(u8, id, "dup")) {
                    assert(machine.data_stack_len > 0, "not enough arguments", .{});
                    machine.data_stack_len += 1;
                    machine.data_stack[machine.data_stack_len - 1] = machine.data_stack[machine.data_stack_len - 2];
                } else if (mem.eql(u8, id, "swap")) {
                    assert(machine.data_stack_len >= 2, "not enough arguments", .{});
                    const tmp = machine.data_stack[machine.data_stack_len - 1];
                    machine.data_stack[machine.data_stack_len - 1] = machine.data_stack[machine.data_stack_len - 2];
                    machine.data_stack[machine.data_stack_len - 2] = tmp;
                } else if (mem.eql(u8, id, "drop")) {
                    assert(machine.data_stack_len > 0, "not enough arguments", .{});
                    machine.data_stack_len -= 1;
                } else {
                    assert(false, "todo", .{});
                }
            },
        }

        std.debug.print("word  {any}\n", .{word});
        std.debug.print("stack {any}\n", .{machine.data_stack[0..machine.data_stack_len]});
        ip += 1;
        if (ip >= words.len) break;
    }
}

fn parse_escape_sequence(char: u8) ?u8 {
    return switch (char) {
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        '\\' => '\\',
        '"' => '"',
        '0' => 0,
        else => null,
    };
}

fn lex(arena: Allocator, input: []const u8) []const Word {
    var words = ArrayListUnmanaged(Word).empty;
    var index: usize = 0;

    while (index < input.len) {
        // Skip whitespace
        while (index < input.len and (input[index] == ' ' or input[index] == '\n' or input[index] == '\t')) {
            index += 1;
        }

        if (index >= input.len) break;

        if (input[index] == '"') {
            // Skip opening quote
            index += 1;
            var string_buf = ArrayListUnmanaged(u8).empty;

            while (index < input.len and input[index] != '"') {
                if (input[index] == '\\' and index + 1 < input.len) {
                    index += 1;
                    if (parse_escape_sequence(input[index])) |escaped| {
                        string_buf.append(arena, escaped) catch oom();
                    } else {
                        // Invalid escape sequence, just add the character
                        string_buf.append(arena, input[index]) catch oom();
                    }
                } else {
                    string_buf.append(arena, input[index]) catch oom();
                }
                index += 1;
            }

            if (index < input.len) {
                // Skip closing quote
                index += 1;
            } else {
                // TODO: proper errors mechanism
                assert(false, "unterminated string literal", .{});
            }

            words.append(arena, .{ .string = string_buf.items }) catch oom();
        } else {
            // Not a string, find the end of the word
            const word_start = index;
            while (index < input.len and input[index] != ' ' and input[index] != '\n' and input[index] != '\t' and input[index] != '"') {
                index += 1;
            }

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

    const input = "0 1 swap \"hello world\" drop \"test\\nwith\\tescapes\" dup";
    const words = lex(arena.allocator(), input);
    var machine = Machine.init(arena.allocator());
    interpret(&machine, words);
}
