pub fn main() !void {
    var gpa = DebugAllocator(.{}).init;
    defer assert(gpa.deinit() == .ok, "Memory leaked", .{});
    var gpa_allocator = gpa.allocator();

    var args_iterator = process.argsWithAllocator(gpa_allocator) catch oom();
    _ = args_iterator.next();
    const filename = args_iterator.next() orelse exit("provide the input file", .{});
    const input = fs.cwd().readFileAllocOptions(
        gpa_allocator,
        filename,
        4194304,
        null,
        .of(u8),
        0,
    ) catch |err| switch (err) {
        error.OutOfMemory => oom(),
        else => exit("Something is wrong with your file", .{}),
    };
    args_iterator.deinit();

    var lexer_arena = ArenaAllocator.init(gpa_allocator);
    const lexer_arena_allocator = lexer_arena.allocator();
    defer lexer_arena.deinit();
    const words = try lex(lexer_arena_allocator, input);
    gpa_allocator.free(input);

    var machine_arena = ArenaAllocator.init(gpa_allocator);
    const machine_arena_allocator = machine_arena.allocator();
    defer machine_arena.deinit();
    var machine = Machine.init(machine_arena_allocator);
    machine.program = words;

    try interpret(&machine);
}

const STACK_CAP = 2 << 16;

// TODO: Refcount

const Value = struct {
    payload: Inner,
    // refcount: u64 = 0,

    const Inner = union(enum) {
        int: i64,
        float: f64,
        char: u8,
        string: []const u8,
        identifier: []const u8,
        array: []Value,
    };

    fn fromInt(i: i64) Value {
        return .{ .payload = .{ .int = i } };
    }

    fn fromFloat(f: f64) Value {
        return .{ .payload = .{ .float = f } };
    }

    fn fromChar(c: u8) Value {
        return .{ .payload = .{ .char = c } };
    }

    fn fromString(str: []const u8) Value {
        return .{ .payload = .{ .string = str } };
    }

    fn fromIdentifier(identifier: []const u8) Value {
        return .{ .payload = .{ .identifier = identifier } };
    }

    fn fromArray(values: []Value) Value {
        return .{ .payload = .{ .array = values } };
    }

    fn fromBool(b: bool) Value {
        // true == -1 because all 1s
        return if (b) fromInt(-1) else fromInt(0);
    }

    fn sameTag(this: Value, other: Value) bool {
        return meta.activeTag(this.payload) == meta.activeTag(other.payload);
    }

    fn print(value: Value) void {
        switch (value.payload) {
            .int => printStderr("{d}", .{value.payload.int}),
            .float => printStderr("{d}", .{value.payload.float}),
            .char => printStderr("{c}", .{value.payload.char}),
            .identifier => printStderr("'{s}", .{value.payload.identifier}),
            .string => printStderr("{s}", .{value.payload.string}),
            .array => {
                printStderr("[", .{});
                for (value.payload.array, 0..) |v, i| {
                    v.print();
                    if (i + 1 != value.payload.array.len)
                        printStderr(" ", .{});
                }
                printStderr("]", .{});
            },
        }
    }
};

// Exactly the same name is also used for parsing
const Builtin = enum(u8) {
    // booleans
    @"=",
    @"!=",
    @"<",
    @"<=",
    @">",
    @">=",
    // arithmetic
    @"+",
    @"*",
    xor,
    @"or",
    @"and",
    @"-",
    @"<<",
    @">>",
    @"/",
    @"%",
    // stack manipulation
    swap,
    dup,
    over,
    rot,
    @"-rot",
    drop,
    nip,
    tuck,
    // printing
    @".",
    @".b",
    @".o",
    @".x",
    @".s",
    // dictionary manipulation
    // load,
    @":",
    @";",
    // OS interactions
    ls,
    sh,
    // last stdout
    // stdout,
    // last stderr
    // stderr,
};

const Word = union(enum) {
    int: i64,
    float: f64,
    char: u8,
    string: []const u8,
    identifier: []const u8,
    quoted: []const u8,
    builtin: Builtin,

    fn print(word: Word) void {
        switch (word) {
            .int => printStderr("{d} ", .{word.int}),
            .float => printStderr("{d} ", .{word.float}),
            .char => printStderr("{}", .{word.char}),
            .string => printStderr("{s} ", .{word.string}),
            .identifier => printStderr("{s} ", .{word.identifier}),
            .quoted => printStderr("'{s} ", .{word.quoted}),
            .builtin => printStderr("{s}", .{@tagName(word.builtin)}),
        }
    }
};

const Definition = struct {
    name: []const u8 = &.{},
    wp: u32 = 0,
    const empty = Definition{};
};

const Machine = struct {
    arena: Allocator,
    data_stack_len: u32 = 0,
    data_stack: []Value,
    call_stack_len: u32 = 0,
    call_stack: []u32,
    dictionary_len: u32 = 0,
    dictionary: []Definition,
    wp: u32 = 0,
    program: []const Word = &.{},

    fn init(arena: Allocator) Machine {
        errdefer oom();
        const data_stack = try arena.alloc(Value, STACK_CAP);
        @memset(data_stack, .fromInt(0));
        const call_stack = try arena.alloc(u32, STACK_CAP);
        @memset(call_stack, 0);
        const dictionary = try arena.alloc(Definition, STACK_CAP);
        @memset(dictionary, .empty);
        return .{
            .arena = arena,
            .data_stack = data_stack,
            .call_stack = call_stack,
            .dictionary = dictionary,
        };
    }

    fn push(machine: *Machine, value: Value) void {
        assert(machine.data_stack_len < math.maxInt(u32), "data stack overflow", .{});
        machine.data_stack_len += 1;
        machine.data_stack[machine.data_stack_len - 1] = value;
    }

    fn pop(machine: *Machine) Value {
        const top1 = machine.top();
        machine.data_stack_len -= 1;
        return top1;
    }

    fn top(machine: Machine) Value {
        return machine.data_stack[machine.data_stack_len - 1];
    }

    fn top2(machine: Machine) Value {
        return machine.data_stack[machine.data_stack_len - 2];
    }

    fn dictionaryLookup(machine: Machine, name: []const u8) ?Definition {
        for (machine.dictionary[0..machine.dictionary_len]) |def|
            if (mem.eql(u8, name, def.name))
                return def;
        return null;
    }
};

const ErrorType = enum { type_error, todo, undefined, syntax_error };
fn reportError(e: ErrorType) !void {
    switch (e) {
        .type_error => return error.TypeError,
        .todo => return error.Todo,
        .undefined => return error.Undefined,
        .syntax_error => return error.SyntaxError,
    }
}

fn interpertBuiltin(machine: *Machine, builtin: Builtin) !void {
    switch (builtin) {
        .@"=" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            machine.push(.fromBool(meta.eql(arg1, arg2)));
        },

        .@"!=" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            machine.push(.fromBool(!meta.eql(arg1, arg2)));
        },

        .@"<" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromBool(arg1.payload.int < arg2.payload.int)),
                .float => machine.push(.fromBool(arg1.payload.float < arg2.payload.float)),
                else => try reportError(.type_error),
            }
        },

        .@"<=" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromBool(arg1.payload.int <= arg2.payload.int)),
                .float => machine.push(.fromBool(arg1.payload.float <= arg2.payload.float)),
                else => try reportError(.type_error),
            }
        },

        .@">" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromBool(arg1.payload.int > arg2.payload.int)),
                .float => machine.push(.fromBool(arg1.payload.float > arg2.payload.float)),
                else => try reportError(.type_error),
            }
        },

        .@">=" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromBool(arg1.payload.int >= arg2.payload.int)),
                .float => machine.push(.fromBool(arg1.payload.float >= arg2.payload.float)),
                else => try reportError(.type_error),
            }
        },

        .@"+" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int + arg2.payload.int)),
                .float => machine.push(.fromFloat(arg1.payload.float + arg2.payload.float)),
                else => try reportError(.type_error),
            }
        },

        .@"*" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int * arg2.payload.int)),
                .float => machine.push(.fromFloat(arg1.payload.float * arg2.payload.float)),
                else => try reportError(.type_error),
            }
        },

        .xor => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int ^ arg2.payload.int)),
                else => try reportError(.type_error),
            }
        },

        .@"or" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int | arg2.payload.int)),
                else => try reportError(.type_error),
            }
        },

        .@"and" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int & arg2.payload.int)),
                else => try reportError(.type_error),
            }
        },

        .@"-" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int - arg2.payload.int)),
                .float => machine.push(.fromFloat(arg1.payload.float - arg2.payload.float)),
                else => try reportError(.type_error),
            }
        },

        .@"<<" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int << @intCast(arg2.payload.int))),
                else => try reportError(.type_error),
            }
        },

        .@">>" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(arg1.payload.int >> @intCast(arg2.payload.int))),
                else => try reportError(.type_error),
            }
        },

        .@"/" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(@divFloor(arg1.payload.int, arg2.payload.int))),
                .float => machine.push(.fromFloat(arg1.payload.float / arg2.payload.float)),
                else => try reportError(.type_error),
            }
        },

        .@"%" => {
            const arg2 = machine.pop();
            const arg1 = machine.pop();
            if (!arg1.sameTag(arg2)) try reportError(.type_error);
            switch (arg1.payload) {
                .int => machine.push(.fromInt(@mod(arg1.payload.int, arg2.payload.int))),
                .float => machine.push(.fromFloat(@mod(arg1.payload.float, arg2.payload.float))),
                else => try reportError(.type_error),
            }
        },

        .swap => {
            const top = machine.pop();
            const top2 = machine.pop();
            machine.push(top);
            machine.push(top2);
        },

        .dup => {
            machine.push(machine.top());
        },

        .over => {
            machine.push(machine.top2());
        },

        .rot => {
            const top = machine.pop();
            const top2 = machine.pop();
            const top3 = machine.pop();
            machine.push(top2);
            machine.push(top);
            machine.push(top3);
        },

        .@"-rot" => {
            const top = machine.pop();
            const top2 = machine.pop();
            const top3 = machine.pop();
            machine.push(top);
            machine.push(top3);
            machine.push(top2);
        },

        .drop => {
            _ = machine.pop();
        },

        .nip => {
            const top = machine.pop();
            _ = machine.pop();
            machine.push(top);
        },

        .tuck => {
            const top = machine.pop();
            const top2 = machine.pop();
            machine.push(top);
            machine.push(top2);
            machine.push(top);
        },

        .@"." => {
            const top = machine.pop();
            top.print();
        },

        .@".b" => {
            const top = machine.pop();
            switch (top.payload) {
                .int => printStderr("0b{b} ", .{top.payload.int}),
                else => try reportError(.type_error),
            }
        },

        .@".o" => {
            const top = machine.pop();
            switch (top.payload) {
                .int => printStderr("0o{o} ", .{top.payload.int}),
                else => try reportError(.type_error),
            }
        },

        .@".x" => {
            const top = machine.pop();
            switch (top.payload) {
                .int => printStderr("0x{x} ", .{top.payload.int}),
                else => try reportError(.type_error),
            }
        },

        .@".s" => {
            for (machine.data_stack[0..machine.data_stack_len], 0..) |v, i| {
                printStderr("STACK[{}]: ", .{i});
                v.print();
                printStderr("\n", .{});
            }
        },

        .@":" => {
            var current_definition: Definition = .empty;
            machine.wp += 1;
            if (machine.program[machine.wp] != .identifier)
                try reportError(.syntax_error);
            current_definition.name = machine.program[machine.wp].identifier;
            machine.wp += 1;
            current_definition.wp = machine.wp;
            while (!(machine.program[machine.wp] == .builtin and
                machine.program[machine.wp].builtin == .@";"))
                machine.wp += 1;
            assert(machine.dictionary_len < math.maxInt(u32), "dictionary overflow", .{});
            machine.dictionary[machine.dictionary_len] = current_definition;
            machine.dictionary_len += 1;
        },

        .@";" => {
            machine.wp = machine.call_stack[machine.call_stack_len - 1];
            machine.call_stack_len -= 1;
        },

        .ls => {
            // TODO: Here we should free once we pop or something
            var array = ArrayListUnmanaged(Value).empty;
            var dir = fs.cwd().openDir(".", .{ .iterate = true }) catch unreachable;
            defer dir.close();
            var iterator = dir.iterateAssumeFirstIteration();
            while (iterator.next() catch null) |entry| {
                const owned = machine.arena.alloc(u8, entry.name.len) catch oom();
                @memcpy(owned, entry.name);
                array.append(machine.arena, .fromString(owned)) catch oom();
            }
            machine.data_stack_len += 1;
            machine.data_stack[machine.data_stack_len - 1] = .fromArray(array.items);
        },

        .sh => {
            // TODO: capture stdout/stderr?
            const top = machine.pop();
            switch (top.payload) {
                .identifier => {
                    const pid = posix.fork() catch exit("Fork failed", .{});
                    if (pid == 0) {
                        // child process
                        process.execv(machine.arena, &.{top.payload.identifier}) catch exit("Execv failed", .{});
                    } else if (pid > 0) {
                        // parent process
                        // TODO: do something with the return code?
                        _ = posix.waitpid(pid, 0);
                    } else {
                        exit("Fork failed", .{});
                    }
                },
                .array => try reportError(.todo),
                else => try reportError(.type_error),
            }
        },
    }
}

// TODO: memory management, some other time
fn interpret(machine: *Machine) !void {
    while (machine.wp < machine.program.len) : (machine.wp += 1) {
        switch (machine.program[machine.wp]) {
            .int => |num| machine.push(.fromInt(num)),
            .float => |num| machine.push(.fromFloat(num)),
            .char => |char| machine.push(.fromChar(char)),
            .string => |str| machine.push(.fromString(str)),
            .quoted => |quoted| machine.push(.fromIdentifier(quoted)),
            .builtin => |builtin| try interpertBuiltin(machine, builtin),
            .identifier => |id| {
                if (machine.dictionaryLookup(id)) |def| {
                    assert(machine.call_stack_len < STACK_CAP, "call stack overflow", .{});
                    machine.call_stack[machine.call_stack_len] = machine.wp;
                    machine.call_stack_len += 1;
                    machine.wp = def.wp - 1;
                } else try reportError(.undefined);
            },
        }
    }
}

fn is_whitespace(char: u8) bool {
    return switch (char) {
        ' ', '\n', '\r', '\t' => true,
        else => false,
    };
}

fn parse_escape_sequence(char: u8) u8 {
    return switch (char) {
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        '\\' => '\\',
        '"' => '"',
        '0' => 0,
        else => char,
    };
}

fn lex(arena: Allocator, input: [:0]const u8) ![]const Word {
    // to get the number of words for allocation
    // we go through the input twice
    // the first time to count the words
    // the second time to lex them

    var program_size: u32 = 0;
    var index: u32 = 0;
    while (input[index] != 0) {
        while (is_whitespace(input[index]))
            index += 1;
        // character
        if (input[index] == '\\') {
            program_size += 1;
            index += 1;
            if (input[index] == '\\')
                index += 1;
            index += 1;
        }
        // string
        else if (input[index] == '"') {
            program_size += 1;
            index += 1;
            while (input[index] != 0 and input[index] != '"') : (index += 1) {
                if (input[index] == '\\')
                    index += 1;
            }
            if (input[index] == 0) {
                try reportError(.syntax_error);
            } else index += 1;
        }
        // quote
        else if (input[index] == '\'') {
            program_size += 1;
            index += 1;
            while (input[index] != 0 and !is_whitespace(input[index]))
                index += 1;
        }
        // identifier
        else if (input[index] != 0) {
            program_size += 1;
            while (input[index] != 0 and
                !is_whitespace(input[index]) and
                input[index] != '"')
                index += 1;
        }
    }

    var words = arena.alloc(Word, program_size) catch oom();
    var word_index: u32 = 0;
    index = 0;
    while (input[index] != 0) {
        while (is_whitespace(input[index]))
            index += 1;
        // character
        if (input[index] == '\\') {
            index += 1;
            if (input[index] == '\\') {
                index += 1;
                const escaped = parse_escape_sequence(input[index]);
                words[word_index] = .{ .char = escaped };
            } else {
                words[word_index] = .{ .char = input[index] };
            }
            word_index += 1;
            index += 1;
        }
        // string
        else if (input[index] == '"') {
            index += 1;
            // Get the string size to allocate
            var string_size: u32 = 0;
            const index_start = index;
            while (input[index] != 0 and input[index] != '"') : (index += 1) {
                if (input[index] == '\\')
                    index += 1;
                string_size += 1;
            }
            if (input[index] == 0)
                try reportError(.syntax_error);
            // Reset the index to actually construct the string
            index = index_start;
            var string = arena.alloc(u8, string_size) catch oom();
            var string_index: u32 = 0;
            while (input[index] != 0 and input[index] != '"') : (index += 1) {
                if (input[index] == '\\') {
                    index += 1;
                    string[string_index] = parse_escape_sequence(input[index]);
                } else {
                    string[string_index] = input[index];
                }
                string_index += 1;
            }
            // Skip closing quote
            if (input[index] != 0)
                index += 1
            else
                try reportError(.syntax_error);
            words[word_index] = .{ .string = string };
            word_index += 1;
        }
        // quote
        else if (input[index] == '\'') {
            index += 1;
            const word_start = index;
            while (input[index] != 0 and
                !is_whitespace(input[index]))
                index += 1;
            const word = input[word_start..index];
            words[word_index] = .{ .quoted = word };
            word_index += 1;
        }
        // identifier: word/number
        else if (input[index] != 0) {
            // Not a string, find the end of the word
            const word_start = index;
            while (input[index] != 0 and
                !is_whitespace(input[index]) and
                input[index] != '"')
                index += 1;
            const word = input[word_start..index];
            // Try to parse as int, float, builtin, or identifier
            if (fmt.parseInt(i64, word, 0) catch null) |num| {
                words[word_index] = .{ .int = num };
                word_index += 1;
            } else if (fmt.parseFloat(f64, word) catch null) |num| {
                words[word_index] = .{ .float = num };
                word_index += 1;
            } else parsed: {
                // a builtin
                inline for (meta.fields(Builtin)) |enumField| {
                    if (mem.eql(u8, word, enumField.name)) {
                        words[word_index] = .{
                            .builtin = @field(Builtin, enumField.name),
                        };
                        word_index += 1;
                        break :parsed;
                    }
                }
                // an identifier
                const word_owned = arena.alloc(u8, word.len) catch oom();
                @memcpy(word_owned, word);
                words[word_index] = .{ .identifier = word_owned };
                word_index += 1;
            }
        }
    }

    return words;
}

fn assert(condition: bool, comptime format: []const u8, args: anytype) void {
    if (!condition) debug.panic(format ++ "\n", args);
}

fn oom() noreturn {
    debug.panic("OOM\n", .{});
}

fn exit(comptime format: []const u8, args: anytype) noreturn {
    @branchHint(.cold);
    debug.print(format ++ "\n", args);
    process.exit(1);
}

fn printStderr(comptime format: []const u8, args: anytype) void {
    var buffer: [64]u8 = undefined;
    const bw = debug.lockStderrWriter(&buffer);
    defer debug.unlockStderrWriter();
    nosuspend bw.print(format, args) catch return;
}

test "lex integers" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic integers
    {
        const words = try lex(allocator, "0 42 -17 +99");
        try testing.expectEqual(@as(usize, 4), words.len);
        try testing.expectEqual(@as(i64, 0), words[0].int);
        try testing.expectEqual(@as(i64, 42), words[1].int);
        try testing.expectEqual(@as(i64, -17), words[2].int);
        try testing.expectEqual(@as(i64, 99), words[3].int);
    }

    // Different bases
    {
        const words = try lex(allocator, "0x1F 0xFF 0b1010 0o77");
        try testing.expectEqual(@as(usize, 4), words.len);
        try testing.expectEqual(@as(i64, 31), words[0].int);
        try testing.expectEqual(@as(i64, 255), words[1].int);
        try testing.expectEqual(@as(i64, 10), words[2].int);
        try testing.expectEqual(@as(i64, 63), words[3].int);
    }

    // Large numbers
    {
        const words = try lex(allocator, "9223372036854775807 -9223372036854775808");
        try testing.expectEqual(@as(usize, 2), words.len);
        try testing.expectEqual(@as(i64, math.maxInt(i64)), words[0].int);
        try testing.expectEqual(@as(i64, math.minInt(i64)), words[1].int);
    }
}

test "lex floats" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic floats
    {
        const words = try lex(allocator, "3.14 -2.5 0.0 +1.23");
        try testing.expectEqual(@as(usize, 4), words.len);
        try testing.expectApproxEqAbs(@as(f64, 3.14), words[0].float, 0.001);
        try testing.expectApproxEqAbs(@as(f64, -2.5), words[1].float, 0.001);
        try testing.expectApproxEqAbs(@as(f64, 0.0), words[2].float, 0.001);
        try testing.expectApproxEqAbs(@as(f64, 1.23), words[3].float, 0.001);
    }

    // Scientific notation
    {
        const words = try lex(allocator, "1e10 1.5e-5 -3.14e+2");
        try testing.expectEqual(@as(usize, 3), words.len);
        try testing.expectApproxEqAbs(@as(f64, 1e10), words[0].float, 0.001);
        try testing.expectApproxEqAbs(@as(f64, 1.5e-5), words[1].float, 0.001);
        try testing.expectApproxEqAbs(@as(f64, -3.14e+2), words[2].float, 0.001);
    }

    // Edge cases
    {
        const words = try lex(allocator, ".5 0. 1.");
        try testing.expectEqual(@as(usize, 3), words.len);
        try testing.expectApproxEqAbs(@as(f64, 0.5), words[0].float, 0.001);
        try testing.expectApproxEqAbs(@as(f64, 0.0), words[1].float, 0.001);
        try testing.expectApproxEqAbs(@as(f64, 1.0), words[2].float, 0.001);
    }
}

test "lex chars" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic chars
    {
        const words = try lex(allocator, "\\a \\b \\c ");
        try testing.expectEqual(@as(usize, 3), words.len);
        try testing.expectEqual('a', words[0].char);
        try testing.expectEqual('b', words[1].char);
        try testing.expectEqual('c', words[2].char);
    }

    // Escaped chars
    {
        const words = try lex(allocator, "\\\\n \\\\t \\\\0");
        try testing.expectEqual(@as(usize, 3), words.len);
        try testing.expectEqual('\n', words[0].char);
        try testing.expectEqual('\t', words[1].char);
        try testing.expectEqual(0, words[2].char);
    }
}

test "lex strings" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic strings
    {
        const words = try lex(allocator, "\"hello\" \"world\" \"\"");
        try testing.expectEqual(@as(usize, 3), words.len);
        try testing.expectEqualStrings("hello", words[0].string);
        try testing.expectEqualStrings("world", words[1].string);
        try testing.expectEqualStrings("", words[2].string);
    }

    // Escape sequences
    {
        const words = try lex(allocator, "\"\\n\\t\\r\" \"\\\"quoted\\\"\" \"back\\\\slash\" \"null\\0char\"");
        try testing.expectEqual(@as(usize, 4), words.len);
        try testing.expectEqualStrings("\n\t\r", words[0].string);
        try testing.expectEqualStrings("\"quoted\"", words[1].string);
        try testing.expectEqualStrings("back\\slash", words[2].string);
        try testing.expectEqualStrings("null\x00char", words[3].string);
    }

    // Strings with spaces
    {
        const words = try lex(allocator, "\"hello world\" \"multiple   spaces\" \"tabs\\there\"");
        try testing.expectEqual(@as(usize, 3), words.len);
        try testing.expectEqualStrings("hello world", words[0].string);
        try testing.expectEqualStrings("multiple   spaces", words[1].string);
        try testing.expectEqualStrings("tabs\there", words[2].string);
    }

    // Invalid escape sequences
    {
        const words = try lex(allocator, "\"\\x\" \"\\q\" \"\\1\"");
        try testing.expectEqual(@as(usize, 3), words.len);
        try testing.expectEqualStrings("x", words[0].string);
        try testing.expectEqualStrings("q", words[1].string);
        try testing.expectEqualStrings("1", words[2].string);
    }
}

test "lex identifiers" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Basic identifiers and operators
    {
        const words = try lex(allocator, "+ - * / dup drop swap over");
        try testing.expectEqual(@as(usize, 8), words.len);
        try testing.expectEqual(Builtin.@"+", words[0].builtin);
        try testing.expectEqual(Builtin.@"-", words[1].builtin);
        try testing.expectEqual(Builtin.@"*", words[2].builtin);
        try testing.expectEqual(Builtin.@"/", words[3].builtin);
        try testing.expectEqual(Builtin.dup, words[4].builtin);
        try testing.expectEqual(Builtin.drop, words[5].builtin);
        try testing.expectEqual(Builtin.swap, words[6].builtin);
        try testing.expectEqual(Builtin.over, words[7].builtin);
    }

    // Complex identifiers
    {
        const words = try lex(allocator, "foo123 _bar baz_ under_score CamelCase");
        try testing.expectEqual(@as(usize, 5), words.len);
        try testing.expectEqualStrings("foo123", words[0].identifier);
        try testing.expectEqualStrings("_bar", words[1].identifier);
        try testing.expectEqualStrings("baz_", words[2].identifier);
        try testing.expectEqualStrings("under_score", words[3].identifier);
        try testing.expectEqualStrings("CamelCase", words[4].identifier);
    }

    // Funny identifiers
    {
        const words = try lex(allocator, "1+ 2/ works?");
        try testing.expectEqual(@as(usize, 3), words.len);
        try testing.expectEqualStrings("1+", words[0].identifier);
        try testing.expectEqualStrings("2/", words[1].identifier);
        try testing.expectEqualStrings("works?", words[2].identifier);
    }
}

test "lex quotes" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    {
        const words = try lex(allocator, "'test");
        try testing.expectEqual(@as(usize, 1), words.len);
        try testing.expectEqualStrings("test", words[0].quoted);
    }
}

test "lex mixed input" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Mixed types
    {
        const words = try lex(allocator, "42 3.14 \"hello\" + -17 \"world\" swap 'swap");
        try testing.expectEqual(@as(usize, 8), words.len);
        try testing.expectEqual(@as(i64, 42), words[0].int);
        try testing.expectApproxEqAbs(@as(f64, 3.14), words[1].float, 0.001);
        try testing.expectEqualStrings("hello", words[2].string);
        try testing.expectEqual(Builtin.@"+", words[3].builtin);
        try testing.expectEqual(@as(i64, -17), words[4].int);
        try testing.expectEqualStrings("world", words[5].string);
        try testing.expectEqual(Builtin.swap, words[6].builtin);
        try testing.expectEqualStrings("swap", words[7].quoted);
    }

    // Different whitespace
    {
        const words = try lex(allocator, "1\n2\t3  4\n\t  5");
        try testing.expectEqual(@as(usize, 5), words.len);
        for (words, 1..) |word, i| {
            try testing.expectEqual(@as(i64, @intCast(i)), word.int);
        }
    }

    // Adjacent strings and numbers
    {
        const words = try lex(allocator, "\"no\"\"space\"42\"between\"3.14");
        try testing.expectEqual(@as(usize, 5), words.len);
        try testing.expectEqualStrings("no", words[0].string);
        try testing.expectEqualStrings("space", words[1].string);
        try testing.expectEqual(@as(i64, 42), words[2].int);
        try testing.expectEqualStrings("between", words[3].string);
        try testing.expectApproxEqAbs(@as(f64, 3.14), words[4].float, 0.001);
    }

    // Empty input
    {
        const words = try lex(allocator, "");
        try testing.expectEqual(@as(usize, 0), words.len);
    }

    // Only whitespace
    {
        const words = try lex(allocator, "   \n\t  \n  ");
        try testing.expectEqual(@as(usize, 0), words.len);
    }
}

const std = @import("std");
const testing = std.testing;
const math = std.math;
const fmt = std.fmt;
const fs = std.fs;
const heap = std.heap;
const debug = std.debug;
const meta = std.meta;
const process = std.process;
const posix = std.posix;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const ArrayList = std.ArrayList;
const DebugAllocator = heap.DebugAllocator;
const ArenaAllocator = heap.ArenaAllocator;
const StaticStringMap = std.StaticStringMap;
