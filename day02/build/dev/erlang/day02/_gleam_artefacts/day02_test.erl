-module(day02_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0, count_safe_test/0, count_safe_damp_test/0]).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/test/day02_test.gleam", 6).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/test/day02_test.gleam", 18).
-spec count_safe_test() -> nil.
count_safe_test() ->
    Answer = day02:count_safe(
        <<"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"/utf8>>
    ),
    gleeunit_ffi:should_be_ok(Answer),
    Safe = gleam@result:unwrap(Answer, -1),
    gleeunit_ffi:should_equal(Safe, 2).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/test/day02_test.gleam", 28).
-spec count_safe_damp_test() -> nil.
count_safe_damp_test() ->
    Answer = day02:count_safe_dampered(
        <<"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"/utf8>>
    ),
    gleeunit_ffi:should_be_ok(Answer),
    Safe = gleam@result:unwrap(Answer, -1),
    gleeunit_ffi:should_equal(Safe, 4).
