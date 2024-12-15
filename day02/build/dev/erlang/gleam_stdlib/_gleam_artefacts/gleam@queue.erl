-module(gleam@queue).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, from_list/1, to_list/1, is_empty/1, length/1, push_back/2, push_front/2, pop_back/1, pop_front/1, reverse/1, is_logically_equal/3, is_equal/2]).
-export_type([queue/1]).

-opaque queue(EWE) :: {queue, list(EWE), list(EWE)}.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 24).
-spec new() -> queue(any()).
new() ->
    {queue, [], []}.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 41).
-spec from_list(list(EWH)) -> queue(EWH).
from_list(List) ->
    {queue, [], List}.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 58).
-spec to_list(queue(EWK)) -> list(EWK).
to_list(Queue) ->
    _pipe = erlang:element(3, Queue),
    lists:append(_pipe, lists:reverse(erlang:element(2, Queue))).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 85).
-spec is_empty(queue(any())) -> boolean().
is_empty(Queue) ->
    (erlang:element(2, Queue) =:= []) andalso (erlang:element(3, Queue) =:= []).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 112).
-spec length(queue(any())) -> integer().
length(Queue) ->
    erlang:length(erlang:element(2, Queue)) + erlang:length(
        erlang:element(3, Queue)
    ).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 126).
-spec push_back(queue(EWR), EWR) -> queue(EWR).
push_back(Queue, Item) ->
    {queue, [Item | erlang:element(2, Queue)], erlang:element(3, Queue)}.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 140).
-spec push_front(queue(EWU), EWU) -> queue(EWU).
push_front(Queue, Item) ->
    {queue, erlang:element(2, Queue), [Item | erlang:element(3, Queue)]}.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 173).
-spec pop_back(queue(EWX)) -> {ok, {EWX, queue(EWX)}} | {error, nil}.
pop_back(Queue) ->
    case Queue of
        {queue, [], []} ->
            {error, nil};

        {queue, [], Out} ->
            pop_back({queue, lists:reverse(Out), []});

        {queue, [First | Rest], Out@1} ->
            Queue@1 = {queue, Rest, Out@1},
            {ok, {First, Queue@1}}
    end.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 213).
-spec pop_front(queue(EXC)) -> {ok, {EXC, queue(EXC)}} | {error, nil}.
pop_front(Queue) ->
    case Queue of
        {queue, [], []} ->
            {error, nil};

        {queue, In, []} ->
            pop_front({queue, [], lists:reverse(In)});

        {queue, In@1, [First | Rest]} ->
            Queue@1 = {queue, In@1, Rest},
            {ok, {First, Queue@1}}
    end.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 247).
-spec reverse(queue(EXH)) -> queue(EXH).
reverse(Queue) ->
    {queue, erlang:element(3, Queue), erlang:element(2, Queue)}.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 271).
-spec check_equal(
    list(EXN),
    list(EXN),
    list(EXN),
    list(EXN),
    fun((EXN, EXN) -> boolean())
) -> boolean().
check_equal(Xs, X_tail, Ys, Y_tail, Eq) ->
    case {Xs, X_tail, Ys, Y_tail} of
        {[], [], [], []} ->
            true;

        {[X | Xs@1], _, [Y | Ys@1], _} ->
            case Eq(X, Y) of
                false ->
                    false;

                true ->
                    check_equal(Xs@1, X_tail, Ys@1, Y_tail, Eq)
            end;

        {[], [_ | _], _, _} ->
            check_equal(lists:reverse(X_tail), [], Ys, Y_tail, Eq);

        {_, _, [], [_ | _]} ->
            check_equal(Xs, X_tail, lists:reverse(Y_tail), [], Eq);

        {_, _, _, _} ->
            false
    end.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 263).
-spec is_logically_equal(queue(EXK), queue(EXK), fun((EXK, EXK) -> boolean())) -> boolean().
is_logically_equal(A, B, Element_is_equal) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        Element_is_equal
    ).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/build/packages/gleam_stdlib/src/gleam/queue.gleam", 301).
-spec is_equal(queue(EXS), queue(EXS)) -> boolean().
is_equal(A, B) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        fun(A@1, B@1) -> A@1 =:= B@1 end
    ).
