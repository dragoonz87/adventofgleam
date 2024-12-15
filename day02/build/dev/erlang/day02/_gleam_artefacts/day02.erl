-module(day02).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([count_safe/1, count_safe_dampered/1, main/0]).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 49).
-spec get_reports_internal(list(binary()), list(list(integer()))) -> {ok,
        list(list(integer()))} |
    {error, nil}.
get_reports_internal(Input, Output) ->
    case Input of
        [] ->
            {ok, Output};

        [<<""/utf8>>] ->
            {ok, Output};

        [First | Rest] ->
            Nums = begin
                _pipe = First,
                _pipe@1 = gleam@string:split(_pipe, <<" "/utf8>>),
                gleam@list:map(_pipe@1, fun(N) -> gleam_stdlib:parse_int(N) end)
            end,
            case gleam@list:any(
                Nums,
                fun(N@1) -> gleam@result:is_error(N@1) end
            ) of
                true ->
                    {error, nil};

                false ->
                    get_reports_internal(
                        Rest,
                        lists:append(
                            Output,
                            [gleam@list:map(
                                    Nums,
                                    fun(N@2) -> gleam@result:unwrap(N@2, -1) end
                                )]
                        )
                    )
            end
    end.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 45).
-spec get_reports(list(binary())) -> {ok, list(list(integer()))} | {error, nil}.
get_reports(Input) ->
    get_reports_internal(Input, []).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 94).
-spec is_safe_internal(list(integer()), boolean(), boolean(), integer()) -> boolean().
is_safe_internal(Result, Increasing, Decreasing, Last) ->
    case Result of
        [] ->
            true;

        [Level | Rest] ->
            case {Increasing, Decreasing} of
                {false, false} ->
                    case Last of
                        -1 ->
                            is_safe_internal(Rest, false, false, Level);

                        L ->
                            case {Level, gleam@int:absolute_value(Level - L)} of
                                {_, Diff} when (Level > L) andalso ((Diff >= 1) andalso (Diff =< 3)) ->
                                    is_safe_internal(Rest, true, false, Level);

                                {_, Diff@1} when (Level < L) andalso ((Diff@1 >= 1) andalso (Diff@1 =< 3)) ->
                                    is_safe_internal(Rest, false, true, Level);

                                {_, _} ->
                                    false
                            end
                    end;

                {true, false} ->
                    case Level - Last of
                        Val when (Val >= 1) andalso (Val =< 3) ->
                            is_safe_internal(Rest, true, false, Level);

                        _ ->
                            false
                    end;

                {false, true} ->
                    case Last - Level of
                        Val@1 when (Val@1 >= 1) andalso (Val@1 =< 3) ->
                            is_safe_internal(Rest, false, true, Level);

                        _ ->
                            false
                    end;

                {true, true} ->
                    false
            end
    end.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 90).
-spec is_safe(list(integer())) -> boolean().
is_safe(Result) ->
    is_safe_internal(Result, false, false, -1).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 81).
-spec map_safe_internal(list(list(integer())), list(boolean())) -> list(boolean()).
map_safe_internal(Results, Output) ->
    case Results of
        [] ->
            Output;

        [First | Rest] ->
            map_safe_internal(Rest, lists:append(Output, [is_safe(First)]))
    end.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 73).
-spec map_safe(list(list(integer()))) -> list(boolean()).
map_safe(Input) ->
    map_safe_internal(Input, []).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 21).
-spec count_safe(binary()) -> {ok, integer()} | {error, nil}.
count_safe(Input) ->
    gleam@result:'try'(
        get_reports(gleam@string:split(Input, <<"\n"/utf8>>)),
        fun(Reports) ->
            Count = begin
                _pipe = Reports,
                _pipe@1 = map_safe(_pipe),
                _pipe@2 = gleam@list:filter(_pipe@1, fun(R) -> R end),
                erlang:length(_pipe@2)
            end,
            {ok, Count}
        end
    ).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 77).
-spec map_safe_dampered(list(list(integer()))) -> list({boolean(),
    list(integer())}).
map_safe_dampered(Input) ->
    gleam@list:zip(map_safe_internal(Input, []), Input).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 190).
-spec damper_internal(list(integer()), list(integer())) -> boolean().
damper_internal(Result, Arr) ->
    case Result of
        [] ->
            false;

        [Level | Rest] ->
            is_safe(lists:append(Arr, Rest)) orelse damper_internal(
                Rest,
                lists:append(Arr, [Level])
            )
    end.

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 186).
-spec damper(list(integer())) -> boolean().
damper(Result) ->
    damper_internal(Result, []).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 33).
-spec count_safe_dampered(binary()) -> {ok, integer()} | {error, nil}.
count_safe_dampered(Input) ->
    gleam@result:'try'(
        get_reports(gleam@string:split(Input, <<"\n"/utf8>>)),
        fun(Reports) ->
            Count = begin
                _pipe = Reports,
                _pipe@1 = map_safe_dampered(_pipe),
                _pipe@2 = gleam@list:filter(
                    _pipe@1,
                    fun(R) ->
                        erlang:element(1, R) orelse damper(erlang:element(2, R))
                    end
                ),
                erlang:length(_pipe@2)
            end,
            {ok, Count}
        end
    ).

-file("/home/drag/Documents/AdventOfCode/2024/Gleam/day02/src/day02.gleam", 8).
-spec main() -> {ok, integer()} | {error, nil}.
main() ->
    _assert_subject = simplifile:read(<<"input.txt"/utf8>>),
    {ok, Input} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"day02"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 9})
    end,
    _assert_subject@1 = count_safe(Input),
    {ok, Count} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@1,
                        module => <<"day02"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 10})
    end,
    gleam_stdlib:println(<<"part 1"/utf8>>),
    gleam@io:debug(Count),
    _assert_subject@2 = count_safe_dampered(Input),
    {ok, Count@1} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@2,
                        module => <<"day02"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 14})
    end,
    gleam_stdlib:println(<<"part 2"/utf8>>),
    gleam@io:debug(Count@1),
    gleam@io:debug(count_safe(<<"5 5 4"/utf8>>)).
