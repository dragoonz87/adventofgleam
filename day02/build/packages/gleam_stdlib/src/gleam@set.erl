-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, size/1, is_empty/1, contains/2, delete/2, to_list/1, fold/3, filter/2, drop/2, take/2, intersection/2, difference/2, is_subset/2, is_disjoint/2, each/2, insert/2, from_list/1, map/2, union/2, symmetric_difference/2]).
-export_type([set/1]).

-opaque set(FBM) :: {set, gleam@dict:dict(FBM, list(nil))}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 32).
-spec new() -> set(any()).
new() ->
    {set, maps:new()}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 50).
-spec size(set(any())) -> integer().
size(Set) ->
    maps:size(erlang:element(2, Set)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 68).
-spec is_empty(set(any())) -> boolean().
is_empty(Set) ->
    Set =:= new().

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 110).
-spec contains(set(FBX), FBX) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam_stdlib:map_get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 131).
-spec delete(set(FBZ), FBZ) -> set(FBZ).
delete(Set, Member) ->
    {set, gleam@dict:delete(erlang:element(2, Set), Member)}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 149).
-spec to_list(set(FCC)) -> list(FCC).
to_list(Set) ->
    maps:keys(erlang:element(2, Set)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 190).
-spec fold(set(FCI), FCK, fun((FCK, FCI) -> FCK)) -> FCK.
fold(Set, Initial, Reducer) ->
    gleam@dict:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 214).
-spec filter(set(FCL), fun((FCL) -> boolean())) -> set(FCL).
filter(Set, Predicate) ->
    {set,
        gleam@dict:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 249).
-spec drop(set(FCS), list(FCS)) -> set(FCS).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 267).
-spec take(set(FCW), list(FCW)) -> set(FCW).
take(Set, Desired) ->
    {set, gleam@dict:take(erlang:element(2, Set), Desired)}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 287).
-spec order(set(FDE), set(FDE)) -> {set(FDE), set(FDE)}.
order(First, Second) ->
    case maps:size(erlang:element(2, First)) > maps:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 305).
-spec intersection(set(FDJ), set(FDJ)) -> set(FDJ).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 323).
-spec difference(set(FDN), set(FDN)) -> set(FDN).
difference(First, Second) ->
    drop(First, to_list(Second)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 344).
-spec is_subset(set(FDR), set(FDR)) -> boolean().
is_subset(First, Second) ->
    intersection(First, Second) =:= First.

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 362).
-spec is_disjoint(set(FDU), set(FDU)) -> boolean().
is_disjoint(First, Second) ->
    intersection(First, Second) =:= new().

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 402).
-spec each(set(FEB), fun((FEB) -> any())) -> nil.
each(Set, Fun) ->
    fold(
        Set,
        nil,
        fun(Nil, Member) ->
            Fun(Member),
            Nil
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 86).
-spec insert(set(FBU), FBU) -> set(FBU).
insert(Set, Member) ->
    {set, gleam@dict:insert(erlang:element(2, Set), Member, [])}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 167).
-spec from_list(list(FCF)) -> set(FCF).
from_list(Members) ->
    Dict = gleam@list:fold(
        Members,
        maps:new(),
        fun(M, K) -> gleam@dict:insert(M, K, []) end
    ),
    {set, Dict}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 232).
-spec map(set(FCO), fun((FCO) -> FCQ)) -> set(FCQ).
map(Set, Fun) ->
    fold(Set, new(), fun(Acc, Member) -> insert(Acc, Fun(Member)) end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 282).
-spec union(set(FDA), set(FDA)) -> set(FDA).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/set.gleam", 374).
-spec symmetric_difference(set(FDX), set(FDX)) -> set(FDX).
symmetric_difference(First, Second) ->
    difference(union(First, Second), intersection(First, Second)).