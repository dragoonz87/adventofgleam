-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 2).
-spec compose(fun((DOL) -> DOM), fun((DOM) -> DON)) -> fun((DOL) -> DON).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 7).
-spec curry2(fun((DOO, DOP) -> DOQ)) -> fun((DOO) -> fun((DOP) -> DOQ)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 12).
-spec curry3(fun((DOS, DOT, DOU) -> DOV)) -> fun((DOS) -> fun((DOT) -> fun((DOU) -> DOV))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 17).
-spec curry4(fun((DOX, DOY, DOZ, DPA) -> DPB)) -> fun((DOX) -> fun((DOY) -> fun((DOZ) -> fun((DPA) -> DPB)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 22).
-spec curry5(fun((DPD, DPE, DPF, DPG, DPH) -> DPI)) -> fun((DPD) -> fun((DPE) -> fun((DPF) -> fun((DPG) -> fun((DPH) -> DPI))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 27).
-spec curry6(fun((DPK, DPL, DPM, DPN, DPO, DPP) -> DPQ)) -> fun((DPK) -> fun((DPL) -> fun((DPM) -> fun((DPN) -> fun((DPO) -> fun((DPP) -> DPQ)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 36).
-spec flip(fun((DPS, DPT) -> DPU)) -> fun((DPT, DPS) -> DPU).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 42).
-spec identity(DPV) -> DPV.
identity(X) ->
    X.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 47).
-spec constant(DPW) -> fun((any()) -> DPW).
constant(Value) ->
    fun(_) -> Value end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 56).
-spec tap(DPY, fun((DPY) -> any())) -> DPY.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 62).
-spec apply1(fun((DQA) -> DQB), DQA) -> DQB.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 67).
-spec apply2(fun((DQC, DQD) -> DQE), DQC, DQD) -> DQE.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 72).
-spec apply3(fun((DQF, DQG, DQH) -> DQI), DQF, DQG, DQH) -> DQI.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
