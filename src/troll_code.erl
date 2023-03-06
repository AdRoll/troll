-module(troll_code).

-export([optimize_fun/1, optimize_fun/2]).

-ignore_xref([optimize_fun/2]).

optimize_fun(Fun) ->
    optimize_fun(Fun, []).

optimize_fun({M, F, A}, _Options) when is_atom(M), is_atom(F), is_integer(A) ->
    fun M:F/A;
optimize_fun(Fun, Options) when is_list(Options) ->
    optimize_fun(Fun, options_to_map(Options));
optimize_fun(Fun, Options) ->
    case erl_eval:fun_data(Fun) of
        {fun_data, BoundEnv, [{clause, _, Args, _, _} | _] = Clauses} ->
            Arity = length(Args),
            optimize(BoundEnv, Clauses, Arity, Options);
        Other ->
            Other
    end.

options_to_map(Optionlist) ->
    proplists:to_map(Optionlist ++ [], [{negations, [{no_match_spec, use_match_spec}]}]).

optimize(BoundEnv, Clauses, Arity, Options) ->
    optimize_full_ms(full_ms(BoundEnv, Clauses, Options), BoundEnv, Clauses, Arity, Options).

optimize_full_ms(undefined, BoundEnv, Clauses, Arity, Options) ->
    optimize_stripped_ms(stripped_ms(BoundEnv, Clauses, Options), BoundEnv, Clauses, Arity);
optimize_full_ms(MS, _BoundEnv, _Clauses, Arity, _Options) ->
    wrap_ms(MS, Arity).

optimize_stripped_ms(StrippedMS, BoundEnv, Clauses, Arity) ->
    {Unliterals, FilledClauses} = fill_env(BoundEnv, Clauses),
    wrap_ms_and_fun(StrippedMS, Unliterals, compile_clauses(FilledClauses), Arity).

wrap_ms_and_fun(undefined, Unliterals, {compiled, CompiledFun}, Arity) ->
    Listified = fun(ArgList) -> erlang:apply(CompiledFun, Unliterals ++ ArgList) end,
    delistify(Listified, Arity);
wrap_ms_and_fun(MS, Unliterals, {compiled, CompiledFun}, Arity) ->
    ListifiedMS = listify_ms(MS),
    Listified =
        fun(ArgList) ->
           case ListifiedMS(ArgList) of
               '$matched' ->
                   erlang:apply(CompiledFun, Unliterals ++ ArgList);
               Ret ->
                   Ret
           end
        end,
    delistify(Listified, Arity).

wrap_ms(MS, Arity) ->
    delistify(listify_ms(MS), Arity).

listify_ms(MS) ->
    CompiledMS = ets:match_spec_compile(MS),
    fun(ArgList) ->
       MSTerm = list_to_tuple(['$wrapped' | ArgList]),
       case ets:match_spec_run([MSTerm], CompiledMS) of
           [Ret] ->
               Ret;
           [] ->
               '$nomatch'
       end
    end.

compile_clauses([{clause, _, Args, _Guards, _Body} | _] = Clauses) ->
    ModuleStr =
        unicode:characters_to_list([?MODULE_STRING, integer_to_list(erlang:unique_integer())]),
    Module = list_to_atom(ModuleStr),
    ModuleFile = ModuleStr ++ ".erl",
    Arity = length(Args),
    Abstract =
        [{attribute, {1, 1}, file, {ModuleStr ++ ".erl", 1}},
         {attribute, {1, 2}, module, Module},
         {attribute, {3, 2}, export, [{execute, Arity}]},
         {function, {4, 1}, execute, Arity, Clauses},
         {eof, {5, 1}}],
    {ok, Module, Bin} =
        compile:forms(
            erl_syntax:revert_forms(Abstract), [nowarn_unused_vars]),
    {module, Module} = code:load_binary(Module, ModuleFile, Bin),
    troll_io:format("Compiled function ~p:~p/~p~n", [Module, execute, Arity]),
    {compiled, fun Module:execute/Arity}.

fill_env(BoundEnv, Clauses) ->
    {Unliterals, Literals} = lists:foldl(fun literalize_var/2, {[], []}, BoundEnv),
    {UnlitVars, UnlitVals} = lists:unzip(Unliterals),
    ExtraArgs = [{var, 0, Name} || Name <- UnlitVars],
    NewClauses = [fill_clause(ExtraArgs, Literals, Clause) || Clause <- Clauses],
    {UnlitVals, NewClauses}.

fill_clause(ExtraArgs, Literals, {clause, Loc, Args, Guards, Body}) ->
    {clause,
     Loc,
     ExtraArgs ++ Args,
     fill_forms(Literals, Guards),
     fill_forms(Literals, Body)}.

fill_forms(Literals, {var, _Loc, Name} = Var) ->
    proplists:get_value(Name, Literals, Var);
fill_forms(Literals, Form) when is_tuple(Form) ->
    list_to_tuple(fill_forms(Literals, tuple_to_list(Form)));
fill_forms(Literals, Forms) when is_list(Forms) ->
    [fill_forms(Literals, Form) || Form <- Forms];
fill_forms(_, Other) ->
    Other.

literalize_var({Name, Value}, {Unliterals, Literals}) ->
    try
        {Unliterals, [{Name, erl_syntax:abstract(Value)} | Literals]}
    catch
        _:_ ->
            {[{Name, Value} | Unliterals], Literals}
    end.

full_ms(BoundEnv, Clauses, #{use_match_spec := true}) ->
    Clauses0 = tupelize_clauses(Clauses),
    build_ms(BoundEnv, Clauses0);
full_ms(_, _, _) ->
    undefined.

stripped_ms(BoundEnv, Clauses, #{use_match_spec := true}) ->
    Clauses0 = strip_clauses(Clauses),
    build_ms(BoundEnv, Clauses0);
stripped_ms(_, _, _) ->
    undefined.

tupelize_clauses(Clauses) ->
    lists:map(fun tupelize_clause/1, Clauses).

tupelize_clause({clause, Loc, Args, Guards, Body}) ->
    Args0 = [{tuple, Loc, [{atom, Loc, '$wrapped'} | Args]}],
    {clause, Loc, Args0, Guards, Body}.

strip_clauses(Clauses) ->
    Stripped = lists:map(fun strip_clause/1, Clauses),
    tupelize_clauses(Stripped).

strip_clause({clause, Loc, Args, Guards, Body}) ->
    {clause, Loc, Args, Guards, [strip_clause_body(Body)]}.

strip_clause_body([Val]) ->
    try
        erl_syntax:abstract(Val)
    catch
        _:_ ->
            {atom, 0, '$matched'}
    end;
strip_clause_body(_) ->
    {atom, 0, '$matched'}.

build_ms(BoundEnv, Clauses) ->
    try ms_transform:transform_from_shell(dbg, Clauses, BoundEnv) of
        {error, Err} ->
            troll_io:format("error transforming to match spec: ~p~n", [Err]),
            undefined;
        MS ->
            MS
    catch
        E:R:S ->
            troll_io:format("~p:~p when transforming to match spec at:~n~p~n", [E, R, S]),
            undefined
    end.

%% Yuck!
delistify(F, 1) ->
    fun(A) -> F([A]) end;
delistify(F, 2) ->
    fun(A, B) -> F([A, B]) end;
delistify(F, 3) ->
    fun(A, B, C) -> F([A, B, C]) end;
delistify(F, 4) ->
    fun(A, B, C, D) -> F([A, B, C, D]) end;
delistify(F, 5) ->
    fun(A, B, C, D, E) -> F([A, B, C, D, E]) end.
