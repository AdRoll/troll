-module(troll_retro).
-author('josh@qhool.com').

-export([start/0, add_triggers/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([tf/2, tf_cast/1]).

-export_type([trigger_action/0, trigger_function/1, trigger_spec/1]).

-type trigger_action() :: start_trace | print_trace | print_message |
                          flush_trace | end_trace | abort_trace | none.
-type trigger_function(State) ::
        fun((TraceMessage :: tuple(),State) -> trigger_action() |
                                               {trigger_action(),State}).

-type trigger_spec(State) ::
        #{ key := term(),
           function := trigger_function(State),
           init_state := State,
           patterns := [ TracePattern :: tuple() ],
           capture_patterns => [ TracePattern :: tuple() ],
           start_limit => pos_integer(),
           print_limit => pos_integer(),
           message_limit => pos_integer(),
           on_message_limit => atom() }.

start() ->
    gen_server:start({local,?MODULE},?MODULE, [], []).

add_triggers([_|_]=Triggers) ->
    gen_server:call(?MODULE,{add_triggers, Triggers});
add_triggers(Trigger) ->
    add_triggers([Trigger]).


%% gen_server implementation
-record(rt_st,{tracer,
               pids = #{},
               mrefs = #{},
               triggers = #{},
               log_level = info}).
-record(rt_trigger,{key,
                    function,
                    state,
                    tpls,
                    match_mfs,
                    bad_patterns,
                    start_limit = 100,
                    print_limit = 5,
                    message_limit = 100,
                    on_message_limit = rotate :: rotate | end_trace |
                                                 abort_trace,
                    active = true,
                    pids = #{} }).

init(_) ->
    {ok, #rt_st{}}.

handle_call({monitor,Pid}, _From, #rt_st{ pids = Pids } = State)
  when is_map_key(Pid,Pids) ->
    {reply,ok,State};
handle_call({monitor,Pid}, _From, #rt_st{ pids = Pids, mrefs = MRefs } = State) ->
    MRef = monitor(process,Pid),
    State1 = State#rt_st{ pids = Pids#{ Pid => MRef },
                          mrefs = MRefs#{ MRef => Pid } },
    {reply,ok,State1};
handle_call({add_triggers, TriggerSpecs}, _From, State) ->
    case build_triggers(TriggerSpecs, []) of
        #{} = M when map_size(M) == 0 ->
            err("No valid trigger specs~n",[]),
            {reply,{error,no_valid_specs},State};
        Triggers ->
            #rt_st{ triggers = Existing } = State1 = ensure_tracing(State),
            tf_cast({add_triggers, Triggers}),
            maps:map(fun set_trigger_tpls/2, Triggers),
            {reply,ok,State1#rt_st{ triggers = maps:merge(Existing, Triggers)}}
    end;
handle_call(Msg, From, State) ->
    warn("Unknown call from ~p: ~p~n",[From,Msg]),
    {reply,ok,State}.

handle_info({'DOWN',MRef, process, Pid, _Reason} = Down,
            #rt_st{ pids = Pids, mrefs = MRefs } = State) ->
    State1 = State#rt_st{ pids = maps:remove(Pid,Pids),
                          mrefs = maps:remove(MRef,MRefs) },
    tf_cast(Down),
    {noreply, State1};
handle_info(Msg, State) ->
    {noreply, handle_print(warning,"Unknown message: ~p~n",[Msg], State)}.

ensure_tracing(#rt_st{ tracer = Tracer } = State) when is_pid(Tracer) ->
    State;
ensure_tracing(State) ->
    TracerState = tf_init(),
    {ok,Tracer} = dbg:tracer(process,{fun ?MODULE:tf/2,TracerState}),
    dbg:p(all,call),
    dbg:tpl(?MODULE,tf_cast,[{'_',[],[]}]),
    link(Tracer),
    tf_cast({set_manager,self()}),
    info("Started tracer ~p~n",[Tracer]),
    State#rt_st{ tracer = Tracer }.

set_trigger_tpls(_K, #rt_trigger{ tpls = TPLs }) ->
    [ erlang:apply(dbg,tpl,TPL) || TPL <- TPLs ].

build_triggers([TrigSpec|More], Out) when is_map_key(key, TrigSpec),
                                          is_map_key(function, TrigSpec),
                                          is_map_key(init_state, TrigSpec),
                                          is_map_key(patterns, TrigSpec) ->
    case maps:fold(fun build_trigger_field/3, #rt_trigger{}, TrigSpec) of
        #rt_trigger{ key = Key, tpls = [] } ->
            err("Trigger ~p had no usable trace patterns.~n",[Key]),
            build_triggers(More, Out);
        #rt_trigger{ key = Key, bad_patterns = [_|_] = BadPats } = Trigger ->
            warn("Trigger ~p had incomprehensible trace patterns: ~p~n",[Key, BadPats]),
            build_triggers(More, [Trigger|Out])
    end;
build_triggers([BadSpec|More], Out) ->
    err("Badly formed trigger spec: ~p~n",[BadSpec]),
    build_triggers(More, Out);
build_triggers([], Out) ->
    maps:from_list( [ {K, Trig} || #rt_trigger{ key = K } = Trig <- Out ] ).

build_trigger_field(key, Key, Trig) ->
    Trig#rt_trigger{ key = Key };
build_trigger_field(function, Fun, Trig) ->
    Trig#rt_trigger{ function = Fun };
build_trigger_field(init_state, State, Trig) ->
    Trig#rt_trigger{ state = State };
build_trigger_field(patterns, Pats, Trig) ->
    {TPLs, MFs, Unknowns} = normalize_patterns(Pats),
    Trig#rt_trigger{ tpls = TPLs, match_mfs = MFs, bad_patterns = Unknowns };
build_trigger_field(start_limit, StartLim, Trig) ->
    Trig#rt_trigger{ start_limit = StartLim };
build_trigger_field(print_limit, PrintLim, Trig) ->
    Trig#rt_trigger{ print_limit = PrintLim };
build_trigger_field(message_limit, MsgLim, Trig) ->
    Trig#rt_trigger{ message_limit = MsgLim };
build_trigger_field(on_message_limt, OnMsgLim, Trig) ->
    Trig#rt_trigger{ on_message_limit = OnMsgLim };
build_trigger_field(_, _, Trig) ->
    Trig.

handle_cast({print, Level, Fmt, Args}, State) ->
    {noreply, handle_print(Level,Fmt,Args,State)};
handle_cast(Msg, State) ->
    warn("Unknown cast: ~p~n",[Msg]),
    {noreply, State}.

numeric_level(debug) -> 0;
numeric_level(info)  -> 1;
numeric_level(warn)  -> 2;
numeric_level(error) -> 3.

handle_print(Level, Fmt, Args, #rt_st{ log_level = SetLevel } = State) ->
    case numeric_level(Level) >= numeric_level(SetLevel) of
        true ->
            io:format(standard_error, "TR ~s: "++Fmt,
                      [string:uppercase(atom_to_list(Level))|Args]);
        _ -> ok
    end,
    State.

-record(rt_pid,{messages = queue:new(),
                message_count = 0,
                message_limit = 100,
                on_limit = rotate :: rotate | stop,
                printer = fun default_printer/1,
                triggers = []}).
-record(rt_trc,{manager,
                printer = fun default_printer/1,
                store = true,
                pids = #{},
                triggers = #{}
              }).

tf_init() ->
    #rt_trc{}.

tf_cast(_Message) ->
    ok.

tf(Msg, St) ->
    dbg("tf: ~p ~p~n",[Msg,St]),
    try tf_(Msg, St)
    catch E:R:S ->
            dbg("tf_ ~p:~p: ~n~p~n",[E,R,S]),
            St
    end.

tf_({trace,_,call,{?MODULE,tf_cast,[Message]}}, #rt_trc{} = St) ->
    handle_tf_cast(Message, St);
tf_(Msg, #rt_trc{}=St) ->
    St1 = store_message(Msg, St),
    St2 = run_triggers(Msg, St1),
    St2;
tf_(A,St) ->
    warn("UNKNOWN TRACE MESSAGE ~w~n", [A]),
    St.

handle_tf_cast({'DOWN',_,process,Pid,_}, St) ->
    end_trace(Pid, St);
handle_tf_cast({add_triggers, Triggers}, #rt_trc{ triggers = Existing } = St) ->
    St#rt_trc{ triggers = maps:merge(Existing, Triggers) };
handle_tf_cast({set_manager, Pid}, St) ->
    St#rt_trc{ manager = Pid }.

run_triggers(Msg, #rt_trc{ triggers = Triggers } = St) ->
    dbg("run_triggers:~n~p~n",[Triggers]),
    maps:fold(fun(K, Trig, St0) ->
                      run_trigger(Msg, trace_msg_pid(Msg), K, Trig, St0)
              end, St, Triggers).

run_trigger(Msg, Pid, K, #rt_trigger{ function = Fun, state = TrigSt,
                                      active = Active, pids = TrigPids } = Trig,
            St) when Active; is_map_key(Pid, TrigPids) ->
    dbg("run_trigger: ~p~n",[Trig]),
    #rt_trc{ triggers = Trigs } = St,
    {MaybeAction, TrigSt1} =
        case Fun(Msg, TrigSt) of
            {A, _} = Res when is_atom(A) ->
                Res;
            A when is_atom(A) ->
                {A, TrigSt};
            TrigSt0 ->
                {none, TrigSt0}
        end,
    dbg("Trigger said ~p~n", [MaybeAction]),
    {Action, TrigSt2} =
        case MaybeAction of
            Act when Act =:= start_trace; Act =:= print_trace;
                     Act =:= flush_trace; Act =:= end_trace; Act =:= abort_trace ->
                {Act, TrigSt1};
            Act when Act =:= ok; Act =:= pass; Act =:= none ->
                {none, TrigSt1};
            _ ->
                {none, {MaybeAction, TrigSt1}}
        end,
    Trig1 = Trig#rt_trigger{ state = TrigSt2 },
    St1 = St#rt_trc{ triggers = Trigs#{ K => Trig1 } },
    case Action of
        start_trace ->   %%% begin storing trace messages for pid
            start_trace(Msg, Trig1, St1);
        print_trace ->   %%% print and clear accumulated messages
            print_trace(Msg, St1),
            flush_trace(Msg, incr_prints(Trig1, St1));
        print_message -> %%% print single message without starting to store
            print_message(Msg, incr_prints(Trig1,St1));
        flush_trace ->   %%% clear messages without printing
            flush_trace(Msg, St1);
        end_trace   ->   %%% print messages and stop tracing
            print_trace(Msg, St1),
            end_trace(Msg, incr_prints(Trig1, St1));
        abort_trace ->   %%% stop tracing without printing
            end_trace(Msg, St1);
        none ->
            St1
    end;
run_trigger(_Msg, _Pid, K, _Trigger, St) ->
    dbg("Skipping trigger ~p~n",[K]),
    St.

start_trace(Msg, Trig, St) ->
    start_trace(Msg, trace_msg_pid(Msg), Trig, St).

start_trace(_Msg, Pid, _Trig, #rt_trc{ pids = Pids } = St)
  when is_map_key(Pid, Pids) ->
    St;
start_trace(Msg, Pid, #rt_trigger{ active = true,
                                   message_limit = Lim,
                                   on_message_limit = OnLim }=Trig, St) ->
    PidTrc = #rt_pid{ message_limit = Lim,
                      on_limit = OnLim },
    St1 = incr_starts(Pid, Trig, St),
    store_message(Msg, set_trace(Msg, PidTrc, St1));
start_trace(_Msg, _Pid, _Trig, St) ->
    St.

store_message(Msg, St) ->
    store_message(Msg, get_trace(Msg, St), St).

store_message(_Msg, error, St) ->
    St;
store_message(Msg, {ok, #rt_pid{ message_count = N,
                               message_limit = Lim,
                               messages = Msgs } = PidTrace},
            St) when N =< Lim ->
    PidTrace1 = PidTrace#rt_pid{ messages = queue:in_r(Msg, Msgs),
                                 message_count = N + 1 },
    set_trace(Msg, PidTrace1, St);
store_message(Msg, {ok, #rt_pid{ on_limit = stop } }, St) ->
    warn("TRACE LIMIT HIT FOR ~p~n",[trace_msg_pid(Msg)]),
    end_trace(Msg, St);
store_message(Msg, {ok, #rt_pid{ messages = Msgs,
                               message_count = N,
                               on_limit = rotate } = PidTrace},
            St) ->
    PidTrace1 = PidTrace#rt_pid{ messages = queue:in_r(Msg, queue:drop_r(Msgs)),
                                 message_count = N - 1 },
    set_trace(Msg, PidTrace1, St).

print_message(Msg, #rt_trc{ printer = Printer } = St) ->
    Printer(Msg),
    St.

print_trace(MsgOrPid, #rt_trc{ printer = Printer } = St) ->
    {ok, #rt_pid{ messages = Msgs,
                  printer = Printer}} = get_trace(MsgOrPid, St),
    MsgList = queue:to_list(Msgs),
    dbg("Printing ~p~n",[MsgList]),
    [ Printer(Msg) || Msg <- queue:to_list(Msgs) ],
    St.

flush_trace(Msg, St) ->
    {ok, PidTrc} = get_trace(Msg, St),
    PidTrc1 = PidTrc#rt_pid{ messages = queue:new(),
                             message_count = 0 },
    set_trace(Msg, PidTrc1, St).

end_trace(Msg, St) ->
    remove_trace(Msg, St).

%% getters/setters/utility
get_trace(Msg, St) when not is_pid(Msg) ->
    get_trace(trace_msg_pid(Msg), St);
get_trace(Pid, #rt_trc{ pids = Pids }) ->
    maps:find(Pid, Pids).

set_trace(Msg, Trc, St) when not is_pid(Msg) ->
    set_trace(trace_msg_pid(Msg), Trc, St);
set_trace(Pid, #rt_pid{} = Trc, #rt_trc{ pids = Pids } = St) ->
    St#rt_trc{ pids = Pids#{ Pid => Trc } }.

remove_trace(Msg, St) when not is_pid(Msg) ->
    remove_trace(trace_msg_pid(Msg), St);
remove_trace(Pid, #rt_trc{ pids = Pids } = St) ->
    St#rt_trc{ pids = maps:remove(Pid, Pids) }.

incr_starts(Pid, #rt_trigger{ key = K, start_limit = Lim, pids = Pids } = Trigger,
            #rt_trc{ triggers = Triggers } = State) when Lim > 1 ->
    Trigger1 = Trigger#rt_trigger{ start_limit = Lim - 1,
                                   pids = Pids#{ Pid => true } },
    State#rt_trc{ triggers = Triggers#{ K => Trigger1 } };
incr_starts(Pid, #rt_trigger{ key = K, pids = Pids } = Trigger,
            #rt_trc{ triggers = Triggers } = State) ->
    info("Trigger ~p hit start limit~n",[K]),
    Trigger1 = Trigger#rt_trigger{ active = false,
                                   pids = Pids#{ Pid => true } },
    State#rt_trc{ triggers = Triggers#{ K => Trigger1 } }.


incr_prints(#rt_trigger{ key = K, print_limit = Lim } = Trigger,
            #rt_trc{ triggers = Triggers } = State) when Lim > 1 ->
    Trigger1 = Trigger#rt_trigger{ print_limit = Lim - 1 },
    State#rt_trc{ triggers = Triggers#{ K => Trigger1 }};
incr_prints(#rt_trigger{ key = K }, #rt_trc{ triggers = Triggers } = State) ->
    info("Trigger ~p hit print limit~n",[K]),
    State#rt_trc{ triggers = maps:remove(K, Triggers) }.

-define(PRINTFN(NAME,LEVEL),
NAME(Fmt,Args) ->
    gen_server:cast(?MODULE,{print,LEVEL,Fmt,Args})).

?PRINTFN(dbg,debug).
?PRINTFN(info,info).
?PRINTFN(warn,warn).
?PRINTFN(err,error).

trace_msg_pid({trace,Pid,call,_MFA}) ->
    Pid;
trace_msg_pid({trace,Pid,return_from,_MFA,_R}) ->
    Pid.

normalize_patterns(Patterns) ->
    {Patterns0,MFs,Unknowns} = lists:unzip3(lists:map(fun normalize_pattern/1, Patterns)),
    TPLs = lists:map(fun tuple_to_list/1, Patterns0),
    {TPLs, MFs, Unknowns}.

normalize_pattern({Mod,Fun}) when is_atom(Mod), is_atom(Fun) ->
    {{Mod,Fun,[{'_',[],[{return_trace}]}]}, {Mod,Fun}, []};
normalize_pattern({Mod,[_|_]=MatchSpec}) when is_atom(Mod) ->
    {{Mod,MatchSpec}, {Mod,'_'}, []};
normalize_pattern({Mod,Fun,[_|_]=MatchSpec}) when is_atom(Mod), is_atom(Fun) ->
    {{Mod,Fun,MatchSpec}, {Mod,Fun}, []};
normalize_pattern(Mod) when is_atom(Mod) ->
    {{Mod,[{'_',[],[{return_trace}]}]}, {Mod,'_'}, []};
normalize_pattern(Other) ->
    {[],[],Other}.

default_printer({trace,Pid,call,{M,F,A}}) ->
    ArgsFmt =
        case length(A) of
            0 -> "";
            Len ->
                [_,_|AF] = lists:flatten(lists:duplicate(Len,", ~p")),
                AF
        end,
    io:format("[~p] call   ~p:~p("++ArgsFmt++")~n", [Pid,M,F|A]);
default_printer({trace,Pid,return_from,{M,F,_A},R}) ->
    io:format("[~p] return ~p:~p(…) -> ~p~n",[Pid,M,F,R]);
default_printer(Other) ->
    io:format("??unknown?? ~p~n",[Other]).
