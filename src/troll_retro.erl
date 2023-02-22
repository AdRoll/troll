-module(troll_retro).
-author('josh@qhool.com').

-export([start/0,start/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([tf/2, tf_cast/1]).

-record(rt_st,{tracer,
               pids = #{},
               mrefs = #{}}).
start() ->
    start([],[]).

start(MFSpecs, Triggers) ->
    gen_server:start({local,?MODULE},?MODULE, [MFSpecs, Triggers], []).

init([MFSpecs,Triggers]) ->
    TracerState = tf_init(self(), Triggers),
    {ok,Tracer} = dbg:tracer(process,{?MODULE,tf,TracerState}),
    dbg:p(all,call),
    dbg:tpl(?MODULE,tf_call,[{'_',[],[]}]),
    Known =
        [ begin dbg:tpl(Mod,Fun,[{'_',[],[{return_trace}]}]), Spec end
          || {Mod,Fun} = Spec <- MFSpecs,
             is_atom(Mod) andalso is_atom(Fun) ] ++
        [ begin dbg:tpl(Mod, Match), Spec end
          || {Mod,[_|_]=Match} = Spec <- MFSpecs, is_atom(Mod) ] ++
        [ begin dbg:tpl(Mod,Fun,Match), Spec end
          || {Mod,Fun,[_|_]=Match} = Spec <- MFSpecs,
             is_atom(Mod), is_atom(Fun) ],
    Unknown = MFSpecs -- Known,
    case Unknown of
        [] -> ok;
        _ -> io:format(standard_error,
                       [ "Some tracing specs not understood:~n" |
                         [ "~t~p~n" || _ <- Unknown ] ],
                       Unknown)
    end,
    case {Known,MFSpecs} of
        {[],[_|_]} ->
            io:format(standard_error, "No tracing specs made sense.~n",[]);
        _ ->
            ok
    end,
    link(Tracer),
    {ok, #rt_st{ tracer = Tracer }}.

handle_call({monitor,Pid}, _From, #rt_st{ pids = Pids } = State)
  when is_map_key(Pid,Pids) ->
    {reply,ok,State};
handle_call({monitor,Pid}, _From, #rt_st{ pids = Pids, mrefs = MRefs } = State) ->
    MRef = monitor(process,Pid),
    State1 = State#rt_st{ pids = Pids#{ Pid => MRef },
                          mrefs = MRefs#{ MRef => Pid } },
    {reply,ok,State1};
handle_call(Msg, From, State) ->
    io:format("Unknown call from ~p: ~p~n",[From,Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Unknown cast: ~p~n",[Msg]),
    {noreply, State}.

handle_info({'DOWN',MRef, process, Pid, _Reason} = Down,
            #rt_st{ pids = Pids, mrefs = MRefs } = State) ->
    State1 = State#rt_st{ pids = maps:remove(Pid,Pids),
                          mrefs = maps:remove(MRef,MRefs) },
    tf_cast(Down),
    {noreply, State1};
handle_info(Msg, State) ->
    io:format("Unknown message: ~p~n",[Msg]),
    {noreply, State}.

-record(rt_pid,{messages = queue:new(),
                message_count = 0,
                message_limit = 100,
                on_limit = rotate :: rotate | stop,
                printer = fun default_printer/1}).
-record(rt_trigger,{func,
                    state,
                    message_limit = 100,
                    on_limit = rotate :: rotate | stop }).
-record(rt_trc,{manager,
                printer = fun default_printer/1,
                store = true,
                pids = #{},
                triggers = #{}
              }).

tf_init(Manager, Triggers) ->
    add_triggers(Triggers, #rt_trc{ manager = Manager }).

add_triggers(NewTriggers, #rt_trc{ triggers = Triggers }=St) ->
    Triggers1 = lists:foldl(fun add_trigger/2, Triggers, NewTriggers),
    St#rt_trc{ triggers = Triggers1 }.

add_trigger({Key,{Fun, InitState}}, Triggers) when is_function(Fun) ->
    Triggers#{ Key => #rt_trigger{ func = Fun, state = InitState } };
add_trigger({Fun, InitState}, Triggers) when is_function(Fun) ->
    add_trigger({make_ref(),{Fun,InitState}}, Triggers);
add_trigger(Wut, Triggers) ->
    io:format("Don't understand trigger ~p~n",[Wut]),
    Triggers.

tf_cast(_Message) ->
    ok.

tf({trace,_,call,{?MODULE,tf_cast,[Message]}}, #rt_trc{} = St) ->
    handle_tf_cast(Message, St);
tf(Msg, #rt_trc{}=St) ->
    St1 = store_trace(Msg, St),
    St2 = run_triggers(Msg, St1),
    St2;
tf(A,St) ->
    io:format("UNKNOWN TRACE MESSAGE ~w~n", [A]),
    St.

handle_tf_cast({'DOWN',_,process,Pid,_}, St) ->
    end_trace(Pid, St).

run_triggers(Msg, #rt_trc{ triggers = Triggers } = St) ->
    maps:fold(fun(K, Trig, St0) -> run_trigger(Msg, K, Trig, St0) end, Triggers, St).

run_trigger(Msg, K, #rt_trigger{ func = Fun, state = TrigSt } = Trig, St) ->
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
            start_trace(Msg, Trig, St1);
        print_trace ->   %%% print and clear accumulated messages
            print_trace(Msg, St1),
            flush_trace(Msg, St1);
        flush_trace ->   %%% clear messages without printing
            flush_trace(Msg, St1);
        end_trace   ->   %%% print messages and stop tracing
            print_trace(Msg, St1),
            end_trace(Msg, St1);
        abort_trace ->   %%% stop tracing without printing
            end_trace(Msg, St1);
        none ->
            St1
    end.

start_trace(Msg, Trig, St) ->
    start_trace(Msg, trace_msg_pid(Msg), Trig, St).

start_trace(_Msg, Pid, _Trig, #rt_trc{ pids = Pids } = St) when is_map_key(Pid, Pids) ->
    St;
start_trace(Msg, Pid, #rt_trigger{ message_limit = Lim, on_limit = OnLim },
            #rt_trc{ pids = Pids } = St) ->
    PidTrc = #rt_pid{ message_limit = Lim,
                      on_limit = OnLim },
    store_trace(Msg, Pid, St#rt_trc{ pids = Pids#{ Pid => PidTrc } }).

store_trace(Msg, St) ->
    store_trace(Msg, get_trace(Msg, St), St).

store_trace(_Msg, error, St) ->
    St;
store_trace(Msg, {ok, #rt_pid{ message_count = N,
                               message_limit = Lim,
                               messages = Msgs } = PidTrace},
            St) when N =< Lim ->
    PidTrace1 = PidTrace#rt_pid{ messages = queue:in_r(Msgs, Msg),
                                 message_count = N + 1 },
    set_trace(Msg, PidTrace1, St);
store_trace(Msg, {ok, #rt_pid{ on_limit = stop } }, St) ->
    io:format("TRACE LIMIT HIT FOR ~p~n",[trace_msg_pid(Msg)]),
    end_trace(Msg, St);
store_trace(Msg, {ok, #rt_pid{ messages = Msgs,
                               message_count = N,
                               on_limit = rotate } = PidTrace},
            St) ->
    PidTrace1 = PidTrace#rt_pid{ messages = queue:in_r(Msg, queue:drop_r(Msgs)),
                                 message_count = N - 1 },
    set_trace(Msg, PidTrace1, St).

print_trace(MsgOrPid, #rt_trc{ printer = Printer } = St) ->
    #rt_pid{ messages = Msgs,
             printer = Printer} = get_trace(MsgOrPid, St),
    [ Printer(Msg) || Msg <- queue:from_list(Msgs) ],
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

trace_msg_pid({trace,Pid,call,_MFA}) ->
    Pid;
trace_msg_pid({trace,Pid,return_from,_MFA,_R}) ->
    Pid.

default_printer(Msg) ->
    io:format("~p~n",[Msg]).
