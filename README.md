troll
=====

A set of erlang debugging/tracing tools.

 * troll:  convenience wrappers for dbg, allowing simple tracing of functions/modules.
 * troll_retro: "Retroactive" tracing:
   * Capture calls beginning when a certain condition is met.
   * Print captured trace when another condition obtains.

Build
-----

    $ rebar3 compile

Using
-----

(These examples are out-of-date)

```
1> TF = fun({trace,_,call,{M=lists,F=seq,A=[1,_]}}, _) ->
             start_trace;
           ({trace,_,call,{M=lists,F=seq,A=[2,_]}}, _) ->
             print_trace;
           ({trace,_,call,{M=lists,F=seq,A=[3,_]}}, _) ->
             end_trace;
           ({trace,_,call,{M,F,A}}, _) ->
             end_trace;
           (_,_) ->
             none
        end.
#Fun<erl_eval.43.65746770>

2> troll_retro:start([{lists,seq}],[{some_key,{TF,undefined}}]).
{ok,<0.151.0>}

3> lists:seq(1,2).
[1,2]

4> lists:seq(2,3).
[<0.140.0>] call   lists:seq(2, 3)
[<0.140.0>] return lists:seq(…) -> [1,2]
[<0.140.0>] call   lists:seq(1, 2)
[2,3]

5> lists:seq(3,4).
[<0.140.0>] call   lists:seq(3, 4)
[<0.140.0>] return lists:seq(…) -> [2,3]
[3,4]

6> lists:seq(3,4).
[3,4]

7> lists:seq(2,4).
[2,3,4]
```

TODO:
=====

* More overload protection, checking message queues of troll_retro gen_server and tracer (perhaps a separate monitoring process?)
* Shut down tracer when no active triggers.
  * undo dbg:tpl
* Performance improvements for shell funs:
  * Detect if supplied fun is an erl_eval fun
  * Try fun2ms
  * Try compiling to dynamic module
  * Warn or require a 'force' option to use shell funs that can't be compiled somehow
* Docs docs docs!
* Manually stop debugging
* Message send/receive functionality (tie together trace to sending/receiving process)
